#include <map>
#include <iostream>
#include <sstream>

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Scope.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/ParentMap.h"
#include "clang/AST/ASTContext.h"

#include "rewriter_stmt.h"

using namespace clang;
using namespace std;

bool isIdenticalExpr(const ASTContext &Ctx, const Expr *Expr1,
                            const Expr *Expr2);



bool IdentExprFinder::VisitExpr(Expr* expr)
{
	Stmt* dir_parent = 0;

	if (expr == expr->IgnoreParenCasts()
		&& isIdenticalExpr(*ctx, expr, se)
		&& (dir_parent = getDirectParent(expr))
		&& !isAddressDependent(dir_parent))
	{
		occurrences.insert(expr);
	}
	return true;
}

Stmt* IdentExprFinder::getDirectParent(Stmt* stmt)
{
	Stmt* parent = stmt;
	Expr* expr;

	for (; (parent=parent_map->getParent(parent)) &&
		(!(expr=dyn_cast<Expr>(parent)) || expr->IgnoreParenCasts() == stmt););
	return parent;
}

bool IdentExprFinder::isAddressDependent(Stmt* stmt)
{
	UnaryOperator* uop = dyn_cast<UnaryOperator>(stmt);
	return uop && uop->getOpcode() == UO_AddrOf;
}

bool CanonFormTranslator::TraverseFunctionDecl(FunctionDecl* decl)
{
	bool ret = true;
	if (decl->hasBody())
	{
		if (parent_map) delete parent_map;
		parent_map = new ParentMap(decl->getBody());
		ret = TraverseStmt(decl->getBody());
	}
	return ret;
}

Stmt* CanonFormTranslator::getEnclosingStmt(Stmt*& parent, Stmt* stmt)
{
	//assert(parent_map);
	parent = parent_map->getParent(stmt);
	Stmt *prev = stmt;
	for (; parent && !isa<CompoundStmt>(parent);	
		prev = parent, parent = parent_map->getParent(parent));

	assert(parent != NULL && prev != NULL);
	Expr* prev_expr = dyn_cast<Expr>(prev);
	return prev_expr ? prev_expr->IgnoreParenCasts() : prev;
}


bool CanonFormTranslator::VisitDeclStmt(DeclStmt* stmt)
{
//	stmt->dump();

	//split declarations with init to decl w/o init + assignment stmt
	vector<Stmt*> stmts;
	SourceLocation loc = stmt->getLocStart();

	for (DeclStmt::decl_iterator it = stmt->decl_begin(),
		end = stmt->decl_end(); it != end; it++)
	{
		VarDecl* vd;
		Stmt* repl;
		if ((vd = dyn_cast<VarDecl>(*it)) &&
			(repl = RewriteVarDecl(vd)))
		{
			stmts.push_back(repl);
//			printf("replace conditional assigment in decl stmt:\n");
//			repl->dump();
		}
	}

	if (stmts.size() > 0)
	{
		StmtResult comp_stmt = sema->ActOnCompoundStmt(loc, loc, stmts, false); //isStmtExpr=
		// we can replace the decl stmt with a sequence of assigment stmts
		//	because we don't use decl stmts in our analysis
		add_replacement(stmt, comp_stmt.get(), "decl with init");
	}

	return true;
}

//bool CanonFormTranslator::VisitVarDecl(VarDecl* vd)
//{}

Stmt* CanonFormTranslator::RewriteVarDecl(VarDecl* vd)
{
	Stmt* repl = NULL;
	if (vd->hasInit())
	{
		// create an assign stmt
		Expr* init = vd->getInit()->IgnoreParenCasts();
//		vd->setInit(NULL);
		SourceLocation loc = vd->getLocStart();
		ExprResult vr = sema->BuildDeclRefExpr(vd, vd->getType(), VK_LValue, loc);

		ConditionalOperator* cond = dyn_cast<ConditionalOperator>(init);
		repl = cond ? RewriteConditionalAssignment(vr.get(), cond).get() :
			(Stmt*)sema->CreateBuiltinBinOp(loc, BO_Assign, vr.get(), init).get();

//		printf("replace conditional assigment in decl stmt:\n");
//		repl->dump();
	}
	return repl;
}

bool CanonFormTranslator::VisitImplicitCastExpr(ImplicitCastExpr* expr)
{
//	printf("Implicit cast, kind=%d\n", CK_IntegralToBoolean);
	if (expr->getCastKind() == CK_LValueToRValue)
	{
		Stmt* parent = parent_map->getParent(expr);
		IfStmt* if_stmt;
		BinaryOperator* bop;
		UnaryOperator* uop=0;
		ForStmt* for_stmt;
		WhileStmt* while_stmt;
		DoStmt* do_stmt;
		ConditionalOperator* cond_op;
	
		if (!parent) 
		{
//			llvm::errs() << "Could not find parent for expr:\n";
//			expr->dump(); 
		}
		else if (((if_stmt = dyn_cast<IfStmt>(parent)) && if_stmt->getCond() == expr) ||
			((bop = dyn_cast<BinaryOperator>(parent)) && bop->isLogicalOp()) ||
			((uop = dyn_cast<UnaryOperator>(parent)) && uop->getOpcode() == UO_LNot) ||
			((for_stmt = dyn_cast<ForStmt>(parent)) && for_stmt->getCond() == expr) ||
			((while_stmt = dyn_cast<WhileStmt>(parent)) && while_stmt->getCond() == expr) ||
			((do_stmt = dyn_cast<DoStmt>(parent)) && do_stmt->getCond() == expr) ||
			((cond_op = dyn_cast<ConditionalOperator>(parent)) && cond_op->getCond() == expr))
		{
//			printf("cast kind integral to boolean\n");
//			parent->dump();
			SourceLocation loc = expr->getLocStart();
			if (!zero_lit)
			{
				llvm::APInt zero_val(ctx->getIntWidth(ctx->IntTy), 0, true); //isSigned=
				zero_lit = IntegerLiteral::Create(*ctx, zero_val, ctx->IntTy, loc);
			}
			ExprResult comp_expr = sema->CreateBuiltinBinOp(
				loc, /*uop ? BO_EQ : BO_NE,*/ BO_NE, expr->getSubExpr(), zero_lit);

			add_replacement(/*uop ? (Stmt*)uop : (Stmt*)expr*/expr,
				comp_expr.get(),"implicit cast to bool");
//			comp_expr.get()->dump();
		}
	}
	return true;
}

bool CanonFormTranslator::VisitBinaryOperator(BinaryOperator* bop)
{
//	bop->dump();
	ConditionalOperator *rhs;
	if (bop->getOpcode() == BO_Assign &&
		(rhs = dyn_cast<ConditionalOperator>(bop->getRHS()->IgnoreParenCasts())))
	{
		StmtResult if_stmt = RewriteConditionalAssignment(bop->getLHS(), rhs);
		add_replacement(bop, if_stmt.get(), "assign with cond op");
	}

	return true;
}

StmtResult CanonFormTranslator::RewriteConditionalAssignment(
	Expr* lhs, ConditionalOperator* rhs)
{
//	printf("rewrite cond. op:\n");
	SourceLocation loc = rhs->getLocStart();
	Sema::FullExprArg cond = sema->MakeFullExpr(rhs->getCond());

	ExprResult asg_stmt1 = sema->CreateBuiltinBinOp(loc, BO_Assign,
		lhs, rhs->getTrueExpr());

	ExprResult asg_stmt2 = sema->CreateBuiltinBinOp(loc, BO_Assign,
		lhs, rhs->getFalseExpr());
	
	StmtResult if_stmt = sema->ActOnIfStmt(loc, cond, NULL,
		asg_stmt1.get(), loc, asg_stmt2.get());

//	if_stmt.get()->dump();
	return if_stmt;
}


bool CanonFormTranslator::VisitForStmt(ForStmt* for_stmt)
{
	Sema::FullExprArg cond = sema->MakeFullExpr(for_stmt->getCond());

	vector<Stmt*> new_body_stmts;
	if (for_stmt->getBody())
	{
		//Stmt::child_iterator it = for_stmt->getBody()->child_begin(),
		//	end = for_stmt->getBody()->child_end();

		//for (; it != end; new_body_stmts.push_back(*it), it++);
		new_body_stmts.push_back(for_stmt->getBody());
	}

	if (for_stmt->getInc())
	{
		new_body_stmts.push_back(for_stmt->getInc());
	}

	SourceLocation loc = for_stmt->getLocStart();
	StmtResult new_body = sema->ActOnCompoundStmt(
		loc, loc, new_body_stmts, false); //isStmtExpr=

	StmtResult while_stmt = sema->ActOnWhileStmt(loc,
		cond, NULL, new_body.get());

	Stmt* repl;
	if (for_stmt->getInit())
	{
		vector<Stmt*> wrapper_stmts;
		wrapper_stmts.push_back(for_stmt->getInit());
		wrapper_stmts.push_back(while_stmt.get());

		StmtResult comp_stmt = sema->ActOnCompoundStmt(
			loc, loc, wrapper_stmts, false);

		repl = comp_stmt.get();
	}
	else
	{
		repl = while_stmt.get();
	}

	add_replacement(for_stmt, repl, "for stmt");
	return true;
}

bool CanonFormTranslator::VisitDoStmt(DoStmt* do_stmt)
{
	Sema::FullExprArg cond = sema->MakeFullExpr(do_stmt->getCond());
	vector<Stmt*> wrapper_stmts;

	if (do_stmt->getBody())
	{
		wrapper_stmts.push_back(do_stmt->getBody());
	}

	SourceLocation loc = do_stmt->getLocStart();
	StmtResult while_stmt = sema->ActOnWhileStmt(
		loc, cond, NULL, do_stmt->getBody());

	wrapper_stmts.push_back(while_stmt.get());
	StmtResult comp_stmt = sema->ActOnCompoundStmt(
		loc, loc, wrapper_stmts, false);

	add_replacement(do_stmt, comp_stmt.get(), "do stmt");
	return true;
}

/*bool CanonFormTranslator::VisitLabelStmt(LabelStmt* label_stmt)
{
	add_replacement(label_stmt, label_stmt->getSubStmt(), "label stmt");
	return true;
}*/

bool CanonFormTranslator::VisitUnaryOperator(UnaryOperator* uop)
{
	if (!uop->isIncrementDecrementOp()) return true;
	// create a decl stmt for a temporary variable to store the old value: temp_vd = se
	Expr* se = uop->getSubExpr()->IgnoreParenCasts();
	QualType type = se->getType();
	SourceLocation loc = uop->getLocStart();
	VarDecl* temp_vd = create_temp_var(type, loc, NULL);
	ExprResult temp_vr = sema->BuildDeclRefExpr(temp_vd, type, VK_LValue, loc);

	Sema::DeclGroupPtrTy decl_grp = Sema::DeclGroupPtrTy::make(DeclGroupRef(temp_vd));
	StmtResult decl_stmt = sema->ActOnDeclStmt(decl_grp, loc, loc);

	ExprResult asg_stmt1 = sema->CreateBuiltinBinOp(loc, BO_Assign, temp_vr.get(), se);

	// create an assigment to replace the increment statement : se = se +/- 1
	if (!one_lit)
	{
		llvm::APInt one_val(ctx->getIntWidth(ctx->IntTy), 1, true); //isSigned=
		one_lit = IntegerLiteral::Create(*ctx, one_val, ctx->IntTy, loc);
	}
	ExprResult rhs_expr = sema->CreateBuiltinBinOp(loc,
		uop->isIncrementOp() ? BO_Add : BO_Sub, se,	one_lit);

	ExprResult asg_stmt2 = sema->CreateBuiltinBinOp(loc, BO_Assign, se, rhs_expr.get());

	// create a compound stmt to replace the enclosing comp_stmt
	//	insert decl_stmt and asg_stmt before parent
	Stmt* enclosing_comp_stmt;
	Stmt* parent = getEnclosingStmt(enclosing_comp_stmt, uop);
	bool primitive_incdec = (parent == uop);
	vector<Stmt*> stmts;
	
	Stmt::child_iterator it = enclosing_comp_stmt->child_begin(),
		end = enclosing_comp_stmt->child_end();

	for (; it != end && try_replace(*it) != parent; stmts.push_back(*it), it++);
	//stmts.push_back(decl_stmt.get());
	stmts.push_back(asg_stmt1.get());
	stmts.push_back(asg_stmt2.get());
	if (!primitive_incdec) stmts.push_back(parent);
	if (it != end) it++;
	for (; it != end; stmts.push_back(*it), it++);

	StmtResult comp_stmt = sema->ActOnCompoundStmt(loc, loc, stmts, false); //isStmtExpr=
	// add a replacement for the enclosing compound stmt
	add_replacement(enclosing_comp_stmt, comp_stmt.get(), "insert separate inc/dec");

	if (!primitive_incdec)
	{
		// add a replacement for se in the uop
		add_replacement(uop, uop->isPrefix() ? se : temp_vr.get(), "replace inc/dec with temp var");

		// add a replacecement to tmp_vr for se and equivalent expressions
		//	which occur before the uop in the parent stmt
		IdentExprFinder ident_expr_finder(ctx, &replacements, parent_map, se);
		BinaryOperator* bop = dyn_cast<BinaryOperator>(parent);

		/*fprintf(stderr, "bop=%p, opcode=%d, is_assign=%d\n", bop,
			bop?bop->getOpcode():0, bop?bop->isAssignmentOp():0);*/

		ident_expr_finder.TraverseStmt(bop && bop->isAssignmentOp() ? bop->getRHS() : parent);
		set<Stmt*> occur = ident_expr_finder.getOccurrences();

		for (set<Stmt*>::iterator it = occur.begin(), end = occur.end();
			it != end && *it != se; add_replacement(*it, temp_vr.get(),
				"replace occurences of inc/dec subexpr before inc/dec"), it++);
	}

	return true;
}

bool CanonFormTranslator::VisitCompoundAssignOperator(CompoundAssignOperator* op)
{
	SourceLocation loc = op->getLocStart();

	ExprResult rhs_expr = sema->CreateBuiltinBinOp(loc,
		BinaryOperator::getOpForCompoundAssignment(op->getOpcode()), op->getLHS(), op->getRHS());

	Expr* lhs_expr = op->getLHS()->IgnoreParenCasts();
	if (DeclRefExpr* re = dyn_cast<DeclRefExpr>(lhs_expr))
	{
		ValueDecl* lhs_vd = re->getDecl();
		lhs_expr = sema->BuildDeclRefExpr(lhs_vd, lhs_vd->getType(), VK_LValue, loc).get();
	}

	ExprResult assign_stmt = sema->CreateBuiltinBinOp(loc,
		BO_Assign, lhs_expr, rhs_expr.get());

	add_replacement(op, assign_stmt.get(), "replace compound assign");
	return true;
}

VarDecl* CanonFormTranslator::create_temp_var(QualType type, SourceLocation loc, Expr* init=NULL)
{
	DeclContext* TUDC = TranslationUnitDecl::castToDeclContext(
		ctx->getTranslationUnitDecl());//sema->TUScope->getEntity());
	//SourceLocation Loc;

	ostringstream oss;
	oss << "_datalog_tmp_" << var_cnt++;
	VarDecl* vd = VarDecl::Create(*ctx, TUDC, loc, loc,
		sema->PP.getIdentifierInfo(oss.str()), type, (TypeSourceInfo*)0,
		SC_None);

	if (init) vd->setInit(init);
	return vd;
}

void CanonFormTranslator::add_replacement(Stmt* old_stmt, Stmt* new_stmt, string msg)
{
	BasicTranslator<CanonFormTranslator>::add_replacement(old_stmt, new_stmt, msg);
	parent_map->addStmt(new_stmt);
	parent_map->setParent(new_stmt, parent_map->getParent(old_stmt));
}
