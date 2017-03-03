#include <map>
#include <iostream>
#include <sstream>

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Sema/Sema.h"
#include "clang/Sema/Scope.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/ParentMap.h"
#include "clang/AST/ASTContext.h"


using namespace clang;
using namespace std;

bool isIdenticalExpr(const ASTContext &Ctx, const Expr *Expr1,
                            const Expr *Expr2);


template <typename Derived>
class BasicTranslator: public RecursiveASTVisitor<Derived>
{
public:
	typedef SmallVectorImpl<Stmt *> DataRecursionQueue;

	BasicTranslator(map<Stmt*,Stmt*>* replacements)
		: replacements(replacements) {}

#define TRANSL_TRAVERSE(NAME, namestr, TYPE) \
	bool Traverse##NAME(TYPE* stmt) \
	{ \
		/*llvm::errs() << "Traverse" << namestr << ": ";*/ \
		Stmt* new_stmt = try_replace(stmt); \
		/*ostringstream oss, oss1; \
		oss << hex << stmt; \
		oss1 << hex << new_stmt;\
		llvm::errs() << "stmt=" << oss.str() << \
			" new_stmt=" << oss1.str() << "\n"; \
		if (stmt != new_stmt) \
		{ \
			llvm::errs() << "FOUND REPLACEMENT: FROM\n"; \
			stmt->dump(); \
			llvm::errs() << "\tTO\n"; \
			new_stmt->dump(); \
		} */ \
	return stmt == new_stmt ? \
		RecursiveASTVisitor<Derived>::Traverse##NAME(stmt) : \
		RecursiveASTVisitor<Derived>::TraverseStmt(new_stmt); \
	}

	TRANSL_TRAVERSE(Stmt, "Stmt", Stmt)
/*	TRANSL_TRAVERSE(UnaryPreInc, "UnaryPreInc", UnaryOperator)
	TRANSL_TRAVERSE(UnaryPostInc, "UnaryPostInc", UnaryOperator)
	TRANSL_TRAVERSE(UnaryPreDec, "UnaryPreDec", UnaryOperator)
	TRANSL_TRAVERSE(UnaryPostDec, "UnaryPostDec", UnaryOperator)
	TRANSL_TRAVERSE(CompoundStmt, "CompoundStmt", CompoundStmt)
	TRANSL_TRAVERSE(DeclStmt, "DeclStmt", DeclStmt)*/

	void add_replacement(Stmt* old_stmt, Stmt* new_stmt, string msg)
	{
		//assert(replacements->find(old_stmt) == replacements->end());
		(*replacements)[old_stmt] = new_stmt;
		/*ostringstream oss;
		oss << hex << old_stmt << " -> " << new_stmt;

		llvm::errs() << "ADD REPLACEMENT (" << oss.str() << ", " << msg
			<< ") FROM:\n";

		old_stmt->dump();
		llvm::errs() << "\tTO\n";
		new_stmt->dump();*/
	}

	Stmt* try_replace(Stmt* stmt)
	{
		Stmt* replacement = stmt;

		for (map<Stmt*,Stmt*>::iterator it;
			(it = replacements->find(replacement)) != replacements->end();
			replacement = it->second);

		return replacement;
	}

protected:
	map<Stmt*,Stmt*>* replacements;
};

class IdentExprFinder: public BasicTranslator<IdentExprFinder>
{
public:
	IdentExprFinder(ASTContext* ctx, map<Stmt*,Stmt*>* replacements,
		ParentMap* parent_map, Expr* se)
		: ctx(ctx), BasicTranslator<IdentExprFinder>(replacements),
			parent_map(parent_map), se(se) {}

	set<Stmt*>& getOccurrences() {return occurrences;}
	bool VisitExpr(Expr* expr);
	bool isAddressDependent(Stmt* stmt);
	Stmt* getDirectParent(Stmt* stmt);

protected:
	ASTContext* ctx;
	Expr* se;
	set<Stmt*> occurrences;
	ParentMap* parent_map;
};


class CanonFormTranslator: public BasicTranslator<CanonFormTranslator>
{
public:
	CanonFormTranslator()
		: parent_map(NULL), sema(NULL), ctx(NULL),
		BasicTranslator<CanonFormTranslator>(&replacements), var_cnt(0),
		zero_lit(NULL), one_lit(NULL) {}

	void setSema(Sema* sema) {this->sema = sema;}
	void setASTContext(ASTContext* ctx) {this->ctx = ctx;}
	Stmt* getEnclosingStmt(Stmt*& parent, Stmt* stmt);
	bool VisitUnaryOperator(UnaryOperator* uop);
	bool VisitForStmt(ForStmt* stmt);
	bool VisitDoStmt(DoStmt* stmt);
//	bool VisitLabelStmt(LabelStmt* stmt);
	bool VisitBinaryOperator(BinaryOperator* bop);
	StmtResult RewriteConditionalAssignment(Expr* lhs, ConditionalOperator* rhs);

	bool VisitCompoundAssignOperator(CompoundAssignOperator* op);
	bool VisitDeclStmt(DeclStmt* stmt);
	bool VisitImplicitCastExpr(ImplicitCastExpr* expr);
	bool TraverseFunctionDecl(FunctionDecl* decl);
	map<Stmt*,Stmt*>& getReplacements() {return replacements;}
	Stmt* RewriteVarDecl(VarDecl* vd);

protected:
	VarDecl* create_temp_var(QualType type, SourceLocation loc, Expr* init);
	void add_replacement(Stmt* old_stmt, Stmt* new_stmt, string msg);

	Sema* sema;
	ASTContext* ctx;
	ParentMap* parent_map;
	map<Stmt*,Stmt*> replacements;
	int var_cnt;
	IntegerLiteral* one_lit, *zero_lit;
};
