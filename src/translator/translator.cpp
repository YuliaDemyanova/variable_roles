#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/ASTContext.h"

#include <string>		// std::string
#include <map>
#include <iostream>		// std::cout
#include <sstream>		// std::ostringstream

#include "rewriter_stmt.h"

#define DUMP_STMT(stmt, ctx) stmt->dumpPretty(ctx); 
 
#define DEBUG_STMT(stmt, stream, ctx) \
	{ \
	(stmt)->printPretty(stream, 0, PrintingPolicy((ctx).getLangOpts())); \
	} 
 
#define DBG_INFO __FILE__ << " " << __FUNCTION__ << "(), " << __LINE__ << " " 
 
#define DEBUG_STMT_VERB(stmt, ostream, ctx)\
{\
	ostream << DBG_INFO << " Statement: (" \
			<< (stmt)->getStmtClassName() << ", " \
			<< hex << stmt << dec << ") ";\
	string s; raw_string_ostream ss(s);\
	DEBUG_STMT(stmt, ss, ctx);\
	ostream << ss.str() << "\n";\
}

using namespace clang;
using namespace clang::tooling;
using namespace std;
using namespace llvm;


struct rel3_struct {const char* rel, *child1, *child2, *child3;};
typedef enum {IF_STMT=0, BOP, ID} rel3_enum;
map<rel3_enum, rel3_struct> rels3;

struct rel2_struct {const char* rel, *child1, *child2;};
typedef enum {ASSIGN=0, SEQ, ARRAY_SUBSCRIPT, UOP, WHILE_STMT,
	ICONST, FCONST, CHAR_CONST, STRING_CONST, RETURN_STMT, LABEL_STMT} rel2_enum;
map<rel2_enum, rel2_struct> rels2;

struct rel1_struct {const char* rel, *child1;};
typedef enum {FUNC_DECL=0, FUNC_CALL, GOTO_STMT} rel1_enum;

const char* Text = "text";
map<rel1_enum, rel1_struct> rels1;

struct rel0_struct {const char* rel;};
typedef enum {EMPTY=0, SIZEOF, BREAK_STMT, GLOBAL_VAR} rel0_enum;
map<rel0_enum, rel0_struct> rels0;

struct reli_struct {const char* rel;};
typedef enum {PARAM=0} reli_enum;
map<reli_enum, reli_struct> relsi;

void init()
{
	rels3[IF_STMT] = {"if_stmt","condition","then_stmt","else_stmt"};
	rels3[BOP] = {"bop_expr","lhs_expr","rhs_expr", "opcode"};
	rels3[ID] = {"identifier", "type", "function", Text}; 

	rels2[ASSIGN] = {"assignment_stmt","lhs_expr","rhs_expr"};
	rels2[SEQ] = {"sequence_stmt","sub_stmt1","sub_stmt2"};
	rels2[ARRAY_SUBSCRIPT] = {"array_expr","arrayptr_expr","arrayind_expr"};
	rels2[UOP] = {"uop_expr","sub_expr", "opcode"};
	rels2[WHILE_STMT] = {"while", "condition", "body"};
	rels2[ICONST] = rels2[FCONST] =
		rels2[CHAR_CONST] = rels2[STRING_CONST] =
		{"constant", "type", Text}; /*{"int_const", Text};*/
//	{"const", "type", Text}; /*{"float_const", Text};*/
//	{"const", "type", Text}; /*{"char_const", Text};*/
	/*{"string_const", Text};*/
//		{"const", "type", Text};
	rels2[LABEL_STMT] = {"label_stmt", "sub_stmt", Text};

	rels1[FUNC_DECL] = {"func_decl", "body"};
	rels2[RETURN_STMT] = {"return_stmt", "function", "sub_expr"};
	rels1[FUNC_CALL] = {"call_expr","function"};
	rels1[GOTO_STMT] = {"goto_stmt", "label"};

	rels0[EMPTY] = {"empty_stmt"};
	rels0[SIZEOF] = {"sizeof_expr"};
	rels0[BREAK_STMT] = {"break_stmt"};

	relsi[PARAM] = {"param"};
}

void printnode(const char* name, int id) 
{
	printf("%s(%d).\n", name, id);
}

void print_rel(const char* rel_name, int id)
{
	printf("%s(%d).\n", rel_name, id);
}

void print_rel(const char* rel_name, int id1, int id2)
{
	printf("%s(%d,%d).\n", rel_name, id1, id2);
}

void print_rel(const char* rel_name, int id1, int id2, int id3)
{
	printf("%s(%d,%d,%d).\n", rel_name, id1, id2, id3);
}

void print_rel(const char* rel_name, int id1, const char* id2, bool quotes=true, bool extra_quotes=false)
{
	ostringstream oss;
	if (extra_quotes) oss << "\'" << id2 << "\'";

	else oss << id2;

	if (quotes)
	{
		printf("%s(%d,\"%s\").\n", rel_name, id1, oss.str().c_str());
	}
	else
	{
		printf("%s(%d,%s).\n", rel_name, id1, oss.str().c_str());
	}
}

void print_rel3(rel3_enum rel, int rel_id, int child1_id, int child2_id, int child3_id)
{
	assert(rels3.find(rel) != rels3.end());
	print_rel(rels3[rel].rel, rel_id);
	print_rel(rels3[rel].child1, rel_id, child1_id);
	print_rel(rels3[rel].child2, rel_id, child2_id);
/*	printf("%s(%d).\n", rels3[rel].rel, rel_id);
	printf("%s(%d,%d).\n", rels3[rel].child1, rel_id, child1_id);
	printf("%s(%d,%d).\n", rels3[rel].child2, rel_id, child2_id);*/

	if (child3_id > 0)
	{
		print_rel(rels3[rel].child3, rel_id, child3_id);
		//printf("%s(%d,%d).\n", rels3[rel].child3, rel_id, child3_id);
	}
}

void print_rel3(rel3_enum rel, int rel_id, int child1_id, int child2_id, const char* child3_str)
{
	assert(rels3.find(rel) != rels3.end());
	print_rel(rels3[rel].rel, rel_id);
	print_rel(rels3[rel].child1, rel_id, child1_id);
	print_rel(rels3[rel].child2, rel_id, child2_id);
	print_rel(rels3[rel].child3, rel_id, child3_str);

/*	printf("%s(%d).\n", rels3[rel].rel, rel_id);
	printf("%s(%d,%d).\n", rels3[rel].child1, rel_id, child1_id);
	printf("%s(%d,%d).\n", rels3[rel].child2, rel_id, child2_id);
	printf("%s(%d,\"%s\").\n", rels3[rel].child3, rel_id, child3_str);*/
}

void print_rel3(rel3_enum rel, int rel_id, const char* child1_str,
	int child2_id, const char* child3_str, bool child1_quotes=true)
{
	assert(rels3.find(rel) != rels3.end());
	print_rel(rels3[rel].rel, rel_id);
	print_rel(rels3[rel].child1, rel_id, child1_str, child1_quotes);
	if (child2_id >= 0) print_rel(rels3[rel].child2, rel_id, child2_id);
	print_rel(rels3[rel].child3, rel_id, child3_str);
}

void print_rel2(rel2_enum rel, int rel_id, int child1_id, int child2_id)
{
	assert(rels2.find(rel) != rels2.end());
	print_rel(rels2[rel].rel, rel_id);
	print_rel(rels2[rel].child1, rel_id, child1_id);
	print_rel(rels2[rel].child2, rel_id, child2_id);

/*	printf("%s(%d).\n", rels2[rel].rel, rel_id);
	printf("%s(%d,%d).\n", rels2[rel].child1, rel_id, child1_id);
	printf("%s(%d,%d).\n", rels2[rel].child2, rel_id, child2_id);*/
}

void print_rel2(rel2_enum rel, int rel_id, int child1_id, const char* child2_str)
{
	assert(rels2.find(rel) != rels2.end());
	print_rel(rels2[rel].rel, rel_id);
	print_rel(rels2[rel].child1, rel_id, child1_id);
	print_rel(rels2[rel].child2, rel_id, child2_str);

/*	printf("%s(%d).\n", rels2[rel].rel, rel_id);
	printf("%s(%d,%d).\n", rels2[rel].child1, rel_id, child1_id);
	printf("%s(%d,\"%s\").\n", rels2[rel].child2, rel_id, child2_str);*/
}

void print_rel2(rel2_enum rel, int rel_id, const char* child1_str,
	const char* child2_str, bool child1_quotes=true, bool child2_extra_quotes=false)
{
	assert(rels2.find(rel) != rels2.end());
	print_rel(rels2[rel].rel, rel_id);
	print_rel(rels2[rel].child1, rel_id, child1_str, child1_quotes);
	print_rel(rels2[rel].child2, rel_id, child2_str, true, child2_extra_quotes);

/*	printf("%s(%d).\n", rels2[rel].rel, rel_id);
	if (quotes)
	{
		printf("%s(%d,\"%s\").\n", rels2[rel].child1, rel_id, child1_str);
	}
	else
	{
		printf("%s(%d,%s).\n", rels2[rel].child1, rel_id, child1_str);
	}

	printf("%s(%d,\"%s\").\n", rels2[rel].child2, rel_id, child2_str);*/
}

void print_rel1(rel1_enum rel, int rel_id, int child1_id)
{
	assert(rels1.find(rel) != rels1.end());
	print_rel(rels1[rel].rel, rel_id);
	if (child1_id > 0) print_rel(rels1[rel].child1, rel_id, child1_id);

/*	printf("%s(%d).\n", rels1[rel].rel, rel_id);

	if (child1_id > 0)
	{
		printf("%s(%d,%d).\n", rels1[rel].child1, rel_id, child1_id);
	}*/
}

void print_rel1(rel1_enum rel, int rel_id, const char* child1_str)
{
	assert(rels1.find(rel) != rels1.end());
	print_rel(rels1[rel].rel, rel_id);
	print_rel(rels1[rel].child1, rel_id, child1_str);

/*	printf("%s(%d).\n", rels1[rel].rel, rel_id);
	printf("%s(%d,\"%s\").\n", rels1[rel].child1, rel_id, child1_str);*/
}

void print_reli(reli_enum rel, int rel_id, int i, int child_i)
{
	assert(relsi.find(rel) != relsi.end());
	print_rel(relsi[rel].rel, rel_id, i, child_i);
	
//	printf("%s(%d,%d,%d).\n", relsi[rel].rel, rel_id, i, child_i);
}

void print_rel0(rel0_enum rel, int rel_id)
{
	assert(rels0.find(rel) != rels0.end());
	print_rel(rels0[rel].rel, rel_id);
//	printf("%s(%d).\n", rels0[rel].rel, rel_id);
}

int getid()
{
	static int max_id=0;
	return ++max_id;
}

class DatalogTranslVisitor: public BasicTranslator<DatalogTranslVisitor>
{
public:
	DatalogTranslVisitor(CanonFormTranslator* translator, ASTContext *Context)
		: Context(Context), translator(translator), current_func(NULL), scope(0),
			BasicTranslator<DatalogTranslVisitor>(&translator->getReplacements()) {}

	bool getname(string& name, NamedDecl* Declaration)
	{
		ostringstream oss;
		int line = 0, col = 0;
		bool res = false;

		FullSourceLoc FullLocation = Context->getFullLoc(Declaration->getLocation());
		if (FullLocation.isValid())
		{
			line = FullLocation.getSpellingLineNumber();
			col = FullLocation.getSpellingColumnNumber();
		}
	
		NamedDecl* last_decl = Declaration->getMostRecentDecl();
		if (!last_decl)
		{
			fprintf(stderr, "%s: getMostRecentDecl returned NULL\n", __func__);
		}
		else
		{
			if (dyn_cast<FunctionDecl>(Declaration))
			{
				oss << Declaration->getNameAsString().c_str();
			}
			else
			{
				oss << Declaration->getQualifiedNameAsString().c_str() << "_" << line << "_" << col;
			}
			name = oss.str();
			res = true;
		}
		return res;
	}

	QualType get_type(ValueDecl* vd)
	{
		QualType type;
		if (FunctionDecl* fd = dyn_cast<FunctionDecl>(vd))
		{
			type = fd->getResultType();
		}
		else
		{
			type = vd->getType();
		}
		return type;
	}

	int get_or_add_node_id(ValueDecl* decl, bool is_function=false)
	{
		ValueDecl* last_decl = cast<ValueDecl>(decl->getMostRecentDecl());
		bool added;
		int id = get_or_add_node((Decl*)last_decl, &added);
		if (added)
		{
			string name;
			if (getname(name, last_decl))
			{
				print_rel3(ID, id, type_to_str(get_type(decl)),
					is_function ? -1 : (last_decl->isDefinedOutsideFunctionOrMethod() ? 0 : scope),
					name.c_str(), /*child1_quotes=*/false);
				//fprintf(stderr, "added node for var decl %s\n", name.c_str());
			}
/*			if (!last_decl->getParentFunctionOrMethod() && !dyn_cast<FunctionDecl>(last_decl))
			{
				print_rel0(GLOBAL_VAR, id);
			}*/
		}
		return id;
	}

	int get_or_add_node(FunctionDecl* func_decl)
	{
		return get_or_add_node_id(func_decl, /*is_function=*/ true);
	}

	int get_or_add_node(DeclRefExpr* expr)
	{
		return get_or_add_node_id(expr->getDecl());
	}

	const char* type_to_str(QualType type)
	{
//		string name = QualType(type->getCanonicalTypeInternal().getTypePtr(), 0).getAsString();

		// replace spaces with underscores
//		for (int pos=0; (pos = name.find(' ', pos)) != string::npos;
//			name.replace(pos, 1, 1, '_'));

		const char* tname;
		if (type->isIntegerType()) tname = "int";
		else if (type->isFloatingType()) tname = "float";
		else if (type->isPointerType() || type->isArrayType()) tname = "ptr";
		else if (type->isStructureType()) tname = "struct";
		else if (type->isVoidType()) tname = "void";
		else tname = "unknown";

		return tname;
	}

	int get_or_add_node(MemberExpr* expr)
	{
		ValueDecl* md = cast<ValueDecl>(expr->getMemberDecl()->getMostRecentDecl());
		bool added;
		int id = get_or_add_node((Decl*)md, &added);
		if (added)
		{
			string name;
			if (getname(name, md))
			{
				print_rel3(ID, id, type_to_str(get_type(md)), 0,
					name.c_str(), /*child1_quotes=*/false);
				//fprintf(stderr, "added node for member decl %s\n", name.c_str());
			}
		}
		return id;
	}

	int get_or_add_node(IntegerLiteral* lit)
	{
		bool added;
		int id = get_or_add_stmt_node((Stmt*)lit, &added);
		if (added)
		{
/*			QualType type = lit->getType();
			fprintf(stderr, "Int literal type: %s %s\n",
				QualType::getAsString(type.split()).c_str(), type_to_str(type));*/
			print_rel2(ICONST, id, type_to_str(lit->getType()), lit->getValue().toString(10, true).c_str(),/*quotes=*/false);
		}
		return id;
	}

	int get_or_add_node(FloatingLiteral* lit)
	{
		bool added;
		int id = get_or_add_stmt_node((Stmt*)lit, &added);
		if (added)
		{
			ostringstream oss;
			oss << lit->getValueAsApproximateDouble();
			print_rel2(FCONST, id, type_to_str(lit->getType()), oss.str().c_str(),/*quotes=*/false);
		}
		return id;
	}

	int get_or_add_node(CharacterLiteral* lit)
	{
		bool added;
		int id = get_or_add_stmt_node((Stmt*)lit, &added);
		if (added)
		{
			ostringstream oss;
			oss << (char)lit->getValue();
			print_rel2(CHAR_CONST, id, type_to_str(lit->getType()), oss.str().c_str(),/*quotes=*/false, true);
		}
		return id;
	}

	int get_or_add_node(StringLiteral* lit)
	{
		bool added;
		int id = get_or_add_stmt_node((Stmt*)lit, &added);
		if (added)
		{
			print_rel2(STRING_CONST, id, type_to_str(lit->getType()), lit->getBytes().data(),/*quotes=*/false);
		}
		return id;
	}

	int get_or_add_node(Stmt* stmt, bool* added=NULL)
	{
		int id;

		Expr* expr = dyn_cast<Expr>(stmt), *nf_expr = 0;
		if (expr) nf_expr = expr->IgnoreParenCasts();

		if (expr != nf_expr) id = get_or_add_node(try_replace(nf_expr));
		else if (DeclRefExpr* dre = dyn_cast<DeclRefExpr>(stmt)) id = get_or_add_node(dre);
		else if (IntegerLiteral* il = dyn_cast<IntegerLiteral>(stmt)) id = get_or_add_node(il);
		else if (FloatingLiteral* fl = dyn_cast<FloatingLiteral>(stmt)) id = get_or_add_node(fl);
		else if (CharacterLiteral* cl = dyn_cast<CharacterLiteral>(stmt)) id = get_or_add_node(cl);
		else if (MemberExpr* me = dyn_cast<MemberExpr>(stmt)) id = get_or_add_node(me);
//		else if (LabelStmt* ls = dyn_cast<LabelStmt>(stmt)) id = get_or_add_node(ls);
		else id = get_or_add_stmt_node(stmt, added);

		return id;
	}

	int get_or_add_stmt_node(Stmt* stmt, bool* added=NULL)
	{
		int id;
		map<Stmt*,int>::iterator it = stmt_node_ids.find(stmt);
		if (it == stmt_node_ids.end())
		{
			id = add_node(stmt);
			if (added) *added = true;
		}
		else
		{
			id = it->second;
			if (added) *added = false;
		}
		return id;
	}

	int get_or_add_node(EnumConstantDecl* decl)
	{
		bool added;
		int id = get_or_add_node((Decl*)decl, &added);
		if (added)
		{
			ostringstream oss;
			print_rel2(ICONST, id, type_to_str(decl->getType()), decl->getInitVal().toString(10, true).c_str(),/*quotes=*/false);
		}
		return id;
	}

	int get_or_add_node(Decl* decl, bool* added=NULL)
	{
		map<Decl*,int>::iterator it = decl_node_ids.find(decl);
		int id;
		if (it == decl_node_ids.end())
		{
			id = add_node(decl);
			if (added) *added = true;
		}
		else
		{
			id = it->second;
			if (added) *added = false;
		}
		return id;
	}

/*	int get_or_add_node(LabelStmt* ls)
	{
		return ls->getSubStmt() ?
			get_or_add_node(try_replace(ls->getSubStmt())) :
			-1;
	}*/

	int add_node(Stmt* stmt)
	{
		int id = 0;
		map<Stmt*,int>::iterator it = stmt_node_ids.find(stmt);

		if (it == stmt_node_ids.end())
		{
			id = getid();
			stmt_node_ids[stmt] = id;

			/*Stmt* replace_stmt = try_replace(stmt);
			if (replace_stmt != stmt)
			{
				stmt_node_ids[replace_stmt] = id;
			}*/
		}
		else 
		{
			id = it->second;
			fprintf(stderr, "stmt node already added (%d)\n", id);
		}

		return id;
	}

	int add_node(Decl* decl)
	{
		int id = 0;
		map<Decl*,int>::iterator it = decl_node_ids.find(decl);

		if (it == decl_node_ids.end())
		{
			id = getid();
			decl_node_ids[decl] = id;
		}
		else 
		{
			id = it->second;
			fprintf(stderr, "decl node already added (%d)\n", id);
		}

		return id;
	}

	void Finish()
	{
		for (list<Stmt*>::iterator it = global_stmts.begin(),
			end = global_stmts.end(); it != end;
			TraverseStmt(*it), it++);
	}

	bool VisitVarDecl(VarDecl* vd)
	{
//		printf("VisitVarDecl: %s\n", vd->getNameAsString().c_str());
		Stmt* stmt;
		if (vd && !(vd->isLocalVarDecl() || dyn_cast<ParmVarDecl>(vd)))
		{
//			printf("with initialiser\n");
			Stmt* stmt = translator->RewriteVarDecl(vd);
			if (stmt)
			{
				global_stmts.push_back(stmt);
			}
		}

		return true;
	}

	bool TraverseFunctionDecl(FunctionDecl* decl)
	{
/*		string s = decl->getNameAsString();
//		fprintf(stderr, "\n\nConvert %s to normal form\n\n", s.c_str());
		printf("\n\nBEFORE:\n");
		if (decl->getBody()) decl->getBody()->dump();*/
		translator->TraverseFunctionDecl(decl);

/*		printf("\n\nAFTER:\n");
		if (decl->getBody()) decl->getBody()->dump();*/
//		fprintf(stderr, "\n\nPrint %s to datalog\n\n", s.c_str());
		RecursiveASTVisitor<DatalogTranslVisitor>::TraverseFunctionDecl(decl);
		return true;
	}

	bool VisitFunctionDecl(FunctionDecl* func_decl)
	{
		current_func = func_decl;
		int func_decl_id = get_or_add_node(func_decl);
		scope = func_decl_id;

		print_rel1(FUNC_DECL,func_decl_id,
			func_decl->getBody() ?
			get_or_add_node(try_replace(func_decl->getBody())) :
			-1);

		int i=0;
		for (FunctionDecl::param_iterator it = func_decl->param_begin(),
			end = func_decl->param_end(); it != end; it++, i++)
		{
			int param_id = get_or_add_node_id(*it);
			print_reli(PARAM, func_decl_id, i, param_id);
		}

		return true;
	}

	bool VisitReturnStmt(ReturnStmt* stmt)
	{
		int node_id = get_or_add_node(stmt);
		print_rel2(RETURN_STMT, node_id, get_or_add_node(current_func),
			stmt->getRetValue() ? get_or_add_node(stmt->getRetValue()) : -1);

		return true;
	}

	bool VisitWhileStmt(WhileStmt* stmt)
	{
		print_rel2(WHILE_STMT, get_or_add_node(stmt),
			get_or_add_node(try_replace(stmt->getCond())),
			get_or_add_node(try_replace(stmt->getBody())));

		return true;
	}

	bool VisitArraySubscriptExpr(ArraySubscriptExpr* expr)
	{
		print_rel2(ARRAY_SUBSCRIPT, get_or_add_node(expr),
			get_or_add_node(try_replace(expr->getBase())),
			get_or_add_node(try_replace(expr->getIdx())));
		return true;
	}

	bool VisitCallExpr(CallExpr* expr)
	{
		FunctionDecl* callee = expr->getDirectCallee();
		if (callee) callee = dyn_cast<FunctionDecl>(callee->getMostRecentDecl());
	
		int callee_id = callee ? get_or_add_node(callee):
			get_or_add_node(try_replace(expr->getCallee()));
		
		int call_expr_id = get_or_add_node(expr);
		print_rel1(FUNC_CALL, call_expr_id, callee_id);

/*		if (!callee)
		{
			print_rel2(ID, callee_id, "UNKNOWN_FUNC");
		}*/

		for (int i=0, argnum = expr->getNumArgs(); i < argnum; i++)
		{
			int arg_id = get_or_add_node(try_replace(expr->getArg(i)));
			print_reli(PARAM, call_expr_id, i, arg_id);
		}

		return true;
	}

	bool VisitIfStmt(IfStmt* if_stmt)
	{
		print_rel3(IF_STMT, get_or_add_node(if_stmt),
			get_or_add_node(try_replace(if_stmt->getCond())),
			get_or_add_node(try_replace(if_stmt->getThen())),
			if_stmt->getElse() ?
				get_or_add_node(try_replace(if_stmt->getElse())) :
				-1);

		return true;
	}

	bool VisitUnaryOperator(UnaryOperator* uop)
	{
		ostringstream oss;
		oss << hex << uop;
		//llvm::errs() << "DatalogTranslVisitor::VisitUnaryOperator stmt=" << oss.str() << "\n";
		const char* opcode = "";
		switch (uop->getOpcode())
		{
		case UO_AddrOf: opcode="ADDR_OF"; break;
		case UO_Deref: opcode="DEREF"; break;
		case UO_Plus: opcode="+"; break;
		case UO_Minus: opcode="-"; break;
		case UO_Not: opcode="BIT_NOT"; break;
		case UO_LNot: opcode="LNOT"; break;
		default: opcode="UNSUPPORTED_UOP"; break;
		}

		print_rel2(UOP, get_or_add_node(uop),
			get_or_add_node(try_replace(uop->getSubExpr())),
			opcode);

		return true;
	}

	bool VisitBinaryOperator(BinaryOperator* bop)
	{
/*		printf("translator, binary operator:\n");
		bop->dump();
		printf("(replaced) lhs: \n");
		try_replace(bop->getLHS())->dump();
		printf("(replaced) rhs: \n");	
		try_replace(bop->getRHS())->dump();*/

		const char* opcode = "";
		if (bop->getOpcode() == BO_Assign)
		{
			print_rel2(ASSIGN, get_or_add_node(bop),
				get_or_add_node(try_replace(bop->getLHS())),
				get_or_add_node(try_replace(bop->getRHS())));
		}
		else
		{
			switch (bop->getOpcode())
			{
			case BO_Mul: opcode="*"; break;
			case BO_Div: opcode="/"; break;

			case BO_Add: opcode=((bop->getLHS()->getType()->isPointerType() ||
				bop->getRHS()->getType()->isPointerType()) ?
				"PTR_PLUS_INT" : "+"); break;

			case BO_Sub: opcode=(bop->getLHS()->getType()->isPointerType() ?
				"PTR_MINUS_INT" : "-"); break;

			case BO_Shl: opcode="BIT_SHL"; break;
			case BO_Shr: opcode="BIT_SHR"; break;
			case BO_LT: opcode="<"; break;
			case BO_GT: opcode=">"; break;
			case BO_LE: opcode="<="; break;
			case BO_GE: opcode=">="; break;
			case BO_EQ: opcode="=="; break;
			case BO_NE: opcode="!="; break;
			case BO_And: opcode="BIT_AND"; break;
			case BO_Xor: opcode="BIT_XOR"; break;
			case BO_Or: opcode="BIT_OR"; break;
			case BO_LAnd: opcode="LAND"; break;
			case BO_LOr: opcode="LOR"; break;
			case BO_Rem: opcode="REM"; break;
			default: opcode="UNSUPPORTED_BOP"; break;
			}

			print_rel3(BOP, get_or_add_node(bop),
				get_or_add_node(try_replace(bop->getLHS())),
				get_or_add_node(try_replace(bop->getRHS())),
				opcode);
		}

		return true;
	}

	bool VisitCompoundStmt(CompoundStmt* stmt)
	{
		/*llvm::errs() << "DatalogTranslVisitor::VisitCompoundStmt";
		stmt->dump();*/

		vector<Stmt*> children;
		for (CompoundStmt::body_iterator it = stmt->body_begin(),
			end = stmt->body_end(); it != end; it++)
		{
			Stmt* child = *it ? try_replace(*it) : NULL;
			if (child && /*!isa<ReturnStmt>(child) &&*/ !isa<DeclStmt>(child))
			{
				children.push_back(*it);
			}
		}

		int size = children.size();

		vector<Stmt*>::reverse_iterator child2_it = children.rbegin(),
			child1_it = child2_it,
			end = children.rend(),
			last = end;

		if (child1_it != end) child1_it++;
		last--;

		if (size == 1)
		{
			stmt_node_ids[try_replace(*child2_it)] = get_or_add_node(stmt);
		}
		else if (size >= 2)
		{
			int seq_stmt_id,
				child2_id = get_or_add_node(try_replace(*child2_it));
			
			if (size > 2)
			{
				for (; child1_it != last; child1_it++)
				{
					seq_stmt_id = getid();
					print_rel2(SEQ, seq_stmt_id,
						get_or_add_node(try_replace(*child1_it)), child2_id);

					child2_id = seq_stmt_id;
					child2_it = child1_it;
				}
			}
	
			//llvm::errs() << "CompoundStmt node_id=" << get_or_add_node(stmt);
			print_rel2(SEQ, get_or_add_node(stmt),
				get_or_add_node(try_replace(*child1_it)), child2_id);
		}

		return true;
	}

	bool VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr* expr)
	{
		if (expr->getKind() == UETT_SizeOf)
		{
			print_rel0(SIZEOF, get_or_add_node(try_replace(expr)));
		}
		return true;
	}

	bool VisitLabelStmt(LabelStmt* stmt)
	{
		print_rel2(LABEL_STMT, get_or_add_node(try_replace(stmt)),
			stmt->getSubStmt() ? get_or_add_node(try_replace(stmt->getSubStmt())) : -1,
			stmt->getName());
		return true;
	}

	bool VisitGotoStmt(GotoStmt* stmt)
	{
		print_rel1(GOTO_STMT, get_or_add_node(try_replace(stmt)),
			get_or_add_node(try_replace(stmt->getLabel()->getStmt())));
		return true;
	}

	bool VisitBreakStmt(BreakStmt* stmt)
	{
		print_rel0(BREAK_STMT, get_or_add_node(try_replace(stmt)));
		return true;
	}

private:
	ASTContext *Context;
	map<Stmt*,int> stmt_node_ids;
	map<Decl*,int> decl_node_ids;
	CanonFormTranslator* translator;
	FunctionDecl* current_func;
	list<Stmt*> global_stmts;
	int scope;
};

class DatalogTranslConsumer : public clang::ASTConsumer {
public:
	explicit DatalogTranslConsumer(CanonFormTranslator* translator, ASTContext *Context)
		: Visitor(translator, Context) {}

	virtual void HandleTranslationUnit(clang::ASTContext &Context) {
		Visitor.TraverseDecl(Context.getTranslationUnitDecl());
		Visitor.Finish();
	}
	
private:
	DatalogTranslVisitor Visitor;
};

class DatalogTranslAction : public clang::ASTFrontendAction {
public:
	virtual clang::ASTConsumer* CreateASTConsumer(
		clang::CompilerInstance &CI, llvm::StringRef InFile)
	{
		translator = new CanonFormTranslator();
		return new DatalogTranslConsumer(translator, &CI.getASTContext());
	}

	void ExecuteAction() override
	{
	    CompilerInstance &CI = getCompilerInstance();
    	if (!CI.hasSema())
		    CI.createSema(getTranslationUnitKind(), nullptr);

		if (!CI.hasDiagnostics())
		{
			DiagnosticOptions diags = clang::DiagnosticOptions();

			clang::TextDiagnosticPrinter *DiagClient =
				new clang::TextDiagnosticPrinter(llvm::errs(), &diags);

			CI.createDiagnostics(DiagClient);
			if (!CI.hasDiagnostics()) return;
		}

		translator->setASTContext(&CI.getASTContext());
		translator->setSema(&CI.getSema());
		ASTFrontendAction::ExecuteAction();
	}

protected:
	CanonFormTranslator* translator;
};


int main(int argc, char **argv) {
	init();
	CommonOptionsParser OptionsParser(argc, (const char**)argv, "SearchGlobalSymbols");
	clang::tooling::ClangTool Tool(OptionsParser.getCompilations(),
								 OptionsParser.getSourcePathList());
	return Tool.run(newFrontendActionFactory<DatalogTranslAction>());
}

