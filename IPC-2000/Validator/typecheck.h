#include "ptree.h"
#include <set>

#ifndef __TYPECHECK
#define __TYPECHECK

class PTypeRef;
class UTypeRef;
class TypeHierarchy;

struct TypeRef {
	virtual ~TypeRef(){};
	virtual bool operator<(const TypeRef & t) const = 0;
	virtual bool operator>(const PTypeRef & p) const = 0;
	virtual bool operator>(const UTypeRef & u) const = 0;
	virtual bool operator==(const TypeRef & t) const = 0;
	virtual TypeRef * clone() const = 0;
	virtual bool operator==(const PTypeRef & p) const {return false;};
	virtual bool operator==(const UTypeRef & u) const {return false;};
	virtual bool expected() const {return true;};
	virtual void addContents(TypeHierarchy * th) const {};
	virtual void show() const = 0;
};


class PTypeRef : public TypeRef {
private:
	const pddl_type * pt;
public:
	PTypeRef(const pddl_type * p) : pt(p) {};
	bool operator<(const TypeRef & t) const
	{
		return t > *this;
	};
	bool operator>(const PTypeRef & p) const
	{
		return p.pt < pt;
	};
	bool operator>(const UTypeRef & u) const
	{
		return false;
	};
	bool operator==(const TypeRef & t) const
	{
		return t==*this;
	};
	bool operator==(const PTypeRef & p) const
	{
		return pt == p.pt;
	};
	PTypeRef * clone() const 
	{
		return new PTypeRef(*this);
	};
	void show() const
	{
		cout << *pt << "\n";
	};
};

class UTypeRef : public TypeRef {
private:
	const set<const pddl_type *> pts;
public:
	UTypeRef(const pddl_type_list* ps) : pts(ps->begin(),ps->end())
	{};

	UTypeRef() {};

	bool operator<(const TypeRef &t) const
	{
		return t > *this;
	};
	bool operator>(const PTypeRef & p) const
	{
		return true;
	};
	bool operator>(const UTypeRef & u) const
	{
		return u.pts < pts;
	};
	bool operator==(const TypeRef & t) const
	{
		return t == *this;
	};
	bool operator==(const UTypeRef & u) const
	{
		return pts == u.pts;
	};
	UTypeRef * clone() const 
	{
		return new UTypeRef(*this);
	};
	bool expected() const {return false;};
	void addContents(TypeHierarchy*) const;
	void show() const 
	{
		cout << "UType\n";
	};
};

struct TRcompare : public std::binary_function<const TypeRef*,const TypeRef*,bool> {

	bool operator()(const TypeRef * t1,const TypeRef * t2) const
	{
		return *t1 < *t2;
	};
};

class TypeHierarchy  {
private:
	typedef map<const TypeRef *, set<const TypeRef*>, TRcompare> Graph;
	typedef Graph::iterator GI;
	
	Graph graph;
	bool closure(GI & gi,set<const TypeRef*> & vs,GI & gs,const TypeRef * t);
	TypeHierarchy(const TypeHierarchy & th);
	
public:
	TypeHierarchy(const analysis * a);
	~TypeHierarchy();
	bool reachable(const TypeRef & t1,const TypeRef & t2);
	void add(const PTypeRef & t,const TypeRef & u);
};


class TypeChecker {
private:
	const analysis *thea;
	TypeHierarchy th;
	const bool isTyped;

public:
	TypeChecker(const analysis * a) : thea(a), th(a), isTyped(a->the_domain->types) {};
	bool typecheckDomain();
	bool typecheckAction(const operator_ * act);
	bool typecheckProblem();
	bool typecheckPlan(const plan * p);
	bool typecheckGoal(const goal * g);
	bool typecheckProposition(const proposition * g);
	bool typecheckActionInstance(const plan_step * p);
	bool typecheckEffect(const effect * e);
	bool typecheckEffects(const effect_lists * e);
	bool typecheckFuncTerm(const func_term * f);
	bool typecheckExpression(const expression * e);
	bool subType(const pddl_typed_symbol *,const pddl_typed_symbol *);
	bool subType(const pddl_type *,const pddl_typed_symbol *);
	bool subType(const pddl_type *,const pddl_type *);

	vector<const_symbol *> range(const var_symbol * v);
};


#endif
