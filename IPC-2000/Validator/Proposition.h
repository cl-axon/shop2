#include "ptree.h"
#include <iostream>
#include "Environment.h"
#include "Ownership.h"

class State;
class Action;

#ifndef __PROPOSITION
#define __PROPOSITION




class Proposition {
protected:
	const Environment & bindings;
public:
	virtual ~Proposition() {};
	Proposition(const Environment & bs) : bindings(bs) {};
	virtual bool evaluate(const State* s) const = 0;
	virtual bool markOwnedPreconditions(const Action* a,Ownership & o,ownership w) const = 0;
	bool markOwnedPreconditions(const Action * a,Ownership & o) const
	{
		return markOwnedPreconditions(a,o,E_PPRE);
	};
	virtual void write(ostream & o) const
	{
		o << "Compound proposition...";
	};
	
	virtual void destroy() const
	{
		delete this;
	};
};

ostream & operator <<(ostream & o,const Proposition & p);

class SimpleProposition : public Proposition {
private:
	static Environment nullEnvironment;

	const proposition * prop;

public:
	SimpleProposition(const parse_category * p,const Environment &bs) :
		Proposition(bs), prop(dynamic_cast<const proposition*>(p))
	{};
	SimpleProposition(const parse_category *p) :
		Proposition(nullEnvironment), prop(dynamic_cast<const proposition*>(p))
	{};

	bool evaluate(const State * s) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;

	void write(ostream & o) const;

	void destroy() const {};
};

class Comparison : public Proposition {
private:
	const comparison * comp;
public:
	Comparison(const comparison * c,const Environment & bs) :
		Proposition(bs), comp(c)
	{};
	bool evaluate(const State * s) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;
};

class ConjGoal : public Proposition {
private:
	const conj_goal * cg;
	const vector<const Proposition *> gs;
public:
	ConjGoal(const conj_goal * c,const vector<const Proposition*> & g,const Environment & bs) :
		Proposition(bs), cg(c), gs(g) 
	{};
	bool evaluate(const State *) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;
	~ConjGoal()
	{
		for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		{
			const_cast<Proposition *>(*i)->destroy();
		};
	};
};


class DisjGoal : public Proposition {
private:
	const disj_goal * dg;
	const vector<const Proposition *> gs;
public:
	DisjGoal(const disj_goal * d,const vector<const Proposition *> & g,const Environment & bs) :
		Proposition(bs), dg(d), gs(g)
	{};
	bool evaluate(const State *) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;
	~DisjGoal()
	{
		for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		{
			(*i)->destroy();
		};
	};
};

class ImplyGoal : public Proposition {
private:
	const imply_goal * ig;
	const Proposition * ant;
	const Proposition * cons;
public:
	ImplyGoal(const imply_goal * i,const Proposition * a,const Proposition * c,
					const Environment & bs) :
			Proposition(bs), ig(i), ant(a), cons(c)
	{};
	bool evaluate(const State *) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;
	~ImplyGoal()
	{
		ant->destroy();
		cons->destroy();
	};
};

class NegGoal : public Proposition {
private:
	const neg_goal * ng;
	const Proposition * p;
public:
	NegGoal(const neg_goal * n,const Proposition * pp,const Environment & bs) :
		Proposition(bs), ng(n), p(pp) 
	{};
	bool evaluate(const State *) const;
	bool markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const;
	~NegGoal()
	{
		p->destroy();
	};
};

/* Problem with quantified goals: Propositions are expected to have ground leaves,
 * but this is obviously not true of quantified goals. Should we expand them out
 * here? If so, how does this impact on the environment? Presumably we have a local
 * environment for this case, extending the general environment?
 * 

class QfiedGoal : public Proposition {
private:
	const qfied_goal * qfg;
	Proposition 

};

*/

class QfiedGoal : public Proposition {
private:
	const qfied_goal * qg;
	Environment * env;

	Validator * vld;

	// Mutable to allow single update. 
	mutable Proposition * pp;

	mutable vector<const Proposition *> props;
	mutable var_symbol_list::const_iterator i;



	

public:
	QfiedGoal(Validator * v,const qfied_goal * q,const Environment & bs) :
		Proposition(bs), qg(q), env(bs.copy(v)), vld(v), pp(0), 
		props(), i(qg->getVars()->begin()) {};

	void create() const;

	bool evaluate(const State * s) const;
	bool markOwnedPreconditions(const Action *,Ownership & o,ownership w) const;

};

//class TimedGoal : public Proposition still to do.
//class SimpleGoal ?

class Validator;

class PropositionFactory {
private:
	map<string,const SimpleProposition *> literals;
	Validator * vld;

	struct buildProp {
		PropositionFactory * myPF;
		const Environment & myEnv;
		
		buildProp(PropositionFactory * pf,const Environment & e) :
			myPF(pf), myEnv(e)
		{};
		const Proposition * operator()(const goal * g) 
		{
			return myPF->buildProposition(g,myEnv);
		};
	};
			
public:
	PropositionFactory(Validator * v) : literals(), vld(v) {};


	~PropositionFactory()
	{
		for(map<string,const SimpleProposition *>::iterator i = literals.begin();
				i != literals.end();++i)
			delete (i->second);
	};
	
	const SimpleProposition * buildLiteral(const proposition * p)
	{
		string s(p->head->getName());
		for(parameter_symbol_list::const_iterator i = p->args->begin();
					i != p->args->end();++i)
		{
			s += (*i)->getName();
		};
		map<string,const SimpleProposition*>::const_iterator i = literals.find(s);
		if(i != literals.end())
			return i->second;
		const SimpleProposition * prp = literals[s] = new SimpleProposition(p);
		return prp;
	};

	const SimpleProposition * buildLiteral(const simple_effect * eff)
	{
		return buildLiteral(eff->prop);
	};

	const SimpleProposition * buildLiteral(const proposition * p,const Environment & bs)
	{
		string s(p->head->getName());
		for(parameter_symbol_list::const_iterator i = p->args->begin();
					i != p->args->end();++i)
		{
			if(dynamic_cast<const var_symbol*>(*i))
			{
				s += bs.find(dynamic_cast<const var_symbol*>(*i))->second->getName();
			}
			else
			{
				s += (*i)->getName();
			};
		};
		map<string,const SimpleProposition*>::const_iterator i = literals.find(s);
		if(i != literals.end())
			return i->second;
		const SimpleProposition * prp = literals[s] = new SimpleProposition(p,bs);
		return prp;
	};

	const SimpleProposition * buildLiteral(const simple_effect * eff,const Environment & bs)
	{
		return buildLiteral(eff->prop,bs);
	};

	const Proposition * buildProposition(const goal * g,const Environment &bs);
	const Proposition * buildProposition(const goal * g);
};

#endif
