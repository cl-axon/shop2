#include "ptree.h"
#include "Proposition.h"
#include <iostream>

#ifndef __ACTION
#define __ACTION

class State;
class Ownership;
class EffectsRecord;
class Validator;
class ExecutionContext;

struct safeaction : public action {

	safeaction(operator_symbol* nm,
	    var_symbol_list* ps,
	    goal* pre,
	    effect_lists* effs,
	    var_symbol_table* st) : action(nm,ps,pre,effs,st) {};

	~safeaction() 
	{
		conj_goal * cg = dynamic_cast<conj_goal*>(precondition);
		if(cg) const_cast<goal_list *>(cg->getGoals())->clear();
						// Mustn't delete the preconditions.
		symtab = 0;		// Mustn't delete the symbol table either.
		parameters = 0;	// Or the parameters.
						// Finally, we don't own the effects, so mustn't 
						// clobber those either.
		effects->add_effects.clear();
		effects->del_effects.clear();
		effects->forall_effects.clear();
		effects->cond_effects.clear();
		effects->assign_effects.clear();			
	};
};	


class Action {
protected:
		const action * act;
		Environment bindings;

		Validator * vld;
		
		const Proposition * pre;

	bool handleEffects(Ownership & o,EffectsRecord & e,
							const State * s,const effect_lists * effs,const Environment & env) const;
	bool handleEffects(Ownership & o,EffectsRecord & e,
							const State * s,const effect_lists * effs) const;

		struct ActionParametersOutput {

			const Environment & bindings;

			ActionParametersOutput(const Environment & bs) : bindings(bs) {};
			string operator()(const var_symbol * v) const
			{
				return bindings.find(v)->second->getName();
			};
		};

	friend class FAEhandler;
		
public:
	Action(Validator * v,const action * a,const const_symbol_list* bs);
	
	virtual ~Action();


	virtual bool confirmPrecondition(const State * s) const;
	virtual void markOwnedPreconditions(Ownership & o) const;	
	virtual bool constructEffects(Ownership & o,EffectsRecord & e,const State * s) const;
	virtual void adjustContext(ExecutionContext &) const;

	virtual void write(ostream & o) const
	{
		o << "(" << act->name->getName() << " ";
		transform(act->parameters->begin(),act->parameters->end(),
					ostream_iterator<string>(o," "),ActionParametersOutput(bindings));
		o << ")";
	};
};

class InvariantAction : public Action {
public:
	InvariantAction(Validator * v,const action * a,const const_symbol_list* bs) :
		Action(v,a,bs)
	{};	

	~InvariantAction();

	void write(ostream & o) const
	{
		o << "Invariant for ";
		Action::write(o);
	};
};

class CondCommunicationAction : public Action {
private:
	mutable bool status;

	conj_goal * gls;
	const Proposition * initPre;
	conj_goal * gli;
	const Proposition * invPre;
	goal * gle;			// Will actually be a conj_goal *, but it's not important.

	effect_lists * els;
	effect_lists * ele;

public:
	CondCommunicationAction(Validator * v,const durative_action * a,const const_symbol_list * bs,
		goal_list * gs,goal_list * gi,goal_list * ge,
		effect_lists * es,effect_lists * el);
	~CondCommunicationAction();

	void write(ostream & o) const 
	{
		Action::write(o);
		o << " - conditional effect monitor";
	};

	void markInitialPreconditions(Ownership & o) const;
	void markOwnedPreconditions(Ownership & o) const;
	bool confirmPrecondition(const State * s) const;
	bool constructEffects(Ownership & o,EffectsRecord & e,const State * s) const;
	
	bool confirmInitialPrecondition(const State * s) const;
	bool constructFinalEffects(Ownership & o,EffectsRecord & e,const State * s) const;
	bool isActive() const {return status;};
};

class DurativeActionElement : public Action {
protected:
	double duration;
	const goal_list * durs;

	const InvariantAction * invariant;
	const vector<CondCommunicationAction *> condActions;
	
public:
	DurativeActionElement(Validator * v,const action * a,const const_symbol_list* bs,
					double d,const goal_list * ds,const InvariantAction * inv,
					const vector<CondCommunicationAction *> & cas) :
		Action(v,a,bs), 
		duration(d), durs(ds), invariant(inv), condActions(cas)
	{
		bindings.duration = duration;
	};
	virtual ~DurativeActionElement();

	void markOwnedPreconditions(Ownership &) const;
	bool confirmPrecondition(const State * s) const;

	
};
		
	

class StartAction : public DurativeActionElement {
public:
	friend class EndAction;
	
	StartAction(Validator * v,const action * a,const const_symbol_list* bs,
					const conj_goal * inv,double d,const goal_list * ds,
					const vector<CondCommunicationAction*> & cas) :
		DurativeActionElement(v,a,bs,d,ds,
			new InvariantAction(v,
				new safeaction(a->name,a->parameters,const_cast<conj_goal *>(inv),
							new effect_lists(),a->symtab),bs),cas)
	{};	

	~StartAction() 
	{
		delete invariant;
	};
	
	void adjustContext(ExecutionContext &) const;
	void markOwnedPreconditions(Ownership & o) const;
	bool confirmPrecondition(const State *) const;
	
	void write(ostream & o) const
	{
		Action::write(o);
		o << " - start";
	};
};

class EndAction : public DurativeActionElement {
public:
	EndAction(Validator * v,const action * a,const const_symbol_list* bs,
				const StartAction * sa,double d, const goal_list * ds) :
		DurativeActionElement(v,a,bs,d,ds,sa->invariant,sa->condActions)
	{};
	~EndAction()
	{};

	void adjustContext(ExecutionContext &) const;
 	bool constructEffects(Ownership & o,EffectsRecord & e,const State * s) const;

	void write(ostream & o) const
	{
		Action::write(o);
		o << " - end";
	};
};


	
ostream & operator <<(ostream & o,const Action & a);
ostream & operator << (ostream & o, const Action * const a);


#endif















