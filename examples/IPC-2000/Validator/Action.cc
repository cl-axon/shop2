#include "State.h"
#include "Action.h"
#include "Plan.h"
#include "main.h"
#include "Validator.h"
#include "Ownership.h"

Action::~Action()
{
	pre->destroy();
};

InvariantAction::~InvariantAction()
{
	delete act;
};

DurativeActionElement::~DurativeActionElement()
{
	delete act;
	const_cast<goal_list *>(durs)->clear();	// We don't own those expressions, so don't delete them!
	delete durs;
};

bool
Action::confirmPrecondition(const State * s) const
{
	return pre->evaluate(s);
};

bool 
DurativeActionElement::confirmPrecondition(const State * s) const
{
	for(goal_list::const_iterator i = durs->begin();i != durs->end();++i)
	{
		const comparison * c = dynamic_cast<const comparison *>(*i);
		double d = s->evaluate(c->getRHS(),bindings);
		bool test;
		switch(c->getOp())
		{
			case E_GREATER:
				test = (duration > d);
				break;
			case E_LESS:
				test = (duration < d);
				break;
			case E_GREATEQ:
				test = (s->getTolerance() >= d - duration);
				if(!test && Verbose)
				{
					cout << "Tolerance of " << d-duration 
							<< " required for " << this << "\n";
				};
				break;
			case E_LESSEQ:
				test = (duration - d <= s->getTolerance());
				if(!test && Verbose)
				{
					cout << "Tolerance of " << duration - d 
							<< " required for " << this << "\n";
				};
				break;
			case E_EQUALS:
				test = (duration > d?duration - d:d - duration) < s->getTolerance();
				if(!test && Verbose)
				{
					cout << "Tolerance of " << ((duration > d)?duration -d:d-duration) 
							<< " required for " << this << "\n";
				};
				break;
			default:
				break;
		};
		if(!test)
		{
			if(Verbose) cout << "Failed duration constraint in " << this << "\n";
			return false;
		};
	};

	return pre->evaluate(s);
};

struct MIP {
	Ownership & own;

	MIP(Ownership & o) : own(o) {};

	void operator()(const CondCommunicationAction * cca) 
	{
		cca->markInitialPreconditions(own);
	};
};

void
StartAction::markOwnedPreconditions(Ownership & o) const
{
	for_each(condActions.begin(),condActions.end(),MIP(o));
	return DurativeActionElement::markOwnedPreconditions(o);
};

void CondCommunicationAction::markInitialPreconditions(Ownership& o) const
{
	if(initPre)
		initPre->markOwnedPreconditions(this,o);
};
	
bool
StartAction::confirmPrecondition(const State * s) const
{
	for(vector<const CondCommunicationAction *>::const_iterator i = condActions.begin();
			i != condActions.end();++i)
	{
		if(!(*i)->confirmInitialPrecondition(s))
		{
			return false;
		};
	};
	return DurativeActionElement::confirmPrecondition(s);
};

bool CondCommunicationAction::confirmInitialPrecondition(const State * s) const
{		
	if(!initPre) 
	{
		status = true;
		return true;
	};
	
	status = initPre->evaluate(s);
	return true;
};
	

void
Action::markOwnedPreconditions(Ownership & o) const
{
	pre->markOwnedPreconditions(this,o);
};


void
DurativeActionElement::markOwnedPreconditions(Ownership & o) const
{
	pre->markOwnedPreconditions(this,o);
	for(goal_list::const_iterator i = durs->begin();i != durs->end();++i)
	{
		const comparison * c = dynamic_cast<const comparison *>(*i);	
		o.markOwnedPreconditionFEs(this,c->getRHS(),bindings);
	};
};

// Perhaps should be void - throw an exception if the ownership conditions are
// violated?
bool 
Action::constructEffects(Ownership & o,EffectsRecord & e,const State * s) const
{
	/* We are going to work through the effect_lists to handle each component in
	 * turn. We have a PropositionFactory that we can use to construct the literals
	 * for the terminal conditions.
	 * 
	 * There is recursion required to handle conditional effects.
	 */

	return handleEffects(o,e,s,act->effects);
};


struct FAEhandler {
	Validator * vld;
	const Action * a;
	Ownership & o;
	EffectsRecord & e;
	const State * s;
	const effect_lists * effs;
	Environment & bds;
	
	var_symbol_table::const_iterator i;
	const var_symbol_table::const_iterator endpt;
	vector<const_symbol *> cs;

	FAEhandler(Validator * v,const Action * ia,Ownership & io,EffectsRecord & ie,const State *is,
					const forall_effect * eff,const Environment & bs) :
			vld(v), a(ia), o(io), e(ie), s(is), effs(eff->getEffects()), bds(*bs.copy(v)),
			i(eff->getVars()->begin()), endpt(eff->getVars()->end()),
			cs(vld->range(i->second)) {};

	bool handle()
	{
		if(i == endpt) 
		{
			Environment * env = bds.copy(vld);
			return a->handleEffects(o,e,s,effs,*env);
		};
		var_symbol_table::const_iterator j = i++;
		vector<const_symbol *> ds = i != endpt?vld->range(i->second):vector<const_symbol *>();
		ds.swap(cs);
		for(vector<const_symbol *>::iterator k = ds.begin();k != ds.end();++k)
		{
			//cout << "Handling " << j->second->getName() << "\n";
			bds[j->second] = *k;
			if(!handle()) return false;
		};
		return true;
	};
};

bool 
Action::handleEffects(Ownership & o,EffectsRecord & e,
						const State * s,const effect_lists * effs,const Environment & bds) const
{
	for(list<simple_effect*>::const_iterator i = effs->add_effects.begin();
					i != effs->add_effects.end();++i)
	{
		const SimpleProposition * p = vld->pf.buildLiteral((*i)->prop,bds);
		if(!o.ownsForAdd(this,p)) 
		{
			return false;
		};
		e.pushAdd(p);
	};

	for(list<simple_effect*>::const_iterator i = effs->del_effects.begin();
					i != effs->del_effects.end();++i)
	{
		const SimpleProposition * p = vld->pf.buildLiteral((*i)->prop,bds);
		if(!o.ownsForDel(this,p))
		{
			return false;
		};
		e.pushDel(p);
	};
	
	for(list<cond_effect*>::const_iterator i = effs->cond_effects.begin();
			i != effs->cond_effects.end();++i)
	{
		// First check preconditions are satisfied.
		const Proposition * p = vld->pf.buildProposition((*i)->getCondition(),bds);
		if(p->evaluate(s))
		{
			//cout << *p << " satisfied\n";
			if(!p->markOwnedPreconditions(this,o) ||
				!handleEffects(o,e,s,(*i)->getEffects(),bds)) 
			{
				p->destroy();
				if(Verbose) 
					cout << "Violation in conditional effect in " << this;
				return false;
			};
		};
		p->destroy();
	};

	for(list<assignment*>::const_iterator i = effs->assign_effects.begin();
			i != effs->assign_effects.end();++i)
	{
		// LHS is owned for appropriate update.
		// RHS will be owned as if for preconditions.
		// Assignment cannot be applied because of the usual problem of conditional
		// effects. RHS can be evaluated and then the update recorded.
		const FuncExp * lhs = vld->fef.buildFuncExp((*i)->getFTerm(),bds);
		double v = s->evaluate((*i)->getExpr(),bds);
		if(!o.markOwnedEffectFE(this,lhs,(*i)->getOp(),(*i)->getExpr(),bds))
		{
			return false;
		};
		e.addFEffect(lhs,(*i)->getOp(),v);
	};

	for(list<forall_effect*>::const_iterator i = effs->forall_effects.begin();
			i != effs->forall_effects.end();++i)
	{
		FAEhandler faeh(vld,this,o,e,s,*i,bds);
		if(!faeh.handle()) return false;
	};
	
	return true;
};

bool Action::handleEffects(Ownership & o,EffectsRecord & e,
							const State * s,const effect_lists * effs) const
{
	return handleEffects(o,e,s,effs,bindings);
};

const Environment buildBindings(const action * a,const const_symbol_list * bs)
{
	Environment bindings;
	const_symbol_list::const_iterator j = bs->begin();
	for(var_symbol_list::iterator i = a->parameters->begin();
			i != a->parameters->end();++i,++j)
	{
		bindings[*i] = *j;
	};
	return bindings;
};

Action::Action(Validator * v,const action * a,const const_symbol_list* bs) : 
		act(a), bindings(buildBindings(a,bs)), vld(v), pre(vld->pf.buildProposition(act->precondition,bindings))
{};

CondCommunicationAction::CondCommunicationAction(Validator * v,const durative_action * a,const const_symbol_list * bs,
	goal_list * gs,goal_list * gi,goal_list * ge,
	effect_lists * es,effect_lists * el) : 
	Action(v,new safeaction(a->name,a->parameters,new conj_goal(const_cast<goal_list*>(ge)),el,a->symtab),bs), 
	status(true),
	gls(new conj_goal(const_cast<goal_list*>(gs))), 
	initPre(gs->empty()?0:vld->pf.buildProposition(gls,bindings)),
	gli(new conj_goal(const_cast<goal_list*>(gi))), 
	invPre(gi->empty()?0:vld->pf.buildProposition(gli,bindings)),
	gle(act->precondition),
	els(es), ele(el) {};

CondCommunicationAction::~CondCommunicationAction() {
	delete initPre;
	delete invPre;
	delete act;
	conj_goal * cg = dynamic_cast<conj_goal*>(gls);
	if(cg) const_cast<goal_list *>(cg->getGoals())->clear();
	cg = dynamic_cast<conj_goal*>(gli);
	if(cg) const_cast<goal_list *>(cg->getGoals())->clear();
	delete gls;
	delete gli;
	els->add_effects.clear();
	els->del_effects.clear();
	els->forall_effects.clear();
	els->cond_effects.clear();
	els->assign_effects.clear();	
	delete els;
};

void Action::adjustContext(ExecutionContext & ec) const
{};

struct ContextAdder {

	ExecutionContext & ec;

	ContextAdder(ExecutionContext & e) : ec(e) {};

	void operator()(const CondCommunicationAction* cca)
	{
		if(cca->isActive())
			ec.addCondAction(cca);
	};
};

void StartAction::adjustContext(ExecutionContext & ec) const
{
	ec.addInvariant(invariant);
	for_each(condActions.begin(),condActions.end(),ContextAdder(ec));
};

struct ContextRemover {

	ExecutionContext & ec;

	ContextRemover(ExecutionContext & e) : ec(e) {};

	void operator()(const CondCommunicationAction* cca)
	{
			ec.removeCondAction(cca);
	};
};


void EndAction::adjustContext(ExecutionContext & ec) const
{
	ec.removeInvariant(invariant);
	for_each(condActions.begin(),condActions.end(),ContextRemover(ec));
};

ostream & operator <<(ostream & o,const Action & a)
{
	a.write(o);
	return o;
};

ostream & operator <<(ostream & o,const Action * const a)
{
	a->write(o);
	return o;
};

void CondCommunicationAction::markOwnedPreconditions(Ownership & o) const 
{
	if(invPre && status)
		invPre->markOwnedPreconditions(this,o);
};

bool CondCommunicationAction::confirmPrecondition(const State * s) const
{
	if(invPre && status)
		if(!invPre->evaluate(s)) status = false;
		
	return true;
};

bool CondCommunicationAction::constructEffects(Ownership & o,EffectsRecord & e,const State * s) const
{
	return true;
};

bool CondCommunicationAction::constructFinalEffects(Ownership & o,EffectsRecord & e,const State * s) const
{
	if(status) 
	{
		return Action::constructEffects(o,e,s);
	};
	return true;
};

bool EndAction::constructEffects(Ownership & o,EffectsRecord & e,const State * s) const 
{
	if(!Action::constructEffects(o,e,s)) 
	{
		return false;
	};
	for(vector<const CondCommunicationAction*>::const_iterator i = condActions.begin();
				i != condActions.end();++i)
	{
		if(!(*i)->constructFinalEffects(o,e,s))
		{
			if(Verbose)
			{
				cout << "Failure in application of effects for temporal conditional effect in " << (*i) << "\n";
			};
			return false;
		};
	};
	return true;
};
	
	
