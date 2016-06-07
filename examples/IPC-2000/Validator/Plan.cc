#include "State.h"
#include "Plan.h"
#include "Action.h"
#include "Validator.h"
#include "main.h"

bool
Happening::canHappen(const State * s) const
{
	if(Verbose) cout << "Checking next happening (time " << time << ")\n";
	for(vector<const Action*>::const_iterator a = actions.begin();a != actions.end();++a)
	{
		if(!(*a)->confirmPrecondition(s))
		{
			if(Verbose) cout << "Plan failed because of unsatisfied precondition in:\n" << (*a) 
					<< "\n";
			return false;
		};
	};
	
	return true;
	
};

Happening::~Happening()
{
	for(vector<const Action *>::const_iterator i = actions.begin();i != actions.end();++i)
	{
			delete (*i);
	};
};



// Is it best to return a bool or throw an exception?
bool 
Happening::applyTo(State * s) const
{
	/* This function will update the state:
	 * 
	 * 	- for each action, need to determine which conditional effects are active;
	 * 	- must also mark ownership of propositions and functional expressions;
	 * 	- confirm no interaction;
	 * 	- set the time in new state.
	 */

	// First establish ownership of preconditions.
	Ownership own(vld);
	
	for(vector<const Action*>::const_iterator a = actions.begin();a != actions.end();++a)
	{
		(*a)->markOwnedPreconditions(own);
	};

	EffectsRecord effs;
	
	for(vector<const Action*>::const_iterator a = actions.begin();a != actions.end();++a)
	{
		if(!(*a)->constructEffects(own,effs,s)) return false;
	};

	effs.enact(s);
	
	return true;

};

void Happening::adjustContext(ExecutionContext & ec) const
{
	for(vector<const Action*>::const_iterator a = actions.begin();a != actions.end();++a)
	{
		(*a)->adjustContext(ec);
	};
};

void ExecutionContext::addCondAction(const CondCommunicationAction * ca)
{
	invariants.actions.push_back(ca);
};

bool ExecutionContext::removeCondAction(const CondCommunicationAction * ca)
{
	vector<const Action *>::iterator i = find(invariants.actions.begin(),invariants.actions.end(),ca);
	if(i != invariants.actions.end())
	{
		invariants.actions.erase(i);
		return true;
	};
	return false;
};

ExecutionContext::ExecutionContext(Validator * v) :
	invariants(v)
{};

void ExecutionContext::addInvariant(const InvariantAction * a)
{
	invariants.actions.push_back(a);
};	

void ExecutionContext::removeInvariant(const InvariantAction * a)
{
	invariants.actions.erase(remove(invariants.actions.begin(),invariants.actions.end(),a),
										invariants.actions.end());
};

void ExecutionContext::setTime(double t)
{
	invariants.time = t;
};

ExecutionContext::~ExecutionContext()
{
	invariants.actions.clear();
};

bool ExecutionContext::hasInvariants() const
{
	return !(invariants.actions.empty());
};

void Adder(State * s,const SimpleProposition * p)
{
	s->add(p);
};

void Deleter(State * s,const SimpleProposition * p)
{
	s->del(p);
};

void Assigner(State * s,Update u)
{
	u.update(s);
};

void EffectsRecord::enact(State * s) const
{
	for_each(dels.begin(),dels.end(),bind1st(ptr_fun(Deleter),s));
	for_each(adds.begin(),adds.end(),bind1st(ptr_fun(Adder),s));
	for_each(updates.begin(),updates.end(),bind1st(ptr_fun(Assigner),s));
};

/* Tricky bit: 
 * 	The effect_lists of each action will include universal effects, conditional 
 * 	effects, add and delete effects and assign effects (ignore timed effects for
 * 	the moment). Add and delete effects are easy. Universal effects must be 
 * 	instantiated for all possible values of the quantified variables - no way round
 * 	this I don't think. Conditional effects are more problematic: we have to first
 * 	confirm that the condition is satisfied in the current state, marking ownership
 * 	as well and then treat the effects using the
 * 	standard effect_lists handling machinery - use recursion here.
 * 	
 * 	At the end of the process we want all the postconditions to be tidily separated 
 * 	into add, delete and assign effects. Can't enact them as they are checked because
 * 	of conditional effects. Therefore, we need to build up the effects as we go along.
 *
 */ 



	

Plan::Plan(Validator * v,const operator_list * ops,const plan * p) :
	vld(v)
{
	timedActionSeq planStructure;

	planStructure.reserve(p->size());

	for_each(p->begin(),p->end(),planBuilder(v,planStructure,ops));
	
	sort(planStructure.begin(),planStructure.end());
	
	timedActionSeq::iterator i = planStructure.begin();
	while(i != planStructure.end())
	{
		timedActionSeq::iterator j 
				= find_if(i,planStructure.end(),after(i->first,v->getTolerance()));
		timedActionSeq vs(i,j);
		happenings.push_back(new Happening(vld,vs));
		i = j;
	};
};

int Plan::length() const
{
	return happenings.size();
};

void Plan::display() const
{
	cout << "Plan size: " << length() << "\n";
};

ostream & operator << (ostream & o,const Plan & p)
{
	p.display();
	copy(p.begin(),p.end(),ostream_iterator<const Happening *>(o,"\n"));
	return o;
};

void Happening::write(ostream & o) const
{
	o << time << ":\n";
	copy(actions.begin(),actions.end(),ostream_iterator<const Action * const>(o,"\n"));
};

ostream & operator << (ostream & o,const Happening * h)
{
	h->write(o);
	return o;
};

void Update::update(State * s) const
{
	s->update(fe,aop,value);
};

void insert_effects(effect_lists * el,effect_lists * more)
{
	el->add_effects.insert(el->add_effects.begin(),
					more->add_effects.begin(),more->add_effects.end());
	el->del_effects.insert(el->del_effects.begin(),
					more->del_effects.begin(),more->del_effects.end());	
	el->forall_effects.insert(el->forall_effects.begin(),
					more->forall_effects.begin(),more->forall_effects.end());					
	el->cond_effects.insert(el->cond_effects.begin(),
					more->cond_effects.begin(),more->cond_effects.end());
	el->assign_effects.insert(el->assign_effects.begin(),
					more->assign_effects.begin(),more->assign_effects.end());
	// Don't need to handle timed effects because this is used to insert effects
	// from one timed effects structure into another and they cannot be nested.
};



void handleDAgoals(const goal * gl,goal_list * gls,goal_list * gli,goal_list * gle)
{
	if(const conj_goal * cg = dynamic_cast<const conj_goal *>(gl))
	{
		for(goal_list::const_iterator i = cg->getGoals()->begin();i != cg->getGoals()->end();++i)
		{
			if(const timed_goal * tg = dynamic_cast<const timed_goal *>(*i))
			{
				switch(tg->getTime())
				{
					case E_AT_START:
						gls->push_back(const_cast<goal*>(tg->getGoal()));
						continue;
					case E_AT_END:
						gle->push_back(const_cast<goal*>(tg->getGoal()));
						continue;
					case E_OVER_ALL:
						gli->push_back(const_cast<goal*>(tg->getGoal()));
						continue;

					default:
						continue;
				};
			}
			else
			{
				if(Verbose) cout << "Untimed precondition in a durative action!\n";
				UnrecognisedCondition uc;
				throw uc;
			};
		};
	}
	else
	{
		if(const timed_goal * tg = dynamic_cast<const timed_goal *>(gl))
			{
				switch(tg->getTime())
				{
					case E_AT_START:
						gls->push_back(const_cast<goal*>(tg->getGoal()));
						break;
					case E_AT_END:
						gle->push_back(const_cast<goal*>(tg->getGoal()));
						break;
					case E_OVER_ALL:
						gli->push_back(const_cast<goal*>(tg->getGoal()));
						break;

					default:
						break;
				};
			}
			else
			{
				if(Verbose) cout << "Untimed precondition in a durative action!\n";
				UnrecognisedCondition uc;
				throw uc;
			};
	};
};


void handleDAeffects(const effect_lists * efcts,effect_lists * els,effect_lists * ele)
{
	for(pc_list<timed_effect*>::const_iterator i = efcts->timed_effects.begin();
			i != efcts->timed_effects.end();++i)
	{
		switch((*i)->ts)
		{
			case E_AT_START:
				insert_effects(els,(*i)->effs);
				continue;
			case E_AT_END:
				insert_effects(ele,(*i)->effs);
				continue;
			case E_CONTINUOUS:
				if(Verbose) cout << "Continuous effect: This validator cannot handle continuous effects\n";
				{
					SyntaxTooComplex stc;
					throw stc; 
				};
				continue;
			default:
				continue;
		};
	};
};

struct handleDAConditionalEffects {

	Validator * vld;
	const durative_action * da;
	const const_symbol_list * params;
	effect_lists * els;
	effect_lists * ele;
	vector<CondCommunicationAction *> condActions;
	
	handleDAConditionalEffects(Validator * v,const durative_action * d,const const_symbol_list * ps,
										effect_lists * es,effect_lists * ee) :
			vld(v), da(d), params(ps), els(es), ele(ee)
	{};

	void operator()(const cond_effect * ce)
	{
		effect_lists * locels = new effect_lists();
		effect_lists * locele = new effect_lists();

		handleDAeffects(ce->getEffects(),locels,locele);


		if(locele->add_effects.empty() && locele->del_effects.empty() && locele->forall_effects.empty()
				&& locele->assign_effects.empty())
		{
			goal_list * gls = new goal_list();
			handleDAgoals(ce->getCondition(),gls,0,0);
			cond_effect * nce = new cond_effect(new conj_goal(gls),locels);
			els->cond_effects.push_back(nce);
			delete locele;
			return;
		};

		goal_list * gls = new goal_list();
		goal_list * gle = new goal_list();
		goal_list * gli = new goal_list();

		handleDAgoals(ce->getCondition(),gls,gli,gle);

		if(gls->empty() && gli->empty())
		{
			delete gls;
			delete gli;
			cond_effect * nce = new cond_effect(new conj_goal(gle),locele);
			ele->cond_effects.push_back(nce);
			delete locels;
			return;
		};

		condActions.push_back(new CondCommunicationAction(vld,da,params,gls,gli,gle,locels,locele));
	};
};

void 
Plan::planBuilder::handleDurativeAction(const durative_action * da,const const_symbol_list * params,
												double start,double duration)
{
	goal_list * gls = new goal_list();
	goal_list * gle = new goal_list();
	goal_list * gli = new goal_list();
	conj_goal * cgs = new conj_goal(gls);
	conj_goal * cge = new conj_goal(gle);
	conj_goal * inv = new conj_goal(gli);

	handleDAgoals(da->precondition,gls,gli,gle);

	effect_lists * els = new effect_lists();
	effect_lists * ele = new effect_lists();

	handleDAeffects(da->effects,els,ele);
	handleDAConditionalEffects hDAc
		= for_each(da->effects->cond_effects.begin(),da->effects->cond_effects.end(),
				handleDAConditionalEffects(vld,da,params,els,ele));

	goal_list * ds = new goal_list();
	goal_list * de = new goal_list();
	
	if(const conj_goal * cg = dynamic_cast<const conj_goal *>(da->dur_constraint))
	{
		for(goal_list::const_iterator i = cg->getGoals()->begin();
				i != cg->getGoals()->end(); ++i)
		{
			if(const timed_goal * tg = dynamic_cast<const timed_goal *>(*i))
			{
				switch(tg->getTime())
				{
					case E_AT_START:
						ds->push_back(const_cast<goal*>(tg->getGoal()));
						continue;
					case E_AT_END:
						de->push_back(const_cast<goal*>(tg->getGoal()));
						continue;

					default:
						continue;
				};
			};
		};
	}
	else
	{
		if(const timed_goal * tg = dynamic_cast<const timed_goal *>(da->dur_constraint))
		{
			switch(tg->getTime())
			{
				case E_AT_START:
					ds->push_back(const_cast<goal*>(tg->getGoal()));
					break;
				case E_AT_END:
					de->push_back(const_cast<goal*>(tg->getGoal()));
					break;

				default:
					break;
			};
		};
	};
	
	action * das = new safeaction(da->name,da->parameters,cgs,els,da->symtab);
	action * dae = new safeaction(da->name,da->parameters,cge,ele,da->symtab);
		// Note that we must use safeactions here, to ensure that the actions we create don't
		// think they own their components for deletion.

	StartAction * sa = new StartAction(vld,das,params,inv,duration,ds,hDAc.condActions);
	tas.push_back(make_pair(start,sa));
	tas.push_back(make_pair(start+duration,new EndAction(vld,dae,params,sa,duration,de)));
};

void Plan::planBuilder::operator()(const plan_step * ps)
{
	double t;
	if(ps->start_time_given)
	{
		t = ps->start_time;
	}
	else
	{
		t = defaultTime++;
	};

	for(operator_list::const_iterator i = ops->begin();i != ops->end();++i)
	{
		if((*i)->name->getName() == ps->op_sym->getName())
		{
			if(const action * a = dynamic_cast<const action *>(*i))
			{
				tas.push_back(make_pair(t,new Action(vld,a,ps->params)));
				return;
			};

			if(const durative_action * da = dynamic_cast<const durative_action *>(*i))
			{
				handleDurativeAction(da,ps->params,t,ps->duration);
				return;
			};

			if(Verbose) cout << "Unknown operator type in plan: " << (*i)->name->getName() << "\n";
			BadOperator bo;
			throw bo;
		};
	};

	if(Verbose) cout << "No matching action defined for " << ps->op_sym->getName() << "\n";
	BadOperator bo;
	throw bo;
};
