#include "Proposition.h"
#include "State.h"
#include "Action.h"
#include "Plan.h"
#include "Validator.h"

Environment SimpleProposition::nullEnvironment;

ostream & operator <<(ostream & o,const Proposition & p)
{
	p.write(o);
	return o;
};

bool
ConjGoal::evaluate(const State * s) const
{
	for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		if(!(*i)->evaluate(s)) return false;
	return true;
};

bool 
ConjGoal::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		if(!(*i)->markOwnedPreconditions(a,o,w)) return false;
	return true;
};

bool
DisjGoal::evaluate(const State * s) const
{
	for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		if((*i)->evaluate(s)) return true;
	return false;
};

bool 
DisjGoal::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	for(vector<const Proposition *>::const_iterator i = gs.begin();i != gs.end();++i)
		if(!(*i)->markOwnedPreconditions(a,o,E_PRE)) return false;
	return true;
};

bool
SimpleProposition::evaluate(const State * s) const
{
	return s->evaluate(this);
};

bool 
SimpleProposition::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	return o.markOwnedPrecondition(a,this,w);
};

bool 
Comparison::evaluate(const State * s) const
{
	double lhs = s->evaluate(comp->getLHS(),bindings);
	double rhs = s->evaluate(comp->getRHS(),bindings);
	
	switch(comp->getOp())
	{
		case E_GREATER:
			return lhs > rhs;
			
		case E_GREATEQ:
			return lhs >= rhs;
			
		case E_LESS:
			return lhs < rhs;
			
		case E_LESSEQ:
			return lhs <= rhs;
			
		case E_EQUALS:
			return lhs == rhs;
			
		default:
			return false;
	};
};

bool 
Comparison::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	return o.markOwnedPreconditionFEs(a,comp->getLHS(),bindings) &&
		   o.markOwnedPreconditionFEs(a,comp->getRHS(),bindings);
};


bool
ImplyGoal::evaluate(const State * s) const
{
	return (!(ant->evaluate(s)) || cons->evaluate(s));
};

bool 
ImplyGoal::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	return (ant->markOwnedPreconditions(a,o,E_PRE) && 
			cons->markOwnedPreconditions(a,o,E_PRE));
};

bool
NegGoal::evaluate(const State * s) const
{
	return !(p->evaluate(s));
};

bool 
NegGoal::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	ownership w1 = w==E_PRE?E_PRE:(w==E_PPRE?E_NPRE:E_PPRE);
	return (p->markOwnedPreconditions(a,o,w1));
};

const Proposition *
PropositionFactory::buildProposition(const goal * g)
{
	static Environment nullBindings;

	return buildProposition(g,nullBindings);
};

void QfiedGoal::create() const
{
	//cout << "Create\n";
	if(i == qg->getVars()->end())
	{
		props.push_back(vld->pf.buildProposition(qg->getGoal(),*(env->copy(vld))));
		//cout << *(props.back()) << "\n";
	}
	else
	{
		//cout << "Processing " << (*i)->getName() << "\n";
		vector<const_symbol *> vals = vld->range(*i);
		const var_symbol * v = *i;
		
		++i;
		for(vector<const_symbol*>::iterator j = vals.begin();j != vals.end();++j)
		{
			//cout << "considering value " << (*j)->getName() << "\n";
			(*env)[v] = *j;
			create();
		};
		if(i == qg->getVars()->end())
		{
			if(qg->getQuantifier()==E_FORALL)
			{
				pp = new ConjGoal(0,props,bindings);
			}
			else
			{
				pp = new DisjGoal(0,props,bindings);
			};
		};
		--i;
	};
};

bool QfiedGoal::evaluate(const State * s) const 
{
	if(!pp) create();

	return pp->evaluate(s);
};

bool QfiedGoal::markOwnedPreconditions(const Action * a,Ownership & o,ownership w) const
{
	if(!pp) create();

	return pp->markOwnedPreconditions(a,o,w);
};

const Proposition * 
PropositionFactory::buildProposition(const goal * g,const Environment &bs)
{
	if(dynamic_cast<const comparison*>(g))
	{
		return new Comparison(dynamic_cast<const comparison*>(g),bs);
	};
	
	if(dynamic_cast<const conj_goal *>(g))
	{
		const conj_goal * cg = dynamic_cast<const conj_goal *>(g);
		vector<const Proposition*> gs;
		gs.reserve(cg->getGoals()->size());
		transform(cg->getGoals()->begin(),
					cg->getGoals()->end(),back_inserter(gs),
						buildProp(this,bs));
		
		return new ConjGoal(cg,gs,bs);
	};


	if(dynamic_cast<const disj_goal*>(g))
	{
		const disj_goal * dg = dynamic_cast<const disj_goal*>(g);
		vector<const Proposition*> gs;
		gs.reserve(dg->getGoals()->size());
		transform(dg->getGoals()->begin(),
					dg->getGoals()->end(),back_inserter(gs),
						buildProp(this,bs));
		return new DisjGoal(dg,gs,bs);
	};

	if(dynamic_cast<const neg_goal *>(g))
	{
		const neg_goal * ng = dynamic_cast<const neg_goal *>(g);
		return new NegGoal(ng,buildProposition(ng->getGoal(),bs),
							bs);
	};

	if(dynamic_cast<const imply_goal*>(g))
	{
		const imply_goal * ig = dynamic_cast<const imply_goal*>(g);
		return new ImplyGoal(ig,buildProposition(ig->getAntecedent(),bs),
								buildProposition(ig->getConsequent(),bs),bs);
	};

	if(dynamic_cast<const simple_goal*>(g))
	{
		const simple_goal * sg = dynamic_cast<const simple_goal*>(g);
		if(sg->getPolarity()==E_POS)
		{
			return buildLiteral(sg->getProp(),bs);
		}
		else
			return new NegGoal(new neg_goal(new simple_goal(const_cast<proposition*>(sg->getProp()),E_POS)),
								buildLiteral(sg->getProp(),bs),bs);
	};

	if(dynamic_cast<const qfied_goal*>(g))
	{
		const qfied_goal * qg = dynamic_cast<const qfied_goal*>(g);
		/* Instead of building a new type here, we could simply do expansion immediately
		 * into the appropriate conj or disj form.
		 */

		return new QfiedGoal(vld,qg,bs);

	};
	
	return 0;
};

void SimpleProposition::write(ostream & o) const
{
	o << "(" << prop->head->getName();

	
	for(parameter_symbol_list::const_iterator i = prop->args->begin();
				i != prop->args->end();++i)
	{
		if(dynamic_cast<const var_symbol *>(*i))
		{
			o << " " << bindings.find(dynamic_cast<const var_symbol*>(*i))->second->getName();
		}
		else
		{
			o << " " << dynamic_cast<const const_symbol*>(*i)->getName();
		};
	};
	o << ")";
};
