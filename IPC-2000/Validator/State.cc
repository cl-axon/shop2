#include "State.h"
#include "Plan.h"
#include "main.h"
#include "Validator.h"
#include "exceptions.h"

State::State(Validator * const v,const effect_lists* is) : 
		tolerance(v->getTolerance()), vld(v), time(0.0)
{
	for(list<simple_effect*>::const_iterator i = is->add_effects.begin();
		i != is->add_effects.end();++i)
	{
		logState[vld->pf.buildLiteral(*i)] = true;
	};

	for(pc_list<assignment*>::const_iterator i = is->assign_effects.begin();
		i != is->assign_effects.end();++i)
	{
		feValue[vld->fef.buildFuncExp((*i)->getFTerm())] 
				= dynamic_cast<const num_expression *>((*i)->getExpr())->double_value();
	};
};

bool State::evaluate(const SimpleProposition * p) const
{
	LogicalState::const_iterator i = logState.find(p);
	if(i != logState.end())
	{
		return i->second;
	}
	else
	{
		return false;
	};
};

double State::evaluate(const FuncExp * fe) const
{
	NumericalState::const_iterator i = feValue.find(fe);
	if(i!=feValue.end())
	{
		return i->second;
	}
	else
	{
		cerr << "Attempt to access undefined expression: " << *fe << "\n";
		// Throw?
		exit(0);
	};
};

double State::evaluate(const expression * e,const Environment & bs) const
{
	if(dynamic_cast<const div_expression *>(e))
	{
		return evaluate(dynamic_cast<const div_expression*>(e)->getLHS(),bs) /
				evaluate(dynamic_cast<const div_expression*>(e)->getRHS(),bs);
	};

	if(dynamic_cast<const minus_expression *>(e))
	{
		return evaluate(dynamic_cast<const minus_expression*>(e)->getLHS(),bs) -
				evaluate(dynamic_cast<const minus_expression*>(e)->getRHS(),bs);
	};

	if(dynamic_cast<const mul_expression *>(e))
	{
		return evaluate(dynamic_cast<const mul_expression*>(e)->getLHS(),bs) *
				evaluate(dynamic_cast<const mul_expression*>(e)->getRHS(),bs);
	};

	if(dynamic_cast<const plus_expression *>(e))
	{
		return evaluate(dynamic_cast<const plus_expression*>(e)->getLHS(),bs) +
				evaluate(dynamic_cast<const plus_expression*>(e)->getRHS(),bs);
	};

	if(dynamic_cast<const num_expression*>(e))
	{
		return dynamic_cast<const num_expression*>(e)->double_value();
	};
	
	if(dynamic_cast<const uminus_expression*>(e))
	{
		return -(evaluate(dynamic_cast<const uminus_expression*>(e)->getExpr(),bs));
	};

	if(dynamic_cast<const func_term*>(e))
	{
		const FuncExp * fexp = vld->fef.buildFuncExp(dynamic_cast<const func_term*>(e),bs);
		if(feValue.find(fexp) != feValue.end())
			return feValue.find(fexp)->second;

		if(Verbose) cout << "Attempt to inspect undefined value: " << *fexp << "\n";
		BadAccessError bae;
		throw bae;
	};

	if(const special_val_expr * sp = dynamic_cast<const special_val_expr *>(e))
	{
		if(sp->getKind() == E_TOTAL_TIME)
		{
				if(vld->durativePlan()) return time;
				return vld->simpleLength();
		};

		if(sp->getKind() == E_DURATION_VAR)
		{
			return bs.duration;
		};
		
		if(Verbose)
			cout << "Not yet handling use of #t expressions!\n";
		SyntaxTooComplex stc;
		throw stc;
	};

	UnrecognisedCondition uc;
	throw uc;
};

bool
State::progress(const Happening * h)
{
	if(h->canHappen(this))
	{
		time = h->getTime();
		return h->applyTo(this);
	}
	else return false;
};

void
State::add(const SimpleProposition * a)
{
	if(Verbose) cout << "Adding " << *a << "\n";
	logState[a] = true;
};

void
State::del(const SimpleProposition * a)
{
	if(Verbose) cout << "Deleting " << *a << "\n";
	logState[a] = false;
};

void 
State::update(const FuncExp * fe,assign_op aop,double value)
{
	if(Verbose) cout << "Updating " << *fe << " (" << feValue[fe] << ") by " << value << " ";
	switch(aop)
	{
		case E_ASSIGN:
			if(Verbose) cout << "assignment\n";
			feValue[fe] = value;
			return;
		case E_INCREASE:
			if(Verbose) cout << "increase\n";
			feValue[fe] += value;
			return;
		case E_DECREASE:
			if(Verbose) cout << "decrease\n";
			feValue[fe] -= value;
			return;
		case E_SCALE_UP:
			if(Verbose) cout << "scale up\n";
			feValue[fe] *= value;
			return;
		case E_SCALE_DOWN:
			if(Verbose) cout << "scale down\n";
			feValue[fe] /= value;
			return;
		default:
			return;
	};
};
