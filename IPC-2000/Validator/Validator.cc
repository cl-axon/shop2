#include "Validator.h"
#include "Action.h"
#include <iostream>
#include "typecheck.h"

Validator::~Validator()
{
	for(vector<Action*>::iterator i = actionRegistry.begin();i != actionRegistry.end();++i)
	{
		delete (*i);
	};

	Environment::collect(this);
};

bool Validator::step()
{
	/* What we need to do here:
	 * 	 - Identify the next happening.
	 * 	 - Apply the happening to the State.
	 */

	if(thisStep!=theplan.end())
	{
		return state.progress(*(thisStep++));
	};
	return false;
};

bool Validator::execute()
{
	while(thisStep != theplan.end() && step());

	return thisStep==theplan.end();
};

	
void Validator::displayPlan() const
{
	cout << "Plan to validate:\n\n" << theplan << "\n";
};

bool Validator::checkGoal(const goal * g)
{
	const Proposition * p = pf.buildProposition(g);
	bool b = p->evaluate(&state);
	p->destroy();
	return b;
};

bool Validator::durativePlan() const
{
	return durative;
};

int Validator::simpleLength() const
{
	if(stepLength) return stepcount;  // Default is step count.
	return theplan.length();	
};

double Validator::finalValue() const
{
	static Environment nullEnv;
	if(metric)
		return state.evaluate(metric->expr,nullEnv);
	if(stepLength) return stepcount;  // Default is step count.
	return theplan.length();	
};

vector<const_symbol*> Validator::range(const var_symbol * v)
{
	return typeC.range(v);
};
