#include <vector>
#include "Action.h"
#include "Plan.h"
#include "State.h"

#ifndef __VALIDATOR
#define __VALIDATOR
#include "typecheck.h"

class plan;
class TypeChecker;

class Validator {
public:
	FuncExpFactory fef;
	PropositionFactory pf;
private:
	double tolerance;
	TypeChecker & typeC;
	
	const metric_spec * metric;

	int stepcount;
	bool stepLength;
	bool durative;
	vector<Action *> actionRegistry;

	Plan theplan;
	State state;


	Plan::const_iterator thisStep;

	bool step();

public:
	Validator(double tol,TypeChecker & tc,const operator_list * ops,const effect_lists * is,const plan * p,const metric_spec * m,
					bool lengthDefault,bool isDur) :
		fef(), pf(this), tolerance(tol), typeC(tc), metric(m), stepcount(p->size()), 
		stepLength(lengthDefault), durative(isDur), actionRegistry(),
		theplan(this,ops,p), state(this,is), thisStep(theplan.begin())
	{};
	~Validator();

	bool execute();
	bool checkGoal(const goal * g);
	double finalValue() const;
	int simpleLength() const;
	bool durativePlan() const;
	double getTolerance() const {return tolerance;};
	void registerAction(Action * a) {actionRegistry.push_back(a);};
	void displayPlan() const;
	
	vector<const_symbol*> range(const var_symbol * v);
};









#endif
