#include "Proposition.h"
#include "FuncExp.h"

class Validator;
class Happening;
#ifndef __STATE
#define __STATE

typedef map<const SimpleProposition*,bool> LogicalState;
typedef map<const FuncExp*,double> NumericalState;


class State {
private:
	const double tolerance;

	Validator * const vld;

	LogicalState logState;
	NumericalState feValue;
	
	double time;

public:
	State(Validator * const v,const effect_lists* is);

	const double getTolerance() const {return tolerance;};

	bool progress(const Happening * h);

	bool evaluate(const SimpleProposition * p) const;
	double evaluate(const FuncExp * fe) const;
	double evaluate(const expression * e,const Environment & bs) const;

	void add(const SimpleProposition *);
	void del(const SimpleProposition *);
	void update(const FuncExp * fe,assign_op aop,double value);
};









#endif
