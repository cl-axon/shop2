#include <vector>
#include <stl_function.h>
#include <algorithm>
#include <map>
#include <iostream>
#include "ptree.h"
#include "exceptions.h"
#include "main.h"
#include "Ownership.h"

#ifndef __PLAN
#define __PLAN

// Use the following switch if your compiler/STL doesn't use std.
// #define NO_STD_NAMESPACE

class State;
class Proposition;
class FuncExp;
class FuncExpFactory;
class Action;
class InvariantAction;
class CondCommunicationAction;


class Update {
private:
	const FuncExp * fe;
	assign_op aop;
	double value;
public:
	Update(const FuncExp * f,assign_op ao,double v) :
		fe(f), aop(ao), value(v)
	{};

	void update(State * s) const;
};

class EffectsRecord {
private:
	vector<const SimpleProposition *> adds;
	vector<const SimpleProposition *> dels;

	vector<Update> updates;
	// Also need something for the FEs.
public:
	void pushAdd(const SimpleProposition * p)
	{
		adds.push_back(p);
	};
	void pushDel(const SimpleProposition * p)
	{
		dels.push_back(p);
	};
	void addFEffect(const FuncExp * fe,assign_op aop,double value)
	{
		updates.push_back(Update(fe,aop,value));
	};
	
	void enact(State * s) const;
};



class Happening {
private:
	Validator * vld;

	double time;
	vector<const Action*> actions;

	Happening(Validator * v) :
		vld(v), time(0.0), actions()
	{}; 

public:
	friend class ExecutionContext;

	Happening(Validator * v,const vector<pair<double,Action*> > & as) : vld(v)
	{
		time = as.begin()->first;
		transform(as.begin(),as.end(),back_inserter(actions),select2nd<pair<double,Action*> >());
	};
	~Happening();
	
	void adjustContext(ExecutionContext &) const;

	double getTime() const {return time;};

	bool canHappen(const State * s) const;
	bool applyTo(State * s) const;

	void write(ostream & o) const;

};

struct ExecutionContext {
	Happening invariants;	// Also includes the temporal conditional effects monitors.
	
	ExecutionContext(Validator * v);	

	void addInvariant(const InvariantAction * a);
	void removeInvariant(const InvariantAction * a);
	void addCondAction(const CondCommunicationAction * ca);
	bool removeCondAction(const CondCommunicationAction * ca);
	void setTime(double t);
	const Happening * getInvariants() const {return &invariants;};
	bool hasInvariants() const;
	~ExecutionContext();
};

struct after {
	double time;
	double tolerance;
	
	after(double t,double tol) : time(t), tolerance(tol) {};

	bool operator()(const pair<double,Action*> p) const
	{
		return p.first > time + tolerance/10;
	};
};

class Plan {
public:
	typedef vector<pair<double,Action*> > timedActionSeq;
	
private:
	vector<Happening*> happenings;
	Validator * vld;

	struct planBuilder {
		Validator * vld;
		timedActionSeq & tas;
		const operator_list * ops;
		double defaultTime;
		timedActionSeq extras;

		planBuilder(Validator * v,timedActionSeq & ps,
						const operator_list * os) :
			vld(v), tas(ps), ops(os), defaultTime(1), extras() {};

		void handleDurativeAction(const durative_action *,const const_symbol_list *,double,double);
		
		void operator()(const plan_step * ps);
		
	};

public: 
	// What about durative actions here?

	Plan(Validator* v,const operator_list * ops,const plan * p);
	~Plan()
	{
		for(vector<Happening*>::const_iterator i = happenings.begin();
				i != happenings.end();++i)
		{
			delete (*i);
		};
	};

	Happening * lastHappening() const
	{
		if(happenings.size()==0) return 0;
		
		vector<Happening*>::const_iterator i = happenings.end();
		
		return *(--i);
	};
	
	friend class const_iterator : public 
#ifndef NO_STD_NAMESPACE
			std::forward_iterator<std::input_iterator_tag,const Happening *>
#endif
#ifdef NO_STD_NAMESPACE
			forward_iterator<input_iterator_tag,const Happening *>
#endif

	{
	private:
		const Plan * myPlan;
		double currenttime;		// currenttime will always be the time of the state prevailing.

		ExecutionContext ec;	// Records invariant checks.
		bool checkingInvariants; 
								// Records current status during execution.

		vector<Happening *>::const_iterator i;
								// The iterator always points at the next happening to be considered 
								// (if there is one), ignoring possible invariant checks.

	public:
		const_iterator(const Plan * p) : 
			myPlan(p), currenttime(0.0), ec(p->vld), checkingInvariants(false), i(p->happenings.begin())
		{
			if(i != p->happenings.end())
			{
				(*i)->adjustContext(ec);
			};
		};

		void toEnd()
		{
			i = myPlan->happenings.end();
			currenttime = myPlan->happenings.empty()?0:myPlan->lastHappening()->getTime();
			checkingInvariants = false;
			
		};

		bool operator ==(const const_iterator & c) const
		{
			return currenttime == c.currenttime && checkingInvariants == c.checkingInvariants;
		
		};

		const Happening * operator*() const
		{
			if(checkingInvariants)
				return ec.getInvariants();

			if(i != myPlan->happenings.end())
				return *i;
				
			return 0;
		};

		const_iterator & operator++()
		{
			if(i == myPlan->happenings.end()) return *this; // Throw?
			
			if(!checkingInvariants && ec.hasInvariants())
			{
				checkingInvariants = true;
				currenttime = (*i)->getTime();
				ec.setTime((currenttime+(*(i+1))->getTime())/2.0);
				return *this;
			};

			checkingInvariants = false;
			
			if(i != myPlan->happenings.end())
			{
				currenttime = ec.hasInvariants()?ec.getInvariants()->getTime():(*i)->getTime();
				++i;
			};
			
			if(i != myPlan->happenings.end()) 
			{
				(*i)->adjustContext(ec);
			};
			return *this;
		};

		const_iterator operator++(int)
		{
			const_iterator ii = *this;
			++(*this);
			return ii;
		}; 

	};

	const_iterator begin() const
	{
		return const_iterator(this);
	};

	const_iterator end() const
	{
		const_iterator c(this);
		c.toEnd();
		return c;
	};

	void display() const;
	int length() const;

};

ostream & operator <<(ostream & o,const Plan & p);
ostream & operator <<(ostream & o,const Happening * h);
#endif
