#ifndef __OWNERSHIP
#define __OWNERSHIP

#include <map>
#include "ptree.h"

class Validator;
class Action;
class FuncExp;
class Environment;
class SimpleProposition;
class expression;


enum ownership {E_PRE,E_PPRE,E_NPRE,E_ADD,E_DEL,E_ASSIGNMENT};

class Ownership {
private:
	map<const SimpleProposition *,pair<const Action *,ownership> > propOwner;

	Validator * vld;
	map<const FuncExp *,pair<const Action *,ownership> > FEOwner;

public:
	Ownership(Validator * v) : vld(v) {};

	bool markOwnedPrecondition(const Action * a,const SimpleProposition * p,ownership o);
	bool markOwnedPreconditionFEs(const Action * a,const expression * e,const Environment & bs);
	bool ownsForAdd(const Action * a,const SimpleProposition * p);
	bool ownsForDel(const Action * a,const SimpleProposition * p);
	bool markOwnedEffectFE(const Action * a,const FuncExp * fe,assign_op aop,
								const expression * e,const Environment & bs);
	
};



#endif
