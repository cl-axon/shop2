#include "FuncExp.h"
#include "State.h"


double
FuncExp::evaluate(const State * s) const 
{
	return s->evaluate(this);
};

ostream & operator <<(ostream & o,const FuncExp & fe) 
{
	fe.write(o);
	return o;
};

Environment FuncExpFactory::nullEnv;

FuncExpFactory::~FuncExpFactory()
{
	for(map<string,const FuncExp*>::const_iterator i = funcexps.begin();i != funcexps.end();++i)
		delete (i->second);
};
