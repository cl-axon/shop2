#include <iostream>
#include "ptree.h"
#include "Environment.h"

#ifndef __FUNCEXP
#define __FUNCEXP

class State;


class FuncExp {
private:
	const Environment & bindings;
	const func_term * fe;

public:
	FuncExp(const func_term * f,const Environment &bs) :
		bindings(bs), fe(f)
	{};

	double evaluate(const State * s) const;

	void write(ostream & o) const 
	{
		o << "(" << fe->getFunction()->getName();
		for(parameter_symbol_list::const_iterator i = fe->getArgs()->begin();
				i != fe->getArgs()->end();++i)
		{
			if(dynamic_cast<const var_symbol *>(*i))
			{
				o << " " << bindings.find(dynamic_cast<const var_symbol *>(*i))->second->getName();
			}
			else
			{
				o << " " << (*i)->getName();
			};
		};
		o << ")";
	};	
};

ostream & operator <<(ostream & o,const FuncExp & fe);



class FuncExpFactory {
private:
	static Environment nullEnv;
	map<string,const FuncExp *> funcexps;
public:
	const FuncExp * buildFuncExp(const func_term * f)
	{
		string s(f->getFunction()->getName());
		for(parameter_symbol_list::const_iterator i = f->getArgs()->begin();
					i != f->getArgs()->end();++i)
		{
			s += (*i)->getName();
		};
		map<string,const FuncExp*>::const_iterator i = funcexps.find(s);
		if(i != funcexps.end())
			return i->second;
		const FuncExp * p = funcexps[s] = new FuncExp(f,nullEnv);
		return p;
	};
	const FuncExp * buildFuncExp(const func_term * f,const Environment & bs)
	{
		string s(f->getFunction()->getName());
		for(parameter_symbol_list::const_iterator i = f->getArgs()->begin();
					i != f->getArgs()->end();++i)
		{
			if(dynamic_cast<const var_symbol*>(*i))
			{
				s += bs.find(dynamic_cast<const var_symbol*>(*i))->second->getName();
			}
			else
			{
				s += (*i)->getName();
			};
		};
		map<string,const FuncExp*>::const_iterator i = funcexps.find(s);
		if(i != funcexps.end())
			return i->second;
		const FuncExp * p = funcexps[s] = new FuncExp(f,bs);
		return p;
	};
	~FuncExpFactory();
};	





#endif
