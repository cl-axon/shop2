#include <map>
#include <vector>
class var_symbol;
class const_symbol;
class Validator;

#ifndef __MYENVIRONMENT
#define __MYENVIRONMENT

struct Environment : public map<const var_symbol*,const const_symbol*> {
	static map<Validator*,vector<Environment *> > copies;

	double duration;

	Environment * copy(Validator * v) const
	{
		Environment * e = new Environment(*this);
		copies[v].push_back(e);
		return e;
	};

	static void collect(Validator * v)
	{
		for(vector<Environment *>::iterator i = copies[v].begin();i != copies[v].end();++i)
			delete *i;
		copies[v].clear();
	};
};

#endif
