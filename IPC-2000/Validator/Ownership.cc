#include "Ownership.h"
#include "Validator.h"
#include "Action.h"
#include "FuncExp.h"
#include "Environment.h"
#include "Proposition.h"

bool 
Ownership::ownsForAdd(const Action * a,const SimpleProposition * p)
{
	if(propOwner.find(p) != propOwner.end())
	{
		if(propOwner[p].first != a)
		{
			// Different action wants to use the same proposition.
			if(Verbose) 
			{
				cout << "Mutex violation: " << a << " (adds " << *p << ")";
				if(propOwner[p].first) cout << " and " << propOwner[p].first;
				cout << "\n";
			};
			
			return false;
		};

		switch(propOwner[p].second)
		{
			case E_PPRE:
				if(Verbose)
				{
					cout << "WARNING: " << *a << " adds a precondition literal " 
							<< *p << "\n";
				};
			case E_PRE:
			case E_NPRE:
				propOwner[p].second = E_ADD;
				return true;
			case E_DEL:
				// Action deletes and adds the same literal.
				if(Verbose)
				{
					cout << "WARNING: " << *a << " adds and deletes the literal " 
							<< *p << "\n";
				};
				return true;
			case E_ADD:
				// Action adds the same literal twice.
				if(Verbose)
				{
					cout << "WARNING: " << *a << " adds literal " << *p << " twice\n";
				};
				return true;
			default:
				return false;
		};
	}
	else
	{
		propOwner[p] = make_pair(a,E_ADD);
		return true;
	};
};

bool 
Ownership::ownsForDel(const Action * a,const SimpleProposition * p)
{
	if(propOwner.find(p) != propOwner.end())
	{
		if(propOwner[p].first != a)
		{
			// Different action wants to use the same proposition.
			if(Verbose) 
			{
				cout << "Mutex violation: " << a << " (deletes " << *p << ")";
				if(propOwner[p].first) cout << " and " << propOwner[p].first;
				cout << "\n";
			};
			return false;
		};

		switch(propOwner[p].second)
		{
			case E_NPRE:
				if(Verbose)
				{
					cout << "WARNING: " << *a << " deletes a false precondition literal " 
							<< *p << "\n";
				};
			case E_PPRE:
			case E_PRE:
				propOwner[p].second = E_DEL;
				return true;
			case E_DEL:
				// Action deletes the same literal twice.
				if(Verbose)
				{
					cerr << "WARNING: " << *a << " deletes the literal " 
							<< *p << " twice\n";
				};
				return true;
			case E_ADD:
				// Action adds and deletes the same literal.
				if(Verbose)
				{
					cerr << "WARNING: " << *a << " adds and deletes the literal " 
								<< *p << "\n";
				};
				return true;
			default:
				return false;
		};
	}
	else
	{
		propOwner[p] = make_pair(a,E_DEL);
		return true;
	};
};

bool Ownership::markOwnedPrecondition(const Action * a,const SimpleProposition * p,ownership o)
{

	if(propOwner.find(p) != propOwner.end())
	{
		if(propOwner[p].second == E_PRE || propOwner[p].second == E_NPRE
				|| propOwner[p].second == E_PPRE)
		{
			if(propOwner[p].first != a) propOwner[p] = make_pair(static_cast<const Action *>(0),o);
			return true;
		}
		else
		{
			if(Verbose && propOwner[p].first != a)
			{
				cout << "Mutex violation: " << a << " (requires " << p << ")\n";
			};
			return propOwner[p].first == a;
		};
	}
	else
	{
		propOwner[p] = make_pair(a,o);
		return true;
	};
};

bool Ownership::markOwnedPreconditionFEs(const Action * a,const expression * e,const Environment & bs)
{	
	if(dynamic_cast<const num_expression *>(e)) return true;
	if(const func_term * fe = dynamic_cast<const func_term *>(e))
	{
		const FuncExp * fexp = vld->fef.buildFuncExp(fe,bs);
		if(FEOwner.find(fexp) != FEOwner.end())
		{
			if(FEOwner[fexp].first == a)
				return true;
				
			if(FEOwner[fexp].second == E_PRE)
			{
				FEOwner[fexp] = make_pair(static_cast<const Action *>(0),E_PRE);
				return true;
			}
			else
			{
				if(Verbose) cout << "Mutex violation: " << a << " (requires " << *fexp << ")\n";
				
				return false;
			};
		}
		else
		{
			FEOwner[fexp] = make_pair(a,E_PRE);
			return true;
		};
	};
	if(const binary_expression * bexp = dynamic_cast<const binary_expression *>(e))
	{
		return markOwnedPreconditionFEs(a,bexp->getLHS(),bs) &&
				markOwnedPreconditionFEs(a,bexp->getRHS(),bs);
	};
	if(const uminus_expression * uexp = dynamic_cast<const uminus_expression *>(e))
	{
		return markOwnedPreconditionFEs(a,uexp->getExpr(),bs);
	};
	if(dynamic_cast<const special_val_expr *>(e))
	{
		return true;
	};

	if(Verbose) cout << "Unrecognised expression type\n";
	UnrecognisedCondition uc;
	throw uc;
	
};

bool Ownership::markOwnedEffectFE(const Action * a,const FuncExp * fe,assign_op aop,
								const expression * e,const Environment & bs)
{
	if(!markOwnedPreconditionFEs(a,e,bs))
	{
		if(Verbose) 
			cout << "Conflict over ownership of resource: " << *fe << " wanted by " << a << "\n";
		return false;
	};
	
	if(FEOwner.find(fe) != FEOwner.end())
	{
		if(FEOwner[fe].first != a)
		{
			if(FEOwner[fe].second == E_ADD && (aop == E_INCREASE || aop == E_DECREASE))
			{
				// Commuting additive assignments are OK.
				FEOwner[fe].first = static_cast<const Action *>(0);
				return true;
			};
			
			// Different action wants to use the same FE for conflicting uses.
			if(Verbose) 
			{
				cout << "Mutex violation: " << a << " (expression " << *fe << ")";
				if(FEOwner[fe].first) cout << " and " << FEOwner[fe].first;
				cout << "\n";
			};
			return false;
		};

		switch(FEOwner[fe].second)
		{
			case E_PRE:
				if(aop == E_INCREASE || aop == E_DECREASE)
				{
					FEOwner[fe].second = E_ADD;
				}
				else
				{
					FEOwner[fe].second = E_ASSIGNMENT;
				};
				return true;
				
			case E_ADD:
				// Action deletes the same literal twice.
				
				if(aop != E_INCREASE && aop != E_DECREASE)
				{
					if(Verbose) cout << *a << " both assigns to and updates expression " << *fe << "\n";
					return false;
				};

				if(Verbose)
				{
					cout << "WARNING: " << *a << " assigns to expression " 
							<< *fe << "twice\n";
				};
				return true;
				
			case E_ASSIGNMENT:
				// Action assigns to expression twice.
				if(Verbose) 
					cout << *a << " assigns to expression " << *fe << " twice\n";
				
				return false;
			default:
				if(Verbose)
					cout << "Unknown expression type in " << a << "\n";
				UnrecognisedCondition uc;
				throw uc;
		};
	}
	else
	{
		FEOwner[fe] = make_pair(a,(aop==E_INCREASE || aop==E_DECREASE)?E_ADD:E_ASSIGNMENT);
		return true;
	};	
};
