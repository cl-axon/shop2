#include "typecheck.h"
#include "ptree.h"
#include <functional>
#include "main.h"
#include "exceptions.h"


TypeHierarchy::TypeHierarchy(const analysis * a)
{
	if(!a || !a->the_domain) 
	{
		ParseFailure pf;
		throw(pf);
	};
	
	if(!a->the_domain->types) return;
	for(pddl_type_list::const_iterator i = a->the_domain->types->begin();
			i != a->the_domain->types->end();++i)
	{
		if((*i)->type)
		{
			add(PTypeRef(*i),PTypeRef((*i)->type));
		}
		else
		{	
			if((*i)->either_types)
			{
				for(pddl_type_list::const_iterator j = (*i)->either_types->begin();
					j != (*i)->either_types->end();++j)
						add(PTypeRef(*i),PTypeRef(*j));
			}
			else
			{
				PTypeRef pt(*i);
				Graph::iterator j = graph.find(&pt);
				if(j == graph.end())
				{
					graph[pt.clone()] = set<const TypeRef*>();
				};
			};
		};
	};
};

TypeHierarchy::~TypeHierarchy()
{
	for(Graph::const_iterator i = graph.begin();i != graph.end();++i)
	{
		delete (i->first);
	};
};




/* gi is the Graph::iterator starting point aiming for t.
 * vs are the places visited so far.
 * gs is the original starting point.
 */

typedef set<const TypeRef *> Nodes;

bool TypeHierarchy::closure(GI & gi,Nodes & vs,GI & gs,const TypeRef * t)
{	 
	if(gi == graph.end()) 
	{
		return false;
	};
	if(*(gi->first) == *t) 
	{
		gs->second.insert(vs.begin(),vs.end());
		return true;
	};

	for(Nodes::iterator n = gi->second.begin();n != gi->second.end();++n)
	{
		if(vs.find(*n) == vs.end())
		{
			vs.insert(*n);
			GI tmp = graph.find(*n);
			if(closure(tmp,vs,gs,t)) return true;
		};
	};
	return false;
};

			

void UTypeRef::addContents(TypeHierarchy * th) const
{
	for(set<const pddl_type*>::const_iterator i = pts.begin();
				i != pts.end();++i)
	{
		th->add(PTypeRef(*i),*this);
	};
};

bool TypeHierarchy::reachable(const TypeRef & t1,const TypeRef & t2)
{
	if(t1 == t2) return true;

	Graph::iterator i = graph.find(&t1);
	if(i == graph.end()) 
	{
		return false;
	};
	
	Graph::const_iterator j = graph.find(&t2);
	if(j == graph.end() && t2.expected()) 
	{
		return false;
	};
	t2.addContents(this);
	j = graph.find(&t2);
	
	if(i->second.find(j->first) != i->second.end()) return true;

	Nodes ns;
	return closure(i,ns,i,j->first);
};

void TypeHierarchy::add(const PTypeRef & t1,const TypeRef & t2)
{
	Graph::const_iterator i = graph.find(&t1);
	Graph::const_iterator j = graph.find(&t2);
	if(j == graph.end())
	{
		graph[t2.clone()] = set<const TypeRef*>();	
		j = graph.find(&t2);
	};
	if(i == graph.end())
	{
		graph[t1.clone()] = set<const TypeRef*>(); 
	};
	graph[&t1].insert(j->first);

};

struct badchecker {
	TypeChecker * thea;
	
	badchecker(TypeChecker * a) : thea(a) {};

	bool operator() (const operator_ * a) const
	{
		return !thea->typecheckAction(a);
	};
	bool operator() (const goal * g) const
	{
		return !thea->typecheckGoal(g);
	};
	bool operator() (const plan_step * p)
	{
		return !thea->typecheckActionInstance(p);
	};
	bool operator() (const effect * e)
	{
		return !thea->typecheckEffect(e);
	};
};


struct matchFunc {
	const func_symbol * f;

	matchFunc(const func_symbol * fs) : f(fs) {};

	bool operator() (const func_decl * fd) const
	{
		if(!fd)
		{
			if(Verbose) cout << "Problematic function declaration!\n";
			ParseFailure pe;
			throw(pe);
		};
		const func_symbol * fdf = fd->getFunction();
		if(!fdf)
		{
			if(Verbose) cout << *fd << " problematic function declaration!\n";
			ParseFailure pe;
			throw(pe);
		};

		return f == fdf;
	};
};

struct matchPred {
	const pred_symbol * p;

	matchPred(const pred_symbol * ps) : p(ps) {};

	bool operator() (const pred_decl * pd) const
	{
		return p == pd->getPred();
	};
};

struct matchOp {
	const operator_symbol * f;

	matchOp(const operator_symbol * fs) : f(fs) {};

	bool operator() (const operator_ * op) const
	{
		return f == op->name;
	};
};

/* What does it all mean?
 * 
 * Let c be a constant symbol, v a variable symbol and t be a type symbol.
 * 
 * t1 - t2 means t1 is a subset of t2.
 * We allow multiple subset relations (multiple inheritance).
 * c - t means c is a member of t.
 * v - t means instantiations of v must be of type t.
 * 
 * Therefore, subType(c - t1,v - t2) iff t1 - t2. Note that this relationship might 
 * be via a path of transitive subset relations.
 * 
 * v - (either t1 t2) is equivalent to v - (t1 U t2).
 * c - (either t1 t2) is not given a meaning, but we use the either field to record
 *                    multiple types for constants. Thus, c - t1 and c - t2 is legal
 *                    and is recorded internally using c - (either t1 t2). The meaning
 *                    of the multiple declarations is that c is of both types t1 and t2,
 *                    so can be substituted for any variable of either type. 
 * t - (either t1 t2) is also not given a meaning (although the current parser will
 * 		actually interpret it as equivalent to "t - t1 t - t2").
 */


bool TypeChecker::subType(const pddl_typed_symbol * tp1,const pddl_typed_symbol * tp2)
{
	if(!isTyped) return true;
	if(tp1->type)
	{
		if(tp2->type)
		{
			return th.reachable(PTypeRef(tp1->type),PTypeRef(tp2->type));
		}
		else
		{
			if(tp2->either_types)
			{
				return th.reachable(PTypeRef(tp1->type),UTypeRef(tp2->either_types));
			};
			if(Verbose) cout << tp2->getName() << " has bad type definition\n";
			TypeException te;
			throw(te);
		};		
	}
	else
	{
		if(!tp1->either_types) 
		{
			if(Verbose) cout << "Object with unknown type: " << tp1->getName() << "\n";
			TypeException te;
			throw(te);
		};

		// The situation is now complicated by the fact that variables and constants
		// must be treated differently. Constants have either types representing 
		// conjunctions of types, while variables use them for disjunctions. 

		if(!tp1->either_types)
		{
			if(Verbose) cout << tp1->getName() << " badly typed!\n";
			TypeException te;
			throw(te);
		};
		
		if(dynamic_cast<const const_symbol*>(tp1))
		{
			// The following confirms that a constant is a subtype of a given type by
			// checking that one of its types is appropriate. 

			
			for(pddl_type_list::const_iterator i = tp1->either_types->begin();
						i != tp1->either_types->end();++i)
			{
				if(subType(*i,tp2)) return true;
			};
			return false;
		}
		else
		{
		
			for(pddl_type_list::const_iterator i = tp1->either_types->begin();
						i != tp1->either_types->end();++i)
			{
				if(!subType(*i,tp2)) return false;
			};
			return true;
		};
	};
};

bool TypeChecker::subType(const pddl_type * t,const pddl_typed_symbol * s)
{
	if(!isTyped) return true;
	if(s->type)
	{
		return th.reachable(PTypeRef(t),PTypeRef(s->type));
	};

	if(!s->either_types)
	{
		if(Verbose) cout << s->getName() << " badly typed!\n";
		TypeException te;
		throw(te);
	};

	return th.reachable(PTypeRef(t),UTypeRef(s->either_types));
};

bool TypeChecker::subType(const pddl_type * t1,const pddl_type * t2)
{
	if(!isTyped) return true;

	if(!t1 || !t2)
	{
		TypeException te;
		throw(te);
	};
	
	return th.reachable(PTypeRef(t1),PTypeRef(t2));
};
	
bool TypeChecker::typecheckProposition(const proposition * p)
{
	if(!isTyped) return true;
	pred_decl_list::const_iterator prd = 
		find_if(thea->the_domain->predicates->begin(),thea->the_domain->predicates->end(),
				matchPred(p->head));
	if(prd==thea->the_domain->predicates->end())
	{
		if(p->head->getName()=="=") return true;
		if(Verbose) cout << "Predicate " << p->head->getName() << " not found\n";
		return false;
	};
	var_symbol_list::const_iterator arg = (*prd)->getArgs()->begin();
	parameter_symbol_list::const_iterator e = p->args->end();
	for(parameter_symbol_list::const_iterator i = p->args->begin();i != e;++i,++arg)
	{
		if(!subType(*i,*arg)) 
		{
			if(Verbose) cout << "Type problem in " << *p << "\n";
			return false;
		};
	};
	return true;
};

bool TypeChecker::typecheckFuncTerm(const func_term * ft)
{
	if(!isTyped) return true;
	func_decl_list::const_iterator fd = 
		find_if(thea->the_domain->functions->begin(),thea->the_domain->functions->end(),
				matchFunc(ft->getFunction()));
	if(fd==thea->the_domain->functions->end())
		return false;
	var_symbol_list::const_iterator arg = (*fd)->getArgs()->begin();
	parameter_symbol_list::const_iterator e = ft->getArgs()->end();
	for(parameter_symbol_list::const_iterator i = ft->getArgs()->begin();
			i != e;++i,++arg)
	{
		if(!subType(*i,*arg)) 
		{
			if(Verbose) cout << "Type problem in function term " << *ft << "\n";
			return false;
		};
	};
	return true;
};

bool TypeChecker::typecheckActionInstance(const plan_step * p)
{
	if(!isTyped) return true;
	operator_list::const_iterator op = 
		find_if(thea->the_domain->ops->begin(),thea->the_domain->ops->end(),
				matchOp(p->op_sym));
	if(op==thea->the_domain->ops->end())
		return false;
	var_symbol_list::const_iterator arg = (*op)->parameters->begin();
	const_symbol_list::const_iterator e = p->params->end();
	for(const_symbol_list::const_iterator i = p->params->begin();
			i != e;++i,++arg)
	{
		if(!subType(*i,*arg))
		{
			if(Verbose) cout << "Type problem in action " << *p << "\n";
			return false;
		};
	};
	return true;
};

bool TypeChecker::typecheckExpression(const expression * exp)
{
	if(!isTyped) return true;
	if(const binary_expression * be = dynamic_cast<const binary_expression *>(exp))
	{
		return typecheckExpression(be->getLHS()) && typecheckExpression(be->getRHS());
	};
	if(const uminus_expression * ue = dynamic_cast<const uminus_expression *>(exp))
	{
		return typecheckExpression(ue->getExpr());
	};
	if(const func_term * ft = dynamic_cast<const func_term *>(exp))
	{
		return typecheckFuncTerm(ft);
	};
	return true;
};
		

bool TypeChecker::typecheckGoal(const goal * g)
{
	if(!isTyped) return true;
	if(const simple_goal * sg = dynamic_cast<const simple_goal *>(g))
	{
		return typecheckProposition(sg->getProp());
	};
	if(const conj_goal * cg = dynamic_cast<const conj_goal *>(g))
	{
		if(cg->getGoals()->end() == find_if(cg->getGoals()->begin(),cg->getGoals()->end(),badchecker(this)))
			return true;
		return false;
	};
	if(const disj_goal * dg = dynamic_cast<const disj_goal *>(g))
	{
		if(dg->getGoals()->end() == find_if(dg->getGoals()->begin(),dg->getGoals()->end(),badchecker(this)))
			return true;
		return false;
	};
	if(const imply_goal * ig = dynamic_cast<const imply_goal *>(g))
	{
		return typecheckGoal(ig->getAntecedent()) && typecheckGoal(ig->getConsequent());
	};
	if(const neg_goal * ng = dynamic_cast<const neg_goal *>(g))
	{
		return typecheckGoal(ng->getGoal());
	};
	if(const timed_goal * tg = dynamic_cast<const timed_goal *>(g))
	{
		return typecheckGoal(tg->getGoal());
	};
	if(const qfied_goal * qg = dynamic_cast<const qfied_goal *>(g))
	{
		return typecheckGoal(qg->getGoal());
	};
	if(const comparison * c = dynamic_cast<const comparison *>(g))
	{
		return typecheckExpression(c->getLHS()) && typecheckExpression(c->getRHS());
	};
	return false;
};

bool TypeChecker::typecheckEffect(const effect * e)
{
	if(!isTyped) return true;
	if(const simple_effect * se = dynamic_cast<const simple_effect *>(e))
	{
		return typecheckProposition(se->prop);
	};
	if(const cond_effect * ce = dynamic_cast<const cond_effect *>(e))
	{
		return typecheckGoal(ce->getCondition()) &&
				typecheckEffects(ce->getEffects());
	};
	if(const forall_effect * fe = dynamic_cast<const forall_effect *>(e))
	{
		return typecheckEffects(fe->getEffects());
	};
	if(const timed_effect * te = dynamic_cast<const timed_effect *>(e))
	{
		return typecheckEffects(te->effs);
	};
	if(const assignment * ass = dynamic_cast<const assignment *>(e))
	{
		return typecheckFuncTerm(ass->getFTerm()) && typecheckExpression(ass->getExpr());
	};
	return false;
};

bool TypeChecker::typecheckEffects(const effect_lists * es)
{
	if(!isTyped) return true;
	return 
		(es->add_effects.end() == find_if(es->add_effects.begin(),es->add_effects.end(),badchecker(this)))
			&&
		(es->del_effects.end() == find_if(es->del_effects.begin(),es->del_effects.end(),badchecker(this)))
			&&
		(es->forall_effects.end() == find_if(es->forall_effects.begin(),es->forall_effects.end(),badchecker(this)))
			&&
		(es->cond_effects.end() == find_if(es->cond_effects.begin(),es->cond_effects.end(),badchecker(this)))
			&&
		(es->assign_effects.end() == find_if(es->assign_effects.begin(),es->assign_effects.end(),badchecker(this)))
			&&
		(es->timed_effects.end() == find_if(es->timed_effects.begin(),es->timed_effects.end(),badchecker(this)));
};

bool TypeChecker::typecheckAction(const operator_ * act)
{
	if(!isTyped) return true;
	if(Verbose) cout << "Type-checking " << act->name->getName() << "\n";
	if(!typecheckGoal(act->precondition)) return false;
	if(!typecheckEffects(act->effects)) return false;
	if(const durative_action * da = dynamic_cast<const durative_action *>(act))
	{
		return typecheckGoal(da->dur_constraint);
	};
	
	return true;
};

bool TypeChecker::typecheckDomain()
{
	if(!isTyped) return true;
	return thea->the_domain->ops->end() == 
				find_if(thea->the_domain->ops->begin(),
						thea->the_domain->ops->end(),badchecker(this));
};

bool TypeChecker::typecheckProblem()
{
	if(!isTyped) return true;
	if(!thea || !thea->the_problem || !thea->the_problem->the_goal) 
	{
		ParseFailure pf;
		throw(pf);
	};
	
	return typecheckGoal(thea->the_problem->the_goal) &&
			typecheckEffects(thea->the_problem->initial_state);
};

bool TypeChecker::typecheckPlan(const plan * p)
{
	if(!isTyped) return true;
	if(!p) 
	{
		ParseFailure pf;
		throw(pf);
	};
	return p->end() == find_if(p->begin(),p->end(),badchecker(this));
};

vector<const_symbol *> TypeChecker::range(const var_symbol * v) 
{
	vector<const_symbol *> l;
	for(const_symbol_table::const_iterator i = thea->const_tab.begin();
			i != thea->const_tab.end();++i)
	{
		if(subType(i->second,v)) l.push_back(i->second);
	};
	
	return l;
};


