
/*-----------------------------------------------------------------------------
  Member functions for parse tree classes

  $Date: 2004/09/18 16:45:07 $
  $Revision: 1.1 $

  s.n.cresswell@durham.ac.uk
  July 2001.

  Durham Planning Group
  http://www.dur.ac.uk/~dcs0www/research/stanstuff/planpage.html
 ----------------------------------------------------------------------------
*/


#include "ptree.h"
#include "macros.h"
#include "DebugWriteController.h"
#include <memory>

/*-----------------------------------------------------------------------------
  Parse category
  ---------------------------------------------------------------------------*/

auto_ptr<WriteController> parse_category::wcntr = auto_ptr<WriteController>(new DebugWriteController);

void parse_category::setWriteController(auto_ptr<WriteController> w) {wcntr = w;};

void parse_category::display(int ind) const
{
    TITLE(parse_category);
}


ostream & operator <<(ostream & o,const parse_category & p)
{
	p.write(o);
	return o;
};


/*-----------------------------------------------------------------------------
  ---------------------------------------------------------------------------*/

void symbol::display(int ind) const 
{ 
    TITLE(symbol);
    LEAF(name); 
}

void symbol::write(ostream & o) const
{
	wcntr->write_symbol(o,this);
};

void pddl_typed_symbol::display(int ind) const
{ 
    TITLE(symbol);
    LEAF(name); 
    FIELD(type);
    FIELD(either_types);
}

void pddl_typed_symbol::write(ostream & o) const
{
	wcntr->write_pddl_typed_symbol(o,this);
};

void const_symbol::write(ostream & o) const
{
	wcntr->write_const_symbol(o,this);
};

/*-----------------------------------------------------------------------------
  ---------------------------------------------------------------------------*/

void pred_decl_list::write(ostream & o) const
{
	wcntr->write_pred_decl_list(o,this);
};

void func_decl_list::write(ostream & o) const
{
	wcntr->write_func_decl_list(o,this);
};

void var_symbol::write(ostream & o) const 
{
	wcntr->write_var_symbol(o,this);
};


/*-----------------------------------------------------------------------------
  ---------------------------------------------------------------------------*/

void plus_expression::display(int ind) const
{
    TITLE(plus_expression);
    FIELD(arg1);
    FIELD(arg2);
}

void plus_expression::write(ostream & o) const
{
	wcntr->write_plus_expression(o,this);
};

void minus_expression::display(int ind) const
{
    TITLE(minus_expression);
    FIELD(arg1);
    FIELD(arg2);
}

void minus_expression::write(ostream & o) const
{
	wcntr->write_minus_expression(o,this);
};

void mul_expression::display(int ind) const
{
    TITLE(mul_expression);
    FIELD(arg1);
    FIELD(arg2);
}

void mul_expression::write(ostream & o) const
{
	wcntr->write_mul_expression(o,this);
};

void div_expression::display(int ind) const
{
    TITLE(div_expression);
    FIELD(arg1);
    FIELD(arg2);
}

void div_expression::write(ostream & o) const
{
	wcntr->write_div_expression(o,this);
};

void uminus_expression::display(int ind) const
{
    TITLE(uminus_expression);
    FIELD(arg1);
}

void uminus_expression::write(ostream & o) const
{
	wcntr->write_uminus_expression(o,this);
};

void int_expression::display(int ind) const
{
    TITLE(int_expression);
    LEAF(val);
}

void int_expression::write(ostream & o) const
{
	wcntr->write_int_expression(o,this);
};

void float_expression::display(int ind) const
{
    TITLE(int_expression);
    LEAF(val);
}

void float_expression::write(ostream & o) const
{
	wcntr->write_float_expression(o,this);
};

void special_val_expr::display(int ind) const
{
    TITLE(special_val_expr);
    if (var==E_HASHT)
	cout << "hasht";
    else if (var==E_DURATION_VAR)
	cout << "?duration";
    else 
	cout << "?? ";
}

void special_val_expr::write(ostream & o) const
{
	wcntr->write_special_val_expr(o,this);
};

void func_term::display(int ind) const
{
    TITLE(func_term);
    FIELD(func_sym);
    FIELD(param_list);
}

void func_term::write(ostream & o) const
{
	wcntr->write_func_term(o,this);
};

void assignment::display(int ind) const
{
    TITLE(assignment);
    LEAF(op);
    FIELD(f_term);
    FIELD(expr);
}

void assignment::write(ostream & o) const
{
	wcntr->write_assignment(o,this);
};

void goal_list::display(int ind) const
{
    TITLE(goal_list);
    for(const_iterator i=begin(); i!=end(); ++i)
	ELT(*i);
}

void goal_list::write(ostream & o) const
{
	wcntr->write_goal_list(o,this);
};

void simple_goal::display(int ind) const
{
/*
    if (plrty == E_POS)
	cout << '+';
    else
	cout << '-';
*/
    FIELD(prop);
};

void simple_goal::write(ostream & o) const
{
	wcntr->write_simple_goal(o,this);
};

void qfied_goal::display(int ind) const
{
    TITLE(qfied_goal);
    
    LABEL(qfier);
    if (qfier == E_FORALL)
	cout << "forall";
    else if (qfier == E_EXISTS)
	cout << "exists";
    else 
	cout << "?quantifier";

    FIELD(vars);
    FIELD(gl);

    cout << ')';
}

void qfied_goal::write(ostream & o) const
{
	wcntr->write_qfied_goal(o,this);
};

void conj_goal::display(int ind) const
{
    TITLE(conj_goal);
    FIELD(goals);
}

void conj_goal::write(ostream & o) const
{
	wcntr->write_conj_goal(o,this);
};

void disj_goal::display(int ind) const
{
    TITLE(disj_goal);
    FIELD(goals);
}

void disj_goal::write(ostream & o) const
{
	wcntr->write_disj_goal(o,this);
};

void timed_goal::display(int ind) const
{
    TITLE(timed_goal);
    LEAF(ts);
    FIELD(gl);
}

void timed_goal::write(ostream & o) const
{
	wcntr->write_timed_goal(o,this);
};

void imply_goal::display(int ind) const
{
    TITLE(imply_goal);
    FIELD(lhs);
    FIELD(rhs);
}

void imply_goal::write(ostream & o) const
{
	wcntr->write_imply_goal(o,this);
};

void neg_goal::display(int ind) const
{
    TITLE(neg_goal);
    FIELD(gl);
}

void neg_goal::write(ostream & o) const
{
	wcntr->write_neg_goal(o,this);
};

void comparison::display(int ind) const
{
    TITLE(comparison);
    LEAF(op);
    FIELD(arg1);
    FIELD(arg2);
}

void comparison::write(ostream & o) const
{
	wcntr->write_comparison(o,this);
};

void proposition::display(int ind) const
{
    TITLE(prop);
    FIELD(head);
    FIELD(args);
};

void proposition::write(ostream & o) const
{
	wcntr->write_proposition(o,this);
};

void pred_decl::display(int ind) const
{
    TITLE(pred_decl);
    FIELD(head);
    FIELD(args);
};

void pred_decl::write(ostream & o) const
{
	wcntr->write_pred_decl(o,this);
};

void func_decl::display(int ind) const
{
    TITLE(pred_decl);
    FIELD(head);
    FIELD(args);
};

void func_decl::write(ostream & o) const
{
	wcntr->write_func_decl(o,this);
};


void simple_effect::display(int ind) const
{
    TITLE(simple_effect);
    FIELD(prop);
}

void simple_effect::write(ostream & o) const
{
	wcntr->write_simple_effect(o,this);
};

void forall_effect::display(int ind) const
{
    TITLE(forall_effect);
    FIELD(operand);
//    FIELD(var_tab);
}

void forall_effect::write(ostream & o) const
{
	wcntr->write_forall_effect(o,this);
};


void cond_effect::display(int ind) const
{
    TITLE(cond_effect);
    FIELD(cond);
    FIELD(effects);
}

void cond_effect::write(ostream & o) const
{
	wcntr->write_cond_effect(o,this);
};

void timed_effect::display(int ind) const
{
    TITLE(timed_effect);
    LEAF(ts);
    FIELD(effs);
}

void timed_effect::write(ostream & o) const
{
	wcntr->write_timed_effect(o,this);
};


/*-----------------------------------------------------------------------------
  effect_lists
  ---------------------------------------------------------------------------*/

void effect_lists::append_effects(effect_lists* from)
{
    // Splice lists in 'from' into lists in 'this'
    add_effects.splice(
	add_effects.begin(),
	from->add_effects);
    del_effects.splice(
	del_effects.begin(),
	from->del_effects);
    forall_effects.splice(
	forall_effects.begin(),
	from->forall_effects);
    cond_effects.splice(
	cond_effects.begin(),
	from->cond_effects);
    assign_effects.splice(
	assign_effects.begin(),
	from->assign_effects);
    timed_effects.splice(
	timed_effects.begin(),
	from->timed_effects);
}

void effect_lists::display(int ind) const
{
    TITLE(effect_lists);

    //list<simple_effect*>::iterator i;

    LABEL(add_effects);
    add_effects.display(ind);

    LABEL(del_effects);
    del_effects.display(ind);

    LABEL(forall_effects);
    forall_effects.display(ind);

    LABEL(cond_effects);
    cond_effects.display(ind);

    LABEL(assign_effects);
    assign_effects.display(ind);

    LABEL(timed_effects);
    timed_effects.display(ind);
}

void effect_lists::write(ostream & o) const
{
	wcntr->write_effect_lists(o,this);
};

void operator_list::display(int ind) const
{
    TITLE(operator_list);
    for(const_iterator i=begin(); i!=end(); ++i)
	ELT(*i);
}

void operator_list::write(ostream & o) const
{
	wcntr->write_operator_list(o,this);
};

void operator_::display(int ind) const
{
    TITLE(operator_);
    FIELD(name);
    FIELD(parameters);
    FIELD(precondition);
    FIELD(effects);
}

void operator_::write(ostream & o) const
{
	wcntr->write_operator_(o,this);
};

void action::display(int ind) const
{
    TITLE(action);
    FIELD(name);
    FIELD(parameters);
    FIELD(precondition);
    FIELD(effects);
}

void action::write(ostream & o) const
{
	wcntr->write_action(o,this);
};

void event::display(int ind) const
{
    TITLE(event);
    FIELD(name);
    FIELD(parameters);
    FIELD(precondition);
    FIELD(effects);
}

void event::write(ostream & o) const
{
	wcntr->write_event(o,this);
};

void process::display(int ind) const
{
    TITLE(process);
    FIELD(name);
    FIELD(parameters);
    FIELD(precondition);
    FIELD(effects);
}

void process::write(ostream & o) const
{
	wcntr->write_process(o,this);
};

void durative_action::display(int ind) const
{
    TITLE(durative_action);
    FIELD(name);
    FIELD(parameters);
    FIELD(dur_constraint);
    FIELD(precondition);
    FIELD(effects);
}

void durative_action::write(ostream & o) const
{
	wcntr->write_durative_action(o,this);
};

void domain::display(int ind) const
{
    TITLE(domain);
    LEAF(name);
    LEAF(req); indent(ind); cout << pddl_req_flags_string(req);
//    FIELD(types);
//    FIELD(constants);
//    FIELD(pred_vars);
    FIELD(predicates); 
    FIELD(ops);
}

void domain::write(ostream & o) const
{
	wcntr->write_domain(o,this);
};

void metric_spec::display(int ind) const
{
    TITLE(metric_spec);
    LEAF(opt);
    FIELD(expr);
}

void metric_spec::write(ostream & o) const
{
	wcntr->write_metric_spec(o,this);
};

void length_spec::display(int ind) const
{
    TITLE(length_spec);
    LEAF(mode);
    LEAF(length);
}

void length_spec::write(ostream & o) const
{
	wcntr->write_length_spec(o,this);
};

void problem::display(int ind) const
{
    TITLE(problem);
    LEAF(req); indent(ind+1); cout << pddl_req_flags_string(req);
    FIELD(types);
    FIELD(objects);
    FIELD(initial_state);
    FIELD(the_goal);
    FIELD(metric);
    FIELD(length);
};

void problem::write(ostream & o) const
{
	wcntr->write_problem(o,this);
};

void plan_step::display(int ind) const
{
    LEAF(start_time);
    FIELD(op_sym);
    FIELD(params);
    LEAF(duration);
}

void plan_step::write(ostream & o) const
{
	wcntr->write_plan_step(o,this);
};

void indent(int ind)
{
    cout << '\n';
    for (int i=0; i<ind; i++)
	cout << "   ";
}


/*---------------------------------------------------------------------------
  Functions called from parser that perform checks, and possibly generate
  errors.
  --------------------------------------------------------------------------*/

void requires(pddl_req_flag flags)
{
    if (!(flags & current_analysis->req))
	current_analysis->error_list.add(
	    E_WARNING,
	    "Undeclared requirement " + 
	    pddl_req_flags_string(flags));
}

void log_error(error_severity sev, const string& description)
{
    current_analysis->error_list.add(
	sev,
	description);
}

/*---------------------------------------------------------------------------
  Descriptions of requirements flags.
  Surely some way to avoid saying this again.
-----------------------------------------------------------------------------*/

string pddl_req_flags_string(pddl_req_flag flags)
{
    string result;

    if (flags & E_EQUALITY) result += ":equality ";
    if (flags & E_STRIPS) result += ":strips ";
    if (flags & E_TYPING) result += ":typing ";
    if (flags & E_DISJUNCTIVE_PRECONDS) 
	result += ":disjunctive-preconditions ";
    if (flags & E_EXT_PRECS) result += ":existential-preconditions ";
    if (flags & E_UNIV_PRECS) result += ":universal-preconditions ";
    if (flags & E_COND_EFFS) result += ":conditional-effects ";
    if (flags & E_FLUENTS) result += ":fluents ";
    if (flags & E_DURATIVE_ACTIONS) result += ":durative-actions ";
    if (flags & E_DURATION_INEQUALITIES) result += ":duration-inequalities ";
    if (flags & E_CONTINUOUS_EFFECTS) result += ":continuous-effects ";
    return result;
}


// Search tables from top of stack to find symbol matching name.
// If none found, add to top table.
// Return pointer to symbol.
var_symbol* var_symbol_table_stack::symbol_get(const string& name)
{
    var_symbol* sym= NULL;

    // Iterate through stack from top to bottom
    // (may need to change direction if changing underlying 
    // impl. of stack)
    for (iterator i=begin(); i!=end() && sym==NULL; ++i)
	sym= (*i)->symbol_probe(name);

    if (sym !=NULL)
	// return found symbol
	return sym;
    else
    {
	// Log a warning 
	// add new symbol to current table.
	log_error(E_WARNING,"Undeclared variable symbol: ?" + name);
	return top()->symbol_put(name);
    }
};

var_symbol* var_symbol_table_stack::symbol_put(const string& name)
{
    return top()->symbol_put(name);
};
