/*-----------------------------------------------------------------------------
  Class definitions for PDDL2.1 parse trees

  $Date: 2004/09/18 16:45:07 $
  $Revision: 1.1.1.1 $

  s.n.cresswell@durham.ac.uk

  Durham Planning Group
  http://www.dur.ac.uk/~dcs0www/research/stanstuff/planpage.html
 ----------------------------------------------------------------------------


  In general, data members are pointers to objects allocated using
  new.  Yacc (bison) is not C++-tolerant enough to allow object
  instances to be returned as semantic values, so it is necessary in
  general to return pointers instead.

  Deleting any parse_category class should automatically delete all
  contained structures.  Symbols are an exception to this, as a
  symbol is always owned by a symbol table.
 ----------------------------------------------------------------------------*/

#ifndef PTREE_H
#define PTREE_H

#include <stl.h>
#include <string>
#include "sStack.h"
#include "macros.h"
#include "parse_error.h"
#include <iostream>


/*-----------------------------------------------------------------------------
  Forward declaration of classes, 
  (because in some cases we have mutually referring structures).
 ----------------------------------------------------------------------------*/

class parse_category;

 class symbol;
  class pred_symbol;
  class func_symbol;
  class pddl_typed_symbol;
   class parameter_symbol;
   class var_symbol;
   class const_symbol;
   class pddl_type;
 class operator_symbol;

 class proposition;
 class proposition_list;
 class pred_decl;
 class func_decl;
 class pred_decl_list;
 class func_decl_list;

 class expression;
  class binary_expression;
   class plus_expression;
   class minus_expression;
   class mul_expression;
   class div_expression;
  class uminus_expression;
  class num_expression;
  class int_expression;
  class float_expression;
  class func_term;
  class special_val_expr;

 class goal_list;
 class goal;
  class simple_goal;
  class qfied_goal;
  class conj_goal;
  class disj_goal;
  class imply_goal;
  class neg_goal;
  class timed_goal;
  class comparison;

 class effect;
  class simple_effect;
  class forall_effect;
  class cond_effect;
  class timed_effect;
  class assignment;

 class operator_list;
 class operator_;
  class action;
  class event;
  class process;
  class durative_action;

 class domain;

 class metric_spec;
 class length_spec;
 class problem;

 class plan_step;
 class plan;

class var_symbol_table_stack;
class analysis;

class WriteController;

enum quantifier { E_FORALL, E_EXISTS };
enum polarity { E_NEG, E_POS };
enum assign_op { E_ASSIGN, E_INCREASE, E_DECREASE, E_SCALE_UP, E_SCALE_DOWN };
enum comparison_op { E_GREATER, E_GREATEQ, E_LESS, E_LESSEQ, E_EQUALS };
enum optimization { E_MINIMIZE, E_MAXIMIZE };
enum time_spec { E_AT_START, E_AT_END, E_OVER_ALL, E_CONTINUOUS };
enum special_val { E_HASHT, E_DURATION_VAR, E_TOTAL_TIME };
enum length_mode { E_SERIAL, E_PARALLEL };

template <class symbol_class> class typed_symbol_list;

/*----------------------------------------------------------------------------
  --------------------------------------------------------------------------*/

extern parse_category* top_thing;
extern analysis* current_analysis;

/*---------------------------------------------------------------------------*
  ---------------------------------------------------------------------------*/

class parse_category
{
protected:
	static auto_ptr<WriteController> wcntr;
public:
    parse_category() {};
    virtual ~parse_category() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const {};
    static void setWriteController(auto_ptr<WriteController> w);
};

ostream & operator <<(ostream  & o,const parse_category & p);


/*---------------------------------------------------------------------------*
  Specialisation of list template.
  This is used as a list of pointers to parse category entities.
  ---------------------------------------------------------------------------*/

template<class pc>
class pc_list : public list<pc>, public parse_category
{
public:
    virtual ~pc_list();
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

/*-----------------------------------------------------------------------------
  Lists of parse category things
  ---------------------------------------------------------------------------*/

template<class pc>
pc_list<pc>::~pc_list()
{
    for (pc_list<pc>::iterator i=begin(); i!=end(); ++i)
	delete(*i);
};

template<class pc>
void pc_list<pc>::display(int ind) const
{
    for (pc_list<pc >::const_iterator i=begin(); i!=end(); ++i)
	ELT(*i);
};

template<class pc>
void pc_list<pc>::write(ostream & o) const
{
	for (pc_list<pc >::const_iterator i=begin(); i!=end(); ++i)
	{
		(*i)->write(o);
	};
};

/*---------------------------------------------------------------------------*
  Symbol tables
  We have various ways of looking up/adding symbols, depending on whether
  we expect symbol to be already present in the table.
 *---------------------------------------------------------------------------*/

template<class symbol_class> 
class symbol_table : public map<string,symbol_class*>
{
public :

    typedef typename map<string,symbol_class*>::iterator iterator;

    // symbol_ref(string)
    // Don't care whether symbol is already present
    symbol_class* symbol_ref(const string& name)
	{
	    iterator i= find(name);
	    //symbol_table::iterator i= find(name);

	    // If name is already in symbol table
	    if (i != end())
	    {
		// Return existing symbol entry
		return i->second;
	    }
	    else
	    {
		// Create new symbol for name and add to table
		symbol_class* sym= new symbol_class(name);

		insert(std::make_pair(name,sym));
		return sym;
	    }
	};

    // Look up symbol, returning NULL pointer if not already present.
    //  (so callers must check the result!).
    symbol_class* symbol_probe(const string& name)
 	{
	    iterator i= find(name);
 	    //symbol_table::iterator i= find(name);

	    // If name is already in symbol table
	    if (i != end())
	    {
		// Return existing symbol entry
 		return i->second;
	    }
	    else
 	    {
		// Otherwise return null pointer
 		return NULL;
 	    }
	};

    // Look up symbol, requiring that symbol is already present
    symbol_class* symbol_get(const string& name)
 	{
	    iterator i= find(name);
 	    //symbol_table::iterator i= find(name);

 	    // If name is already in symbol table
 	    if (i != end())
	    {
		// Return found symbol
		return i->second;
	    }
	    else
 	    {
		// Log an error, then add symbol to table anyway.
		log_error( E_WARNING,
			   "Undeclared symbol: " + name );
		symbol_class* sym= new symbol_class(name);
		insert(std::make_pair(name,sym));

		return(sym);
 	    }
	};

    // Add symbol to table, requiring that symbol is not already present
    symbol_class* symbol_put(const string& name)
 	{
	    iterator i= find(name);
 	    //symbol_table::iterator i= find(name);

 	    // If name is already in symbol table
 	    if (i != end())
	    {
 		// Log an error
		log_error( E_WARNING,
			   "Re-declaration of symbol in same scope: " + name);
		return i->second;
	    }
	    else
 	    {
		// add new symbol
		symbol_class* sym= new symbol_class(name);
		insert(std::make_pair(name,sym));

		return(sym);
 	    }
	};

    virtual void display(int ind) const
	{
	    TITLE(symbol_table);
	    //for (symbol_table::iterator i=begin(); i!=end(); ++i)
	    for (const_iterator i=begin(); i!=end(); ++i)
	    {
		LEAF(i->first);
		FIELD(i->second);
	    }
	};

    virtual ~symbol_table()
	{
//	    for(symbol_table::iterator i= begin(); i!=end(); ++i)
	    for(iterator i= begin(); i!=end(); ++i)
		delete i->second;
	};
};


// Refinements of symbol tables
typedef symbol_table<var_symbol>   var_symbol_table;
typedef symbol_table<const_symbol> const_symbol_table;
typedef symbol_table<pddl_type>    pddl_type_symbol_table;
typedef symbol_table<pred_symbol>  pred_symbol_table;
typedef symbol_table<func_symbol>  func_symbol_table;
typedef symbol_table<operator_symbol>  operator_symbol_table;


/*-----------------------------------------------------------------------------
  Lists of symbols 
  ---------------------------------------------------------------------------*/

// No destructor for symbol lists
// - destroying the symbols themselves is responsibility of a symbol_table
// - hence we don't make it a pc_list.
template <class symbol_class>
class typed_symbol_list : public list<symbol_class*>, public parse_category
{
public:
    typedef typename list<symbol_class*>::iterator iterator;

    void set_types(pddl_type* t)
	{
	    //for (typed_symbol_list::iterator i= begin(); i!=end(); ++i)
	    for (iterator i= begin(); i!=end(); ++i)
	    {
	    	if((*i)->type)
	    	{
	    		(*i)->either_types = new typed_symbol_list<pddl_type>;
	    		(*i)->either_types->push_back((*i)->type);
	    		(*i)->either_types->push_back(t);
	    		(*i)->type = 0;
	    		continue;
	    	};
	    	if((*i)->either_types)
	    	{
	    		(*i)->either_types->push_back(t);
	    		continue;
	    	};
	    	(*i)->type = t;
	    };
	};

    void set_either_types(typed_symbol_list<pddl_type>* tl)
 	{
 	    //for (typed_symbol_list::iterator i= begin(); i!=end(); ++i)
 	    for (iterator i= begin(); i!=end(); ++i)
 		(*i)->either_types= tl;
 	};

    virtual void display(int ind) const
	{
	    TITLE(typed_symbol_list<>);
	    //for (typed_symbol_list::iterator i= begin(); i!=end(); ++i)
	    for (const_iterator i= begin(); i!=end(); ++i)
		ELT(*i);
	};

	virtual void write(ostream & o) const
	{
		for (list<symbol_class*>::const_iterator i=begin(); i!=end(); ++i)
		{
			o << " ";
			(*i)->symbol_class::write(o);
		};
	};

    virtual ~typed_symbol_list() {};
};


//class var_symbol_list : public typed_symbol_list<var_symbol> {};
typedef typed_symbol_list<var_symbol> var_symbol_list;
typedef typed_symbol_list<parameter_symbol> parameter_symbol_list;
typedef typed_symbol_list<const_symbol> const_symbol_list;
typedef typed_symbol_list<pddl_type> pddl_type_list;



/*----------------------------------------------------------------------------
  Symbols
   used for constants, variables, types, and predicate names.
   Generally, a pointer to a symbol will be used as a unique identifier. 
  --------------------------------------------------------------------------*/

class symbol : public parse_category
{
protected:
    string name;

public:
    symbol() {};
    symbol(const string& s) : name(s) {};

    virtual ~symbol() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const string getName() const {return name;};
};

class pred_symbol : public symbol
{
public:
    pred_symbol(const string& s) : symbol(s) {};
    virtual ~pred_symbol() {};
};

class func_symbol : public symbol
{
public:
    func_symbol(const string& s) : symbol(s) {};
    virtual ~func_symbol() {};
};

// Variables, constants or types.
class pddl_typed_symbol : public symbol
{
public:
    pddl_type* type;               // parent type
    pddl_type_list* either_types;  // types declared with 'either'

    pddl_typed_symbol() : symbol(""), type(NULL), either_types(NULL) {};
    pddl_typed_symbol(const string& s) : symbol(s), type(NULL), either_types(NULL) {};
    
    virtual ~pddl_typed_symbol() 
	{
	    delete either_types;
	};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

// Parameters can be variables or constant symbols
class parameter_symbol : public pddl_typed_symbol 
{
public:
    parameter_symbol(const string& s) : pddl_typed_symbol(s) {};
    virtual ~parameter_symbol() {};
};


class var_symbol   : public parameter_symbol 
{
public:
    var_symbol(const string& s) : parameter_symbol(s) {};
    virtual ~var_symbol() {};

    virtual void write(ostream & o) const;
};


class const_symbol : public parameter_symbol 
{
public:
    const_symbol(const string& s) : parameter_symbol(s) {};
    virtual ~const_symbol() {};

    virtual void write(ostream & o) const;
};

// PDDL types

class pddl_type : public pddl_typed_symbol
{
public:
    pddl_type(const string& s) : pddl_typed_symbol(s) {};
    virtual ~pddl_type() {};
};

class operator_symbol : public symbol
{
public:
    operator_symbol(const string& s) : symbol(s) {};
    // probably need to also refer to operator itself to enable
    // lookup of operator by name
    virtual ~operator_symbol() {};
};




/*---------------------------------------------------------------------------*
  Proposition
 *---------------------------------------------------------------------------*/

class proposition : public parse_category
{
public:
    pred_symbol* head;
    parameter_symbol_list* args;

    proposition(pred_symbol* h, parameter_symbol_list* a) :
	head(h), args(a) {};

    virtual ~proposition()
	{
	    // don't delete head - it belongs to a symbol table
	    delete args;
	};

    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};



class proposition_list : public pc_list<proposition*> {};

// Nearly the same as a proposition, but:
//    The arguments must be variables.
//    These variables are local to the declaration,
//     so the pred_decl class has its own symbol table.

class pred_decl : public parse_category
{
private:
    pred_symbol* head;
    var_symbol_list* args;
    var_symbol_table* var_tab;

public:
    pred_decl(pred_symbol* h, 
//	      typed_symbol_list<var_symbol>* a, 
	      var_symbol_list* a, 
	      var_symbol_table* vt) :
	head(h), args(a), var_tab(vt) {};

	const pred_symbol * getPred() const {return head;};
    const var_symbol_list * getArgs() const {return args;};
    
    virtual ~pred_decl() { delete args; delete var_tab; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
    
};



class func_decl : public parse_category
{
private:
    func_symbol* head;
    var_symbol_list* args;
    var_symbol_table* var_tab;


public:
    func_decl(func_symbol* h, 
//	      typed_symbol_list<var_symbol>* a, 
	      var_symbol_list* a,
	      var_symbol_table* vt) :
      head(h), args(a), var_tab(vt) {};

	const func_symbol * getFunction() const {return head;};
    const var_symbol_list * getArgs() const {return args;};

    virtual ~func_decl() { delete args; delete var_tab; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    
};


class pred_decl_list : public pc_list<pred_decl*>
{
public:
    virtual ~pred_decl_list() {};
    virtual void write(ostream & o) const;
};

class func_decl_list : public pc_list<func_decl*> 
{
public:
    virtual ~func_decl_list() {};
    virtual void write(ostream & o) const;
};

/*----------------------------------------------------------------------------
  Expressions
  --------------------------------------------------------------------------*/

class expression : public parse_category
{
};

class binary_expression : public expression {
protected: 
	expression * arg1;
	expression * arg2;
public:
	binary_expression(expression * a1,expression * a2) :
		arg1(a1), arg2(a2)
	{};
	virtual ~binary_expression()
	{
		delete arg1;
		delete arg2;
	};
	const expression * getLHS() const {return arg1;};
	const expression * getRHS() const {return arg2;};
};

class plus_expression : public binary_expression
{
public:
    plus_expression(expression *a1, expression *a2) : 
	binary_expression(a1,a2) {};
    
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class minus_expression : public binary_expression
{
public:
    minus_expression(expression *a1, expression *a2) : 
	binary_expression(a1,a2) {};
    
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};  

class mul_expression : public binary_expression
{
public:
    mul_expression(expression *a1, expression *a2) : 
	binary_expression(a1,a2) {};
    
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class div_expression : public binary_expression
{
public:
    div_expression(expression *a1, expression *a2) : 
	binary_expression(a1,a2) {};
    
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class uminus_expression : public expression
{
private:
    expression *arg1;
public:
    uminus_expression(expression *a1) : 
	arg1(a1) {};
    virtual ~uminus_expression()
	{ delete arg1;};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const expression * getExpr() const {return arg1;};
};  

class num_expression : public expression {
public:
	virtual ~num_expression() {};
	virtual const double double_value() const = 0;
	};

class int_expression : public num_expression
{
private:
    int val;
public:
    int_expression(int v) : val(v) {};
    virtual ~int_expression() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const double double_value() const {return static_cast<const double>(val);};
};

class float_expression : public num_expression
{
private:
    float val;
public:
    float_expression(float v) : val(v) {};
    virtual ~float_expression() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
    
    const double double_value() const {return static_cast<const double>(val);};
};

class func_term : public expression
{
private:
    func_symbol *func_sym;
    parameter_symbol_list *param_list;
public:
    func_term(func_symbol *fs, parameter_symbol_list *psl) : 
	func_sym(fs), param_list(psl) {};
    virtual ~func_term() {delete param_list;};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const func_symbol * getFunction() const {return func_sym;};
    const parameter_symbol_list * getArgs() const {return param_list;};
};

// FIX: this is the duration var
// This class for special values hasht and ?duration
// Not sure what should be done with these.
class special_val_expr : public expression
{
private:
    const special_val var;

public:
    special_val_expr(special_val v) : var(v) {};
    virtual ~special_val_expr() {};
    const special_val getKind() const {return var;};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

// [ end of expression classes ]

/*---------------------------------------------------------------------------
  Goals
  ---------------------------------------------------------------------------*/


class goal_list: public pc_list<goal*>
{
public:
    virtual ~goal_list() {}; 
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class goal : public parse_category {};

class simple_goal : public goal
{
private:
    polarity plrty;    // +ve or -ve goals
    proposition* prop;

public:
    simple_goal(proposition* prp, polarity pol) : plrty(pol), prop(prp) {};
    virtual ~simple_goal()
	{ delete prop; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const polarity getPolarity() const {return plrty;};
    const proposition * getProp() const {return prop;};
};

class qfied_goal : public goal
{
private:
    const quantifier qfier;
    var_symbol_list* vars;
    goal* gl;

public:
    qfied_goal(quantifier q, var_symbol_list* vl, goal* g) :
	qfier(q),
	vars(vl),
	gl(g) 
	{};
    virtual ~qfied_goal()  { delete vars; delete gl; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
    const quantifier getQuantifier() const {return qfier;};
    const var_symbol_list* getVars() const {return vars;};
    const goal * getGoal() const {return gl;};
};


class conj_goal : public goal
{
private:
    goal_list* goals;
public:
    conj_goal(goal_list* gs): goals(gs) {};
    virtual ~conj_goal() { delete goals; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal_list * getGoals() const {return goals;};
};

class disj_goal : public goal 
{
private:
    goal_list* goals;
public:
    disj_goal(goal_list* gs): goals(gs) {};
    virtual ~disj_goal() { delete goals; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal_list * getGoals() const {return goals;};
};

class imply_goal : public goal
{
private:
    goal* lhs;
    goal* rhs;

public:
    imply_goal(goal* lhs, goal* rhs) :
	lhs(lhs), rhs(rhs)
	{};
    virtual ~imply_goal()  { delete lhs; delete rhs; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal * getAntecedent() const {return lhs;};
    const goal * getConsequent() const {return rhs;};
};

class neg_goal : public goal
{
private:
    goal* gl;

public:
    neg_goal(goal* g) :
	gl(g) {};

    virtual ~neg_goal() { delete gl; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal * getGoal() const {return gl;};
};

class timed_goal : public goal
{
private:
    goal* gl;
    time_spec ts;

public:
    timed_goal (goal* g, time_spec t) : gl(g), ts(t) {};
    virtual ~timed_goal() { delete gl; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal * getGoal() const {return gl;};
    time_spec getTime() const {return ts;};
};

class comparison : public goal, public binary_expression // proposition?
{
private:
    comparison_op op;

public:
    comparison(comparison_op c_op, expression* e1, expression* e2) : 
	binary_expression(e1,e2), op(c_op) {};
    
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
    const comparison_op getOp() const {return op;};
};


/*---------------------------------------------------------------------------*
  Effect lists
  - a single class containing a separate list of effects of each type
 *---------------------------------------------------------------------------*/

class effect_lists : public parse_category
{
public:
    pc_list<simple_effect*> add_effects;
    pc_list<simple_effect*> del_effects;
    pc_list<forall_effect*> forall_effects;
    pc_list<cond_effect*>   cond_effects;
    pc_list<assignment*>    assign_effects;
    pc_list<timed_effect*>  timed_effects;

    effect_lists() {};

    virtual ~effect_lists() {};
    void append_effects(effect_lists* from);
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

/*-----------------------------------------------------------------------------
  effect classes
  ---------------------------------------------------------------------------*/

class effect : public parse_category 
{
public:
    effect() {};

    virtual ~effect() {};
    virtual void display(int ind) const {};
};


class simple_effect : public effect 
{
public:
    proposition* prop;

    simple_effect(proposition* eff) : effect(), prop(eff) {};
    virtual ~simple_effect() {delete prop;};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class forall_effect : public effect
{
private:
    effect_lists* operand;
    var_symbol_table* var_tab;

public:
    forall_effect(effect_lists* eff, var_symbol_table* vt) : 
	effect(), operand(eff), var_tab(vt) 
	{};

    virtual ~forall_effect()
	{
	    delete operand;
	    delete var_tab;
	};

    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

	const var_symbol_table * getVars() const {return var_tab;};
    const effect_lists * getEffects() const {return operand;};
};


class cond_effect : public effect
{
private:
    goal* cond;
    effect_lists* effects;

public:
    // Construct from a list
    cond_effect(goal* g, effect_lists* e) : 
	effect(), 
	cond(g),
	effects(e)
        
	{};

    virtual ~cond_effect()
	{
	    delete cond;
	    delete effects;
	};

    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const goal * getCondition() const {return cond;};
    const effect_lists* getEffects() const {return effects;};
};


class timed_effect : public effect 
{
public:
    time_spec ts;
    effect_lists* effs;

    timed_effect(effect_lists* e, time_spec t ) : ts(t), effs(e) {};
    virtual ~timed_effect() {delete effs;};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};


class assignment : public effect
{
private:
    func_term *f_term; // Thing to which value is assigned.
    assign_op op;      // Assignment operator, e.g. 
    expression *expr;  // Value that gets assigned
public:
    assignment(func_term *ft, assign_op a_op, expression *e) : 
	f_term(ft), op(a_op), expr(e) {};
    virtual ~assignment() { delete f_term; delete expr; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;

    const func_term * getFTerm() const {return f_term;};
    const expression* getExpr() const {return expr;};
    const assign_op getOp() const {return op;};
};



/*----------------------------------------------------------------------------
  Operators
  --------------------------------------------------------------------------*/

class operator_ : public parse_category
{
public:
    operator_symbol* name;
    var_symbol_table* symtab;
    var_symbol_list* parameters;
    goal* precondition;
    effect_lists* effects;

    operator_() {};
    operator_( operator_symbol* nm,
	       var_symbol_list* ps,
	       goal* pre,
	       effect_lists* effs,
	       var_symbol_table* st) :
	name(nm),
	symtab(st),
	parameters(ps),
	precondition(pre),
	effects(effs)
	
	{};
    virtual ~operator_() 
	{
	    delete parameters;
	    delete precondition;
	    delete effects;
	    delete symtab;
	};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class operator_list: public pc_list<operator_*> 
{
public:
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
    virtual ~operator_list() {};
};


/*----------------------------------------------------------------------------
  Classes derived from operator:
    action
    event 
    process
    durative_action
  --------------------------------------------------------------------------*/

class action : public operator_
{
public:
    action( operator_symbol* nm,
	    var_symbol_list* ps,
	    goal* pre,
	    effect_lists* effs,
	    var_symbol_table* st) :
	operator_(nm,ps,pre,effs,st)
	{};
    virtual ~action() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class event : public operator_
{
public:
    event(goal* pre, effect_lists* effect) {};
    event( operator_symbol* nm,
	   var_symbol_list* ps,
	   goal* pre,
	   effect_lists* effs,
	   var_symbol_table* st) :
	operator_(nm,ps,pre,effs,st)
	{};
    virtual ~event() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

class process : public operator_
{
public:
    process() {};
    process( operator_symbol* nm,
	     var_symbol_list* ps,
	     goal* pre,
	     effect_lists* effs,
	     var_symbol_table* st) :
	operator_(nm,ps,pre,effs,st)
	{};
    virtual ~process() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};


class durative_action : public operator_
{
public:
    goal* dur_constraint;
    durative_action() {};
    virtual ~durative_action() 
	{
	    delete dur_constraint;
	};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};

/*---------------------------------------------------------------------------
  PDDL requirements flags
  -------------------------------------------------------------------------*/

typedef unsigned long pddl_req_flag;


// When changing these, also look at the function pddl_req_attribute_name()
enum pddl_req_attr { E_EQUALITY              =    1, 
		     E_STRIPS                =    2, 
		     E_TYPING                =    4,
		     E_DISJUNCTIVE_PRECONDS  =    8, 
		     E_EXT_PRECS             =   16,
		     E_UNIV_PRECS            =   32,
		     E_COND_EFFS             =   64,
		     E_FLUENTS               =  128,
		     E_DURATIVE_ACTIONS      =  256,
		     E_TIME                  =  512,    // Obsolete?
		     E_DURATION_INEQUALITIES = 1024,
		     E_CONTINUOUS_EFFECTS    = 2048,
		     E_NEGATIVE_PRECONDITIONS= 4096
		     
// Attributes which are defined as combinations of others 
// are expanded by parser, and don't need to be included here.
};


/*---------------------------------------------------------------------------
  Functions relating to error handling
  ---------------------------------------------------------------------------*/

void requires(pddl_req_flag);
//string pddl_req_flags_string(pddl_req_flag flags);
string pddl_req_flags_string(pddl_req_flag flags);
void log_error(error_severity sev, const string& description);

/*---------------------------------------------------------------------------
  Domain.
  ---------------------------------------------------------------------------*/

class domain : public parse_category
{
public:
    operator_list* ops;
    string name;
    pddl_req_flag req;
    pddl_type_list* types;
    const_symbol_list* constants;
    var_symbol_table* pred_vars;  // Vars used in predicate declarations
    pred_decl_list* predicates;
    func_decl_list* functions;

    domain( operator_list* ops) : 
	ops(ops),
	req(0),
	types(NULL),
	constants(NULL),
	pred_vars(NULL),
	predicates(NULL),
	functions(NULL)
	{};

    virtual ~domain() 
	{
	    delete ops;
	    delete types;
	    delete constants;
	    delete predicates;
	    delete functions;
	    delete pred_vars;
	};
    virtual void display(int ind) const;
    void write(ostream & o) const;
    bool isDurative() const
    {
    	return req & (E_DURATIVE_ACTIONS | E_TIME);
    };
};

/*----------------------------------------------------------------------------
  Plan
 ----------------------------------------------------------------------------*/

class plan_step : public parse_category
{
public:
    operator_symbol* op_sym;
    const_symbol_list* params;
    
    bool start_time_given;
    bool duration_given;
    float start_time;
    float duration;

    plan_step(operator_symbol* o, const_symbol_list* p) :
	op_sym(o),
	params(p)
	{};

    virtual ~plan_step()
	{
	    delete params;
	};

    virtual void  display(int ind) const;
    virtual void write(ostream & o) const;
};


class plan : public pc_list<plan_step*> 
{
public:
    virtual ~plan() {};
};

/*----------------------------------------------------------------------------
  PDDL+ entities
  --------------------------------------------------------------------------*/


class metric_spec : public parse_category
{
public:
    optimization opt;
    expression* expr;

    metric_spec(optimization o, expression* e) : opt(o), expr(e) {};
    virtual ~metric_spec() { delete expr; };
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};


class length_spec : public parse_category
{
public:
    length_mode mode;
    int length;

    length_spec(length_mode m, int l) : mode(m), length(l) {};
    virtual ~length_spec() {};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};


/*----------------------------------------------------------------------------
  Problem definition
  --------------------------------------------------------------------------*/

class problem : public parse_category
{
    
public:
	char * name;
	char * domain_name;
    pddl_req_flag req;
    pddl_type_list* types;
    const_symbol_list* objects;
    effect_lists* initial_state;
    goal* the_goal;
    metric_spec* metric;
    length_spec* length;

    problem() :
    name(0),
    domain_name(0),
	req(0),
	types(NULL),
	objects(NULL),
	initial_state(NULL),
	the_goal(NULL),
	metric(NULL),
	length(NULL) 
	{};
	
    virtual ~problem() 
	{
		delete [] name;
		delete [] domain_name;
	    delete types;
	    delete objects;
	    delete initial_state;
	    delete the_goal;
	    delete metric;
	    delete length;
	};
    virtual void display(int ind) const;
    virtual void write(ostream & o) const;
};



/*----------------------------------------------------------------------------
  We need to be able to search back through the tables in the stack to
  find a reference to a particular symbol.  The standard STL stack
  only allows access to top.

  The symbol_ref() member function does this, making use of its access
  to the iterator for the stack.
  --------------------------------------------------------------------------*/

class var_symbol_table_stack : public sStack<var_symbol_table*>
{
public:
    var_symbol* symbol_get(const string& name);
    var_symbol* symbol_put(const string& name);
    ~var_symbol_table_stack()
    {
    	for(deque<var_symbol_table*>::const_iterator i = begin();
    			i != end();++i)
    		delete (*i);
    };
};

/*---------------------------------------------------------------------------*
  Analysis.
  Here we store various symbol tables for constants, types, and predicates.
  For variables, we have a stack of symbol tables which is used during
  parsing.  
  Operators and quantified constructs have their own local scope,
  and their own symbol tables.
 *---------------------------------------------------------------------------*/


class analysis 
{
public:
    var_symbol_table_stack var_tab_stack;
    const_symbol_table     const_tab;
    pddl_type_symbol_table pddl_type_tab;
    pred_symbol_table      pred_tab;
    func_symbol_table      func_tab;
    operator_symbol_table  op_tab;
    pddl_req_flag          req;
    
    parse_error_list error_list;
    
    domain* the_domain;
    problem* the_problem;

    analysis() :
	the_domain(NULL),
	the_problem(NULL)
	{
	    // Push a symbol table on stack as a safety net
	    var_tab_stack.push(new var_symbol_table);
	}

    virtual ~analysis() 
	{
	    delete the_domain;
	    delete the_problem;
	};
};


#endif /* PTREE_H */

