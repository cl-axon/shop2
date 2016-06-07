#include "State.h"
#include "Plan.h"
#include "Validator.h"
#include "typecheck.h"

#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "ptree.h"
#include <FlexLexer.h>

#include <string>
#include "main.h"

extern int yyparse();
extern int yydebug;

parse_category* top_thing = NULL;

analysis an_analysis;
analysis* current_analysis;

yyFlexLexer* yfl;
bool Verbose;
char * current_filename;

typedef map<double,vector<string> > Ranking;

struct showList {
	void operator()(const pair<double,vector<string> > & ps) const
	{
		cout << "\nValue: " << ps.first << "\n ";
		copy(ps.second.begin(),ps.second.end(),ostream_iterator<string>(cout," "));
		cout << "\n";
	};
};

void usage()
{
	cout << "Useage: validate [options] domainFile problemFile planFile1 ...\n" 
		     << "Options:\n    -t <n>  -- Set tolerance to (float) value of n.\n" 
		     << "    -v      -- Verbose reporting of plan check progress.\n"
		     << "    -h      -- Print this message.\n"
		     << "    -g      -- Use graphplan length where no metric specified.\n"
		     << "Multiple plan file arguments can be appended for checking.\n\n";
};

int main(int argc,char * argv[])
{
  try {
	if(argc < 2)
	{
		usage();
		return 0;
	};

    current_analysis= &an_analysis;

	Verbose = false;
	double tolerance = 0.01;
	bool lengthDefault = true;
	
    int argcount = 1;
    while(argcount < argc && argv[argcount][0] == '-')
    {
		switch(argv[argcount][1])
		{
    		case 'v': 
	    	
	    		Verbose = true;
	    		++argcount;
	    		break;
	    	
    	
    		case 't':
	   
	    		tolerance = atof(argv[++argcount]);
	    		++argcount;
	    		break;

	    	case 'g':

	 			lengthDefault = false;
	 			++argcount;
	 			break;

	    	case 'h':
	    	
	    		usage();
	    	    exit(0);

	    	default:
	    		cout << "Unrecognised command line switch: " << argv[argcount] << "\n";
	    		exit(0);
		};
    };

	if(argcount>=argc) 
	{
		usage();
		return 0;
	};
	
    ifstream domainFile(argv[argcount++]);
    if(!domainFile) 
    {
    	cerr << "Bad domain file!\n";
    	exit(1);
    };
    
    yfl= new yyFlexLexer(&domainFile,&cout);
    yydebug=0;
    yyparse();
    delete yfl;

    if(!an_analysis.the_domain)
    {
    	cerr << "Problem in domain definition!\n";
    	exit(1);
    };
    
    TypeChecker tc(current_analysis);
    if(!tc.typecheckDomain())
    {
    	cerr << "Type problem in domain description!\n";
    	exit(1);
    };

	if(argcount>=argc) 
	{
		return 0;
	};
	
    ifstream problemFile(argv[argcount++]);
    if(!problemFile)
    {
    	cerr << "Bad problem file!\n";
    	exit(1);
    };
    
    yfl = new yyFlexLexer(&problemFile,&cout);
    yyparse();
    delete yfl;

	if(!tc.typecheckProblem())
	{
		cerr << "Type problem in problem specification!\n";
		exit(1);
	};
	
    Ranking rnk;
    vector<string> failed;
    vector<string> queries;

	while(argcount < argc)
	{
		cout << "Checking plan: " << argv[argcount] << "\n";
		string name(argv[argcount]);
		
	    ifstream planFile(argv[argcount++]);
	    if(!planFile)
	    {
	    	failed.push_back(name);
	    	cout << "Bad plan file!\n";
	    	continue;
	    };
	    
	    yfl = new yyFlexLexer(&planFile,&cout);
	    yyparse();
	    delete yfl;

		plan * the_plan = dynamic_cast<plan*>(top_thing);
		
		
	    if(!the_plan || !tc.typecheckPlan(the_plan))
	    {
	    	failed.push_back(name);
	    	cout << "Bad plan description!\n";
	    	delete the_plan;
	    	continue;
	    };
	    
	    Validator v(tolerance,tc,an_analysis.the_domain->ops,
	    			an_analysis.the_problem->initial_state,
	    			the_plan,an_analysis.the_problem->metric,lengthDefault,
	    			an_analysis.the_domain->isDurative());
		delete the_plan;
		
	    if(Verbose) v.displayPlan();
	    
	    try {

		    if(v.execute())
		    {
		    	cout << "Plan executed successfully - checking goal\n";
		    	if(v.checkGoal(an_analysis.the_problem->the_goal))
		    	{
		    		rnk[v.finalValue()].push_back(name);
		    		cout << "Plan valid\n";
		    		cout << "Final value: " << v.finalValue() << "\n";
		    	}
		    	else
		    	{
		    		failed.push_back(name);
		    		cout << "Goal not satisfied\n";
		    		cout << "Plan invalid\n";
		    	};
		    }
		    else
		    {
		    	failed.push_back(name);
		    	cout << "Plan failed to execute\n";
		    	// Plan didn't execute - can we report why?
		    };
		}
		catch(exception & e)
		{
			cout << "Error occurred in validation attempt:\n  " << e.what() << "\n";
			queries.push_back(name);
		};

	};

	if(!rnk.empty())
	{
		cout << "\nSuccessful plans:";

		if(an_analysis.the_problem->metric && 
				an_analysis.the_problem->metric->opt == E_MINIMIZE)
		{
			for_each(rnk.begin(),rnk.end(),showList());
		}
		else
		{
			for_each(rnk.rbegin(),rnk.rend(),showList());
		};
		cout << "\n";
	};
	
	if(!failed.empty())
	{
		cout << "\n\nFailed plans:\n ";
		copy(failed.begin(),failed.end(),ostream_iterator<string>(cout," "));
		cout << "\n";
	};

	if(!queries.empty())
	{
		cout << "\n\nQueries (validator failed):\n ";
		copy(queries.begin(),queries.end(),ostream_iterator<string>(cout," "));
		cout << "\n";
	};
  }
  catch(exception & e)
  {
  	cerr << "Error: " << e.what() << "\n";
  	an_analysis.error_list.report();
  	return 0;
  };

    return 0;
};


