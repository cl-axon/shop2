#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "ptree.h"
#include <FlexLexer.h>
#include "TypeStripWC.h"

extern int yyparse();
extern int yydebug;

parse_category* top_thing=NULL;

char * current_filename;

analysis an_analysis;
analysis* current_analysis;

yyFlexLexer * yfl;

bool Verbose = false;

int main(int argc,char * argv[])
{
	current_analysis = & an_analysis;
	
	yfl = new yyFlexLexer;
    
    ifstream current_in_stream(argv[1]);
    yydebug=0; // Set to 1 to output yacc trace 

	cout << "Processing file: " << argv[1] << '\n';
	
	if (current_in_stream.bad())
	{
	    cout << "Failed to open\n";
	    // Log an error to be reported in summary later
	    line_no= 0;
	    log_error(E_FATAL,"Failed to open file");
	}
	else
	{
	    line_no= 1;

	    // Switch the tokeniser to the current input stream
	    yfl->switch_streams(&current_in_stream,&cout);
	    yyparse();

	    // Output syntax tree
	    auto_ptr<WriteController> ts 
	    		= auto_ptr<WriteController>(new TypeStripWriteController(current_analysis));
	    parse_category::setWriteController(ts);
	    if (top_thing) 
		{
			string nm(argv[1]);
			nm += ".untyped";
			ofstream domfile(nm.c_str());
			domfile << *top_thing;
		};
	}
    // Output the errors from all input files
    current_analysis->error_list.report();
    delete yfl;

    for(int i = 2;i < argc;++i)
    {
    	yfl = new yyFlexLexer;
    	ifstream problem_in_stream(argv[i]);
    	cout << "Processing file: " << argv[i] << "\n";
    	if (problem_in_stream.bad())
		{
		    cout << "Failed to open\n";
		    // Log an error to be reported in summary later
		    line_no= 0;
		    log_error(E_FATAL,"Failed to open file");
		}
		else
		{
		    line_no= 1;

		    // Switch the tokeniser to the current input stream
		    yfl->switch_streams(&problem_in_stream,&cout);
		    yyparse();
		    
		    if (top_thing)
		    {
		    	string nm(argv[2]);
		    	nm += ".untyped";
		    	ofstream probfile(nm.c_str());
		    	probfile << *top_thing;
		    };
		}
	}
}
