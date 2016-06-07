/*
  main() for the PDDL2.1 parser

  $Date: 2004/09/18 16:45:07 $
  $Revision: 1.1 $

  This expects any number of filenames as arguments, although
  it probably doesn't ever make sense to supply more than two.

  s.n.cresswell@durham.ac.uk

  Durham Planning Group
  http://www.dur.ac.uk/~dcs0www/research/stanstuff/planpage.html
 */

#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include "ptree.h"
#include <FlexLexer.h>

extern int yyparse();
extern int yydebug;

parse_category* top_thing=NULL;

analysis an_analysis;
analysis* current_analysis;
char * current_filename;

yyFlexLexer* yfl;

int main(int argc,char * argv[])
{
    current_analysis= &an_analysis;
    ifstream* current_in_stream;
    yydebug=0; // Set to 1 to output yacc trace 

    yfl= new yyFlexLexer;

    // Loop over given args
    for (int a=1; a<argc; ++a)
    {
	current_filename= argv[a];
	cout << "File: " << current_filename << '\n';
	current_in_stream= new ifstream(current_filename);
	if (current_in_stream->bad())
	{
	    // Output a message now
	    cout << "Failed to open\n";
	    
	    // Log an error to be reported in summary later
	    line_no= 0;
	    log_error(E_FATAL,"Failed to open file");
	}
	else
	{
	    line_no= 1;

	    // Switch the tokeniser to the current input stream
	    yfl->switch_streams(current_in_stream,&cout);
	    yyparse();

	    // Output syntax tree
	    if (top_thing) top_thing->display(0);
	}
	delete current_in_stream;
    }
    // Output the errors from all input files
    current_analysis->error_list.report();
    delete yfl;
}
