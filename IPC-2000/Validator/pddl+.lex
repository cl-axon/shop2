%{

#include <string>


%}

char [a-zA-Z_]
digit [0-9]
int -?{digit}*
float -?{digit}+(\.{digit}*)?
string {char}+(-|{char}|{digit})*
whitespace [ \t]+
nl \n
comment ;.*$

%%
"(" {return OPEN_BRAC;}
")" {return CLOSE_BRAC;}
"[" {return OPEN_SQ;}
"]" {return CLOSE_SQ;}
":requirements" {return REQS;}
":equality" {return EQUALITY;}
":strips" {return STRIPS;}
":adl" {return ADL;}
":typing" {return TYPING;}
":disjunctive-preconditions" {return DISJUNCTIVE_PRECONDS;}
":existential-preconditions" {return EXT_PRECS;}
":universal-preconditions" {return UNIV_PRECS;}
":quantified-preconditions" {return QUANT_PRECS;}
":conditional-effects" {return COND_EFFS;}
":fluents" {return FLUENTS;}
":time" {return TIME;}
":constants" {return CONSTANTS;}
":predicates" {return PREDS;}
":functions" {return FUNCTIONS;}
":types" {return TYPES;}
":durative-actions" {return DURATIVE_ACTIONS;}
":duration-inequalities" {return DURATION_INEQUALITIES;}
":continuous-effects" {return CONTINUOUS_EFFECTS;}
":negative-preconditions" {return NEGATIVE_PRECONDITIONS;}
"define" {return DEFINE;}
"domain" {return DOMAIN;}
":action" {return ACTION;}
":process" {return PROCESS;}
":event" {return EVENT;}
":durative-action" {return DURATIVE_ACTION;}
":parameters" {return ARGS;}
":precondition" {return PRE;}
":condition" {return CONDITION;}
":start-precondition" {return START_PRE;}
":end-precondition" {return END_PRE;}
"at start" {return AT_START;}
"at end" {return AT_END;}
"over all" {return OVER_ALL;}
":effect" {return EFFECTS;}
":initial-effect" {return INITIAL_EFFECT;}
":final-effect" {return FINAL_EFFECT;}
":invariant" {return INVARIANT;}
":duration" {return DURATION;}
"and" {return AND;}
"or" {return OR;}
"exists" {return EXISTS;}
"forall" {return FORALL;}
"imply" {return IMPLY;}
"not" {return NOT;}
"when" {return WHEN;}
"either" {return EITHER;}
"problem" {return PROBLEM;}
":domain" {return FORDOMAIN;}
":objects" {return OBJECTS;}
":init" {return INITIALLY;}
":goal" {return GOALS;}
"=" {return EQ;}
":length" {return LENGTH;}
":serial" {return SERIAL;}
":parallel" {return PARALLEL;}
":metric" {return METRIC;}
"minimize" {return MINIMIZE;}
"maximize" {return MAXIMIZE;}
"#t" {return HASHT;}
"duration" {return DURATION_VAR;}
"total-time" {return TOTAL_TIME;}
"increase"   {return INCREASE;}
"decrease"   {return DECREASE;}
"scale-up"   {return SCALE_UP;}
"scale-down" {return SCALE_DOWN;}
"assign"     {return ASSIGN;}
"+" {return PLUS;}
"-" {return HYPHEN;}
"*" {return MUL;}
"/" {return DIV;}
">" {return GREATER;}
">=" {return GREATEQ;}
"<" {return LESS;}
"<=" {return LESSEQ;}
"?" {return Q;}
":" {return COLON;}

{string} {unsigned int i; 
          yylval.cp = new char[strlen(yytext)+1];
          strcpy(yylval.cp,yytext);
	  for(i = 0;i<strlen(yylval.cp);i++)
	      yylval.cp[i] = tolower(yylval.cp[i]);
	  // If symbol is registered as a function symbol,
	  // return token FUNCTION_SYMBOL else return NAME
	  if (current_analysis->func_tab.symbol_probe(yylval.cp) != NULL)
	      return FUNCTION_SYMBOL;
	  else
	      return NAME;}

{whitespace} ;
{comment} ;
{nl} {line_no++;};

{int} {yylval.ival = atoi(yytext);return (INTVAL);}
{float} {yylval.fval = atof(yytext);return (FLOATVAL);}

%% 

int yywrap()
{
	return 1;
};
