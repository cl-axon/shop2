#include "Environment.h"

map<Validator *,vector<Environment *> > Environment::copies 
						= map<Validator *,vector<Environment*> >();
