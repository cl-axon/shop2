#ifndef MY__EXCEPTIONS
#define MY__EXCEPTIONS

struct BadOperator : public exception {

	const char * what() const throw()
	{
		return "Bad operator in plan!";
	};
};

struct SyntaxTooComplex : public exception {

	const char * what() const throw()
	{
		return "Syntax not handled by this validator!";
	};
};

struct UnrecognisedCondition : public exception {

	const char * what() const throw()
	{
		return "Unrecognised exception - unexpected situation!";
	};
};

struct BadAccessError : public exception {

	const char * what() const throw()
	{
		return "Attempt to access undefined numeric expression!";
	};
};

struct ParseFailure : public exception {

	const char * what() const throw()
	{
		return "Parser failed to read domain!\n";
	};
};

struct TypeException : public exception {

	const char * what() const throw()
	{
		return "Error in type-checking!\n";
	};
};
#endif
