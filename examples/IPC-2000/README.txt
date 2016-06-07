It is an implementation of the Logisitics domain in SHOP2 syntax
for the logistics domain in the AIPS2000's planning competition.

The domain is implemented in the file "logistics.lisp".
prob*.lisp are the problems in SHOP2 format.
prob*.soln are the plans found by SHOP2.
validate.result is the result of the validator.

You can see that SHOP2 can solve all problems in a short time.
(see validate.result)

Here are some useful scripts:

- problem-converter.lisp

  It converts all problems in current directory into SHOP2 format.

- run.lisp

  It runs SHOP2 to solve all the problems in the current directory.
  It is depended on SHOP2.lisp, solution-converter.lisp, logistics.lisp

- validate.sh

  It calls the plan validator given in the AIPS2002's planning competition
  to validate all plans SHOP2 generated.
  It is depended on "validate", which is the plan validator given
  in the AIPS2002's planning competition .  You may need to recompile the
  validator for your own platform.

