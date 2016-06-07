#!/bin/sh

if [ $# -eq 0 ]
then


  for f in \
LOGISTICS-4-0 \
LOGISTICS-4-1 \
LOGISTICS-4-2 \
LOGISTICS-5-0 \
LOGISTICS-5-1 \
LOGISTICS-5-2 \
LOGISTICS-6-0 \
LOGISTICS-6-1 \
LOGISTICS-6-2 \
LOGISTICS-6-3 \
LOGISTICS-7-0 \
LOGISTICS-7-1 \
LOGISTICS-8-0 \
LOGISTICS-8-1 \
LOGISTICS-9-0 \
LOGISTICS-9-1 \
LOGISTICS-10-0 \
LOGISTICS-10-1 \
LOGISTICS-11-0 \
LOGISTICS-11-1 \
LOGISTICS-12-0 \
LOGISTICS-12-1 \
LOGISTICS-13-0 \
LOGISTICS-13-1 \
LOGISTICS-14-0 \
LOGISTICS-14-1 \
LOGISTICS-15-0 \
LOGISTICS-15-1
  do
    ../../Validator/validate domain.pddl prob${f}.pddl prob${f}.soln
    echo ---------------------------------------------
  done

else
  ../../Validator/validate -v domain.pddl ${1}.pddl ${1}.soln
fi

