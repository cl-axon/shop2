If you are using Allegro Lisp, and you encounter the following error message

    Error: Stack overflow (signal 1000)
      [condition type: SYNCHRONOUS-OPERATING-SYSTEM-SIGNAL]

Then please use the following command to increase the stack limit.

    (sys:set-stack-cushion 150000000)

