Welcome to SHOP2!  This distribution contains the following files:

shop2.lisp      The SHOP2 program; at the top of the program file
                is the SHOP2 license.  PLEASE COMPILE SHOP2.LISP BEFORE
                YOU RUN THE EXAMPLES.  To compile the file in Allegro Lisp,
                use the following command:

                    (compile-file "shop2.lisp")

                Then it will generate a file named "shop2.fasl".

install.lisp    This script compiles shop2.lisp into a
                form that both loads and runs faster.  It uses a
                function called "compile-file", which is available
                in Allegro Lisp 6.0.

shop2-v11.pdf   The SHOP2 documentation (in Adobe Acrobat format)

shop2-v11.doc   The SHOP2 documentation (in Microsoft Word format)

ji4shop2/       The Java interface for SHOP2.

examples/       Example domains in seperate subdirectores:

      toy/        Some very simple toy examples

      logistics/  A simple logistics planning domain

      blocks/     A relatively sophisticated encoding of the traditional
                  blocks-world planning domain

                See documentation files (e.g., README.txt) in the example
                directories for more information about those examples.

IPC-2000/       The logistics domain in the second international planning
                competition (at AIPS-2000).  This directory contains the
                solution validator provided in the third International
                Planning Competition at AIPS-2002.

Furthermore, we have implemented some domains in the third international
planning (at AIPS-2002).  You might download them separately from SHOP2
homepage.


Remark:

If you are using Allegro Lisp, and you encounter the following error message

    Error: Stack overflow (signal 1000)
      [condition type: SYNCHRONOUS-OPERATING-SYSTEM-SIGNAL]

Then please use the following command to increase the stack limit.

    (sys:set-stack-cushion 150000000)


