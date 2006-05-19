Welcome to SHOP2!  This distribution contains the following files:

shop2.lisp  The SHOP2 program; at the top of the program file
            is the SHOP2 license

state-utils.lisp 
            Additional source code for SHOP2.  A first step in
	    decomposing SHOP2 into mutliple files.

shop2-<foo>.pdf   The SHOP2 documentation (in Adobe Acrobat format)

shop2-<foo>.doc	  The SHOP2 documentation (in MS Word format)

install.lisp    This script compiles shop2.lisp into a
                form that both loads and runs faster.  It uses a
                function called "compile-file", which is available
                in Allegro Lisp 6.0.

		This file is largely obsolete now.  You should
                probably be using ASDF to laod the system instead.

shop2.asd	ASDF system definition for SHOP2 system.  This is now
                the preferred means of loading shop2.  Other methods
                will soon be deleted unless someone else is interested
                in maintaining them.

shop2.system	MK-DEFSYSTEM system definition for SHOP2.  This
                definition is less obsolete than INSTALL.LISP, but
                is also falling into bit rot and will no longer be
                maintained unless someone volunteers.

ji4shop2/       The Java interface for SHOP2.

examples/   Example domains in seperate subdirectores:

 depots/    The Depots domain from the third international planning
            competition (at AIPS-2002)

 UMT2/	    The UM Translog 2 domain from the third international planning
            competition.

 toy/       Some very simple toy examples

 logistics/  A simple logistics planning domain

 blocks/     A relatively sophisticated encoding of the traditional
             blocks-world planning domain

    See documentation files (e.g., README.txt) in the example
    directories for more information about those examples.

IPC-2000/       The logistics domain in the second international planning
                competition (at AIPS-2000).  This directory contains the
                solution validator provided in the third International
                Planning Competition at AIPS-2002.

		[This directory should probably have been placed in the
                examples.]


 



