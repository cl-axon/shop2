# SHOP2

Welcome to SHOP2!

#### Contents

* [About](#about-)
* [Files](#files-)
* [Future Plans](#future-plans-)
* [Reporting Issues](#reporting-issues-)
* [History](#history-)
  * [Changes in Shop2 1.3](#changes-in-shop2-1-3-)
  * [Git Fork](#git-fork-)


## About [&#x219F;](#contents)

TBD


## Files [&#x219F;](#contents)

This distribution contains the following files:

* ``shop2.lisp``  The SHOP2 program; at the top of the program file
                  is the SHOP2 license

* ``state-utils.lisp`` Additional source code for SHOP2.  A first step in decomposing SHOP2 into mutliple files.

* ``shop2-<foo>.pdf``  The SHOP2 documentation (in Adobe Acrobat format)

* ``shop2-<foo>.doc``  The SHOP2 documentation (in MS Word format)

* ``install.lisp`` This script compiles shop2.lisp into a
                   form that both loads and runs faster.  It uses a
                   function called "compile-file", which is available
                   in Allegro Lisp 6.0.
                   This file is largely obsolete now.  You should
                   probably be using ASDF to laod the system instead.

* ``shop2.asd``   ASDF system definition for SHOP2 system.  This is now
                  the preferred means of loading shop2.  Other methods
                  will soon be deleted unless someone else is interested
                  in maintaining them.

* ``shop2.system`` MK-DEFSYSTEM system definition for SHOP2.  This
                   definition is less obsolete than INSTALL.LISP, but
                   is also falling into bit rot and will no longer be
                   maintained unless someone volunteers.

* ``ji4shop2/``   The Java interface for SHOP2.

* ``examples/``   Example domains in seperate subdirectores:

* ``depots/``    The Depots domain from the third international planning competition (at AIPS-2002)
* ``toy/``       Some very simple toy examples
* ``logistics/`` A simple logistics planning domain
* ``blocks/``    A relatively sophisticated encoding of the traditional blocks-world planning domain

See documentation files (e.g., README.txt) in the example directories for more information about those examples.

* ``IPC-2000/``   The logistics domain in the second international planning
                  competition (at AIPS-2000).  This directory contains the
                  solution validator provided in the third International
                  Planning Competition at AIPS-2002.
                  [This directory should probably have been placed in the examples.]


## Future Plans [&#x219F;](#contents)

This is a release candidate for SHOP2 (lisp version), version 1.3.
This is primarily a bugfix release to follow on to SHOP2 1.2.  We
expect that this will be the last release in SHOP2 1.x, aside from
bugfixes, and that further effort will move to a new version 2.x that
will have a substantially new architecture to better support modular,
object-oriented extensions to SHOP2 and integrating SHOP2 into larger
applications.


## Reporting Issues [&#x219F;](#contents)

We would be particularly interested in getting bug reports (or, better
yet, patches!) from people who have tried to use SHOP2 with Lisps
other than Allegro and SBCL and on platforms other than Linux.  We
would also be interested in hearing from people who have run the
regression test suite.


## History [&#x219F;](#contents)


### Changes in SHOP2 1.3 [&#x219F;](#contents)

There are two substantial changes from SHOP2 1.2 to 1.3:

1. The new default loading method is to use the ASDF (Another System
Definition Facility) system loader.  Those unfamiliar with ASDF can
see the web page http://www.cliki.net/asdf for more details.  However,
it is very likely that if you have a modern Common Lisp
implementation, ASDF is already distributed with your common lisp.  A
good first test in loading SHOP2 is to do the following:

  1. make a symbolic link from your asdf system definition file (this will be system dependent) to shop2.asd.  Do NOT copy the file; link it. If you don't know where this is, you can try starting up your Lisp implementation and doing the following: ``(require :asdf)`` [if this fails, you need to install asdf, see the above website] and ``asdf:*central-registry*`` [this should print a list of directories to hold asd links.]
  1. start your lisp compiler
  1. ``(require :asdf``) --- if this doesn't work, obtain and install a copy of asdf, using the above web site.
  1. ``(asdf:oos 'asdf:load-op :shop2)`` If you are lucky (users of up-to-date ACL and SBCL may expect to be lucky), this will Just Work.

1. There is now an extensive regression test suite for SHOP2.  To run the regression test suite, you may type: ``(asdf:oos 'asdf:test-op :shop2)`` **WARNING**:  this may take a couple of days to finish!  This runs all the domain descriptions distributed with SHOP2, and checks the results against saved plans.


### Git Fork [&#x219F;](#contents)

This git fork was created from the SHOP2 SourceForce CVS repository using the following command:

```bash
$ git cvsimport -v \
    -d:pserver:anonymous@shop.cvs.sourceforge.net:/cvsroot/shop \
    -C . shop2
```
