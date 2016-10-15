# SHOP2

Welcome to SHOP2!

#### Contents

* [About](#about-)
* [Git Fork](#git-fork-)
  * [Original Sources](#original-sources-)
  * [Branch Organization](#branch-organization-)
* [Installation](#installation-)
* [Usage](#usage-)
* [Papers](#papers-)
* [Files](#files-)
* [Future Plans](#future-plans-)
* [Reporting Issues](#reporting-issues-)
* [License](#license-)


## About [&#x219F;](#contents)

SHOP2 -- Simple Hierarchical Ordered Planner 2 -- is a domain-independent
planning system based on Hierarchical Task Network (HTN) planning. In the 2002
International Planning Competition, SHOP2 received one of the top four awards,
one of the two awards for distinguished performance.

For more information, see the [papers](#papers-) section below and the
[UMD SHOP site](https://www.cs.umd.edu/projects/shop/description.html).


## Git Fork [&#x219F;](#contents)

### Branch Organization [&#x219F;](#contents)

The git fork repo maintains the following branches of SHOP2:

* `cvs/mirror` - The original CVS repo hosted on Source Forge (preserved for
  historical and pristine comparison purposes)
* `cvs/mirror-with-updates` - Updates made to the CVS mirror (mostly a `README`
  and some updates for Quicklisp)
* `svn/mirror` - The mirror maintained by synching with the upstream git repo
* `svn/mirror+csv-history` - The upstream SVN repo mirror combined with the
  original CVS VCS history
* `master` - The CVS+SVN codebase+history with a top-level, Github-friendly
  `README` and changes fur using SBCL with Quicklisp


### Original Sources [&#x219F;](#contents)

This git fork was created from the SHOP2 SourceForce CVS repository using the
following command:

```bash
$ git cvsimport -v \
    -d:pserver:anonymous@shop.cvs.sourceforge.net:/cvsroot/shop \
    -C . shop2
```

That covered the years 2004-2006.

Furthermore, subsequent history was incorporated from the following repo:
 * https://svn.sift.info:3333/svn/shop2-public/shop/trunk/shop2

This `README` file is part of the fork and provided for users of the Github web
UI more than anything else. The actual project `README` is located in the
`docs` directory:
 * [docs/README.txt](docs/README.txt)

That file contains more detailed info, historical changes, etc.



## Installation [&#x219F;](#contents)

To install and load, QuickLisp is used. If you don't have QuickLisp installed,
you can do so with the following ``make`` target:

```
$ make quicklisp
```

This will help you get QuickLisp downloaded and installed for your Lisp, if you
haven't done so already. It will also set up the local QuickLisp ``shop2``
directory for you.

If you already have QuickLisp installed, you can set up the local directory
with the following:

```
$ make install
```

## Usage [&#x219F;](#contents)

To begin, start SBCL (e.g.), and use QuickLisp to load ``shop2``:

```cl
* (ql:quickload "shop2")
```
```
To load "shop2":
  Load 1 ASDF system:
    shop2
; Loading "shop2"
.............................

SHOP2 version 1.3 alpha with SIFT mods (May 26, 2005)
Copyright (C) 2002  University of Maryland.
Robert Goldman's modifications Copyright (C) 2004,2005 SIFT, LLC.
This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied.  This software is distributed under an
MPL/GPL/LGPL triple license.  For details, see the software source file.

("shop2")
*
```

To load an example:

```cl
* (load "examples/toy/basic-example.lisp")
```
```
Defining domain ...
Defining problem PROBLEM1 ...
---------------------------------------------------------------------------
Problem PROBLEM1 with :WHICH = :FIRST, :VERBOSE = :PLANS
Totals: Plans Mincost Maxcost Expansions Inferences  CPU time  Real time
           1     2.0     2.0          4          2     0.000      0.000
Plans:
(((!DROP BANJO) (!PICKUP KIWI)))

T
*
```


## Papers [&#x219F;](#contents)

* 2001 - [Total-Order Planning with Partially Ordered Subtasks](http://www.cs.umd.edu/~nau/papers/nau2001total-order.pdf) - First paper on SHOP2
* 2003 - [SHOP2: An HTN Planning System](http://www.cs.umd.edu/~nau/papers/nau2003shop2.pdf) - Describes features of SHOP2 that helped it excel in the 2002 International Planning Competition
* 2003 - [Automating DAML-S web services composition using SHOP2](http://www.cs.umd.edu/~nau/papers/wu2003automating.pdf) - Describes the use of SHOP2 to execute DAML-S web-service descriptions
* 2004 - [Forward-chaining planning in nondeterministic domains](http://www.cs.umd.edu/~nau/papers/kuter2004forward-chaining.pdf) - Describes how to adapt SHOP2 and several other planners to work in nondeterministic planning domains
* 2004 - [Applications of SHOP and SHOP2](http://www.cs.umd.edu/~nau/papers/nau2004applications.pdf) - Describes some of the applications for which people have used SHOP and SHOP2
* 2005 - [A hierarchical task-network planner based on symbolic model checking](http://www.cs.umd.edu/~nau/papers/kuter2005hierarchical.pdf) - Further work on adapting SHOP2 to work in nondeterministic problem domains
* 2005 - [Using domain-configurable search control for probabilistic planning](http://www.cs.umd.edu/~nau/papers/kuter2005using.pdf) - Describes how to adapt SHOP2 and several other planners to work in probabilistic planning domains such as MDPs
* 2005 - [Information gathering during planning for web service composition](http://www.cs.umd.edu/~nau/papers/kuter2005information.pdf) - Extension of SHOP2 to gather information from the web in order to do web-service composition
* 2005 - [Web service composition with volatile information](http://www.cs.umd.edu/~nau/papers/au2005web.pdf) - An extension of SHOP2 to do web service composition in environments when the world is changing while the planning is going on


## Reporting Issues [&#x219F;](#contents)

We would be particularly interested in getting bug reports (or, better
yet, patches!) from people who have tried to use SHOP2 with Lisps
other than Allegro and SBCL and on platforms other than Linux.  We
would also be interested in hearing from people who have run the
regression test suite.


## License

Copyright © 2002  University of Maryland.

Robert Goldman's modifications Copyright © 2004,2005 SIFT, LLC.

This software is distributed under an MPL/GPL/LGPL triple license.  For
details, see the software source file.

This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied.

