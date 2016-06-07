CC = gcc
JAVAC=javac
DIR=umd/cs/shop2/
LDIR=umd/cs/javalisp/
SDIR=umd/cs/shop2sample/

.SUFFIXES: .java .class

all: $(LDIR)ProcessOutputThread.class $(LDIR)Javalisp.class $(DIR)QueryWatcher.class $(DIR)LogicalAtom.class $(DIR)State.class $(DIR)TaskNode.class $(DIR)TaskAtom.class $(DIR)Shop.class $(SDIR)Shop2Sample.class

.java.class:
	$(JAVAC) -d . $*.java

doc/index.html:  $(LDIR)ProcessOutputThread.java $(LDIR)Javalisp.java $(DIR)QueryWatcher.java $(DIR)LogicalAtom.java $(DIR)State.java $(DIR)TaskAtom.java $(DIR)TaskNode.java $(DIR)Shop.java
	javadoc -private -d doc umd.cs.shop2 umd.cs.javalisp

clean:
	- rm *~ $(DIR)*~ $(DIR)*.class $(LDIR)*~ $(LDIR)*.class $(SDIR)*.class $(SDIR)*~ tolisp.txt tojava.txt
