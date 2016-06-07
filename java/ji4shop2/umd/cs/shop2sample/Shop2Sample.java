package umd.cs.shop2sample;
import umd.cs.shop2.*;
import java.util.*;

/**
 * This is a sample program that uses SHOP2 as part of some
 * computation.  Specifically, it creates plans for two tasks using a
 * single domain and problem state and reports on the combined length
 * of the plans.  It is intended to illustrate how a Java program can
 * interact with the Java Interface for SHOP2.
 *
 * @author murdock
 * @version 0.1, 2003-04-21 */
public final class Shop2Sample {
    static final String DOMAINFILE = "sample/d2.lisp";

    static final String TASKA = "(T1)";
    static final String TASKB = "(T2)";

    public static void main(String[] args)
    {
	int countA, countB;
	State startState;
	TaskAtom taskAtomA = new TaskAtom(TASKA); 
	TaskAtom taskAtomB = new TaskAtom(TASKB); 
	Shop shopA, shopB;
	SampleQuery sQuery = new SampleQuery();
	String[] facts = {
	    "(bar)",
	    "(baz)"
	};

	Shop.setVerbosity(3); /* replace 3 with 0 to turn off verbose output */

	startState = makeState(facts);
	shopA = new Shop(DOMAINFILE, startState, taskAtomA);
	countA = countNodes(shopA.getTree());

	System.out.println("Count A: " + Integer.toString(countA));

	/* Note: you must get done using shopA completely before you
	   can do anything with shopB; otherwise SHOP2 will get confused. */

	startState.addElement(new LogicalAtom("(foo)"));
	shopB = new Shop(DOMAINFILE, startState, taskAtomB, "", 
			 ":explanation t", sQuery);
	countB = countNodes(shopB.getTree());

	Shop.kill();

	System.out.println("Count B: " + Integer.toString(countB));
	System.out.println("Total Count: " + Integer.toString(countA+countB));
    }


    static State makeState(String[] strings) {
	State newState = new State();
	for (int i = 0; i < strings.length; i++)
	    newState.addElement(new LogicalAtom(strings[i]));
	return newState;
    }

    static int countNodes(TaskNode tnode) {
	int count = 0;
	
	if (tnode.atom().isPrimitive())
	    count = 1;
	else {
	    Vector children = tnode.children();
	    Enumeration e = children.elements();

	    while(e.hasMoreElements())
		{
		    TaskNode n;
		    n = (TaskNode)e.nextElement();
		    count += countNodes(n);
		}

	}

	return count;
    }
}
