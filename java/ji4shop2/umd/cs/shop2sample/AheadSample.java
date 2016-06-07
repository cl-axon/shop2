package umd.cs.shop2sample;
import umd.cs.shop2.*;
import java.util.*;

/**
 * This sample program illustrates how AHEAD will use SHOP2.
 *
 * @author murdock
 * @version 0.1, 2003-04-21 */
public final class AheadSample {
    static final String DOMAINFILE = "ahead/RussianMafiyaIndustryTakeover-tmk.lisp";
    static final String SAMPLETASK = "(RussianMafiyaIndustryTakeover)";

    public static void main(String[] args)
    {
	String[] hypothesis = {
	    "(isa UID101 RussianMafiyaIndustryTakeover)",
	    "(targetedIndustry UID101 UID201)",
	    "(perpetrator UID101 UID202)",

	    "(isa UID102 IndustryTakeoverMurderForHire)",
	    "(subEvents UID101 UID102)",

	    "(isa UID102 MurderForHire)",
	    "(victimIntended UID102 UID206)",
	    "(hitContractor UID102 UID204)",
	    "(hitman UID102 UID205)",

	    "(employees UID203 UID206)",
	    "(hasMembers UID201 UID203)",
	    "(hasMembers UID202 UID204)",
	    "(isa UID202 MafiyaGroup-Russian)",

	    "(isa UID103 EMailSending)",
	    "(dateOfEvent UID103 \"02/15/03\")",
	    "(senderOfInfo UID103 UID204)",
	    "(recipientOfInfo UID103 UID205)",

	    "(isa UID104 Observing)",
	    "(dateOfEvent UID104 \"02/27/03\")",
	    "(objectsObserved UID104 UID206)",
	    "(perpetrator UID104 UID205)",

	    "(isa UID105 Murder)",
	    "(dateOfEvent UID105 \"03/01/03\")",
	    "(victimIntended UID105 UID206)",
	    "(perpetrator UID105 UID205)",
	    "(deviceTypeUsed UID105 Rifle)",
	    "(eventOccursAt UID105 CityOfSaratovRussia)",

	    "(isa UID106 MakingAPhoneCall)",
	    "(dateOfEvent UID106 \"03/01/03\")",
	    "(callerNumber UID106 UID301)",
	    "(receiverNumber UID106 UID302)",
	    "(agentPhoneNumber UID205 UID301)",
	    "(agentPhoneNumber UID204 UID302)"
	};

	int countA, countB;
	State startState;
	Shop sh;
	TaskAtom mainTask = new TaskAtom(SAMPLETASK); 
	SampleQuery sQuery = new SampleQuery();
	Shop.setVerbosity(3);

	startState = makeState(hypothesis);

	sh = new Shop(DOMAINFILE, startState, mainTask, "ahead/ahead.lisp", 
		      ":explanation t", sQuery);
	
	printTrace(sh.getTree());

	Shop.kill();
    }

    static State makeState(String[] strings) {
	State newState = new State();
	for (int i = 0; i < strings.length; i++)
	    newState.addElement(new LogicalAtom(strings[i]));
	return newState;
    }

    static void printTrace(TaskNode tnode) {
	System.out.println("**** START OF TRACE ****");
	System.out.println();
	printTraceElements(tnode);
	System.out.println("**** END OF TRACE ****");
    }

    static void printTraceElements(TaskNode tnode) {
 	TaskAtom atom;
	atom = tnode.atom();
	if (atom.isPrimitive()) {
	    System.out.println(atom.taskString());
	    System.out.println();
	}
	else {
	    Vector children = tnode.children();
	    Enumeration e = children.elements();

	    while(e.hasMoreElements())
		printTraceElements((TaskNode)e.nextElement());
	}
    }
}
