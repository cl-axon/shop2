package umd.cs.shop2sample;
import umd.cs.shop2.*;

/**
 * A sample query mechanism that simply returns fixed strings for a couple of
 * hard-coded queries.
 *
 * @author murdock
 * @version 0.1, 2003-04-21 */
public final class SampleQuery implements QueryWatcher {
    public String processQuery(String query) {
	String retval;
	System.out.println("Processing Query: " + query);

	if (query.equalsIgnoreCase("(and (employees #:?BUSINESS166 UID206) (hasMembers UID201 #:?BUSINESS166) (isa UID202 MafiyaGroup-Russian))"))
	    retval = "(((UID6147 PoliceOrganization) ((#:?BUSINESS166 UID203))))";
	else if (query.equalsIgnoreCase("(AND (FOO))"))
	    retval = "(((UID101 FooOrganization)))";
	else
	    retval = "()";

	System.out.println("Query Result: " + retval);
	return retval;
    }
}
