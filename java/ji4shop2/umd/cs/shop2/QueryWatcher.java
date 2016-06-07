package umd.cs.shop2;

/**
 * Watcher for queries issued by SHOP2 to be performed by Java.
 * Useful when information about the state is to large to conveniently
 * send to SHOP2 all at once.
 */

public interface QueryWatcher {

    /* Runs a query and returns the resulting bindings and source.
     *  @param Query Lisp-like knowledge query consisting of nested
     *  AND's, NOT's, and logical assertions that may include some
     *  variables (beginning with question marks). E.g., 
     * (AND (ON ?A BOX1) (NOT (AND (ON ?B BOX1) (ON BOX1 BOX2))))
     *
     * @return Lisp-like attributions and bindings.  For example,
     * consider a query with two responses, one with attribution
     * (UID1000 PoliceReport) that binds ?A to to UID1001 and ?B to
     * UID1002, the other with attribution (UID2000 PoliceReport) that
     * binds ?A to to UID2001 and ?B to UID2002.  This query would
     * return the string:
     * (((UID1000 PoliceReport) (?A UID1001) (?B UID1002)) ((UID2000 PoliceReport) (?A UID2001) (?B UID2002)))
     * A failed query returns the string ().
     */
    public String processQuery(String query);
}
