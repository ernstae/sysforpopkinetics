import rfpk.spk.spkdb.*;
import java.sql.*;

public class TestSpkdb {
    public static void main(String args[]) {
	String username = "jordabble";
	String password = "codered";
	String firstName = "Mike";
	String surname = "Jordan";
	final int maxTests = 3;

	boolean b = true;
	String s = "connection";
	int i = 1;

	String n[] = {"username", "password", "first_name", "surname"};
	String v[] = { username, password, firstName, surname };
	
	Connection conn;

	try {
	    conn = Spkdb.connect("spktest", "localhost.localdomain", "tester", "tester");
	} catch (Exception e) {
	    s += " >> " + e;
	    b = false;
	}
	ok(b, i, s);
	/*
	for (i = 2; i <= maxTests; i++) {

	    try {
		switch (i) {
		case 1: 
		    break;
		case 2:
		    s = "new user";
		    long r = Spkdb.newUser(conn, n, v);		    
		    s += " is user number " + r;
		    b = r == 1;
		    break;
		case 3:
		    s = "disconnect";
		    Spkdb.disconnect(conn);
		    break;
		}
	    } catch (Exception e) {
		s += " >> " + e;
		b = false;
	    }
	    ok(b, i, s);
	}
	*/
    
    }
    private static void ok(boolean b, int i, String m) {
	String s = b ? "ok:\t" : "not ok:\t";
	s += i + " - " + m;
	System.out.println(s);
    }
}
