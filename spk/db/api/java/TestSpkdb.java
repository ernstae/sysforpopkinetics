import rfpk.spk.spkdb.*;
import java.sql.*;

public class TestSpkdb {
    public static void main(String args[]) {
	String username = "air";
	String password = "codered";
	String firstName = "Mike";
	String surname = "Jordan";
	final int maxTests = 11;

	boolean b = true;
	boolean target = true;
	String s = "connection";
	int i = 1;

	long userId = 0;
	
	Connection conn;

	try {
	    conn = Spkdb.connect("spktest", "localhost.localdomain", "tester", "tester");
	} catch (Exception e) {
	    s += " >> " + e;
	    b = false;
	    ok(b, i, s);
	    return;
	}
	ok(b, i, s);

	for (i = 2; i <= maxTests; i++) {
	    try {
		switch (i) {
		case 2:
		    target = false;
		    s = "newUser";
		    {
			String n[] = {"username", "first_name", "surname"};
			String v[] = { username,  firstName, surname };
			long r = Spkdb.newUser(conn, n, v);		    
			b = r == 1;
		    }
		    break;
		case 3:
		    target = false;
		    s = "newUser";
		    {
			String n[] = {"password", "first_name", "surname"};
			String v[] = { password,  firstName, surname };
			long r = Spkdb.newUser(conn, n, v);		    
			b = r == 1;
		    }
		    break;
		case 4:
		    target = true;
		    {
			String n[] = {"username", "password", "first_name", "surname"};
			String v[] = { username,  password, firstName, surname };
			s = "newUser";
			userId = Spkdb.newUser(conn, n, v);		    
			s += " is user number " + userId;
			b = userId == 1;
		    }
		    break;
		case 5:
		    target = false;
		    s = "newUser";
		    {
			String n[] = {"username", "password", "first_name", "surname"};
			String v[] = { username,  password, firstName, surname };
			long r = Spkdb.newUser(conn, n, v);		    
			b = r == 1;
		    }
		    break;
		case 6:
		    target = true;
		    s = "updateUser";
		    {
			String n[] = {"first_name", "surname"};
			String v[] = {"Gerry",       "Peyton" };
			b = Spkdb.updateUser(conn, userId, n, v);
		    }
		    break;
		case 7:
		    target = false;
		    s = "updateUser";
		    {
			String n[] = {"username", "first_name"};
			String v[] = {"glove",    "Gary"      };
			b = Spkdb.updateUser(conn, userId, n, v);
		    }
		    break;
		case 8:
		    target = false;
		    s = "updateUser";
		    {
			String n[] = {"first_name", "username"};
			String v[] = {"Gary",       "glove"   };
			b = Spkdb.updateUser(conn, userId, n, v);
		    }
		    break;
		case 9:
		    target = true;
		    s = "getUser";
		    ResultSet rs = Spkdb.getUser(conn, userId);
		    if (rs.next()) {
			String name = rs.getString("first_name");
			b = name.compareTo("Gerry") == 0;
		    } 
		    else {
			s += ": no record for userId=" + userId;
			b = false;
		    } 
		    break;
		case 10:
		    target = false;
		    s = "newJob";
		    long jobId = Spkdb.newJob(conn, 
					      userId,
					      "abstract",
					      33,
					      "1.01",
					      44,
					      "1.4.3",
					      "nonexistantfile");
		    b = jobId != 0;
		    s += " is job number " + jobId;
		    break;
		case 11:
		    target = true;
		    s = "newJob";
		    jobId = Spkdb.newJob(conn, 
					      userId,
					      "abstract",
					      33,
					      "1.01",
					      44,
					      "1.4.3",
					      "xmlSource");
		    b = jobId != 0;
		    s += " is job number " + jobId;
		    break;
		}
	    } catch (Exception e) {
		s += " >> " + e;
		b = false;
	    }
	    ok(b == target, i, s);
	}
	try {
	    b = Spkdb.disconnect(conn);
	} catch (Exception e) {
	    s += " >> " + e;
	    b = false;
	}
	ok(b, i, "disconnect");
    }
    private static void ok(boolean b, int i, String m) {
	String s = b ? "ok:\t" : "not ok:\t";
	s += i + " - " + m;
	System.out.println(s);
    }
}
