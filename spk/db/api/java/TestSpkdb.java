import rfpk.spk.spkdb.*;
import java.sql.*;

public class TestSpkdb {
    public static void main(String args[]) {
	String username = "air";
	String password = "codered";
	String firstName = "Mike";
	String surname = "Jordan";
	final int maxTests = 20;
	String xmlSource = "<spksource></spksource>";

	boolean b = true;
	boolean target = true;
	String s = "connection";
	int i = 1;

	long userId = 0;
	long jobId = 0;
	long newerJobId = 0;
	long newestJobId = 0;
	
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
		    target = true;
		    s = "newJob";
		    jobId = Spkdb.newJob(conn, 
					      userId,
					      "Abstract: Job 1",
					      33,
					      "1.01",
					      44,
					      "1.4.3",
					      xmlSource);
		    b = jobId != 0;
		    s += ": job number " + jobId;
		    break;
		case 11:
		    target = true;
		    s = "jobStatus";
		    rs = Spkdb.jobStatus(conn, jobId);
		    if (rs.next()) {
			String name = rs.getString("state_code");
			b = name.compareTo("q2c") == 0;
		    } 
		    else {
			s += ": no record for jobId=" + jobId;
			b = false;
		    } 
		    break;
		case 12:
		    Thread.sleep(1000);
		    target = true;
		    s = "newJob";

		    newerJobId = Spkdb.newJob(conn, 
					      userId,
					      "Abstract: job 2",
					      33,
					      "1.01",
					      44,
					      "1.4.3",
					      xmlSource);
		    b = newerJobId != 0;
		    s += ": job number " + newerJobId;

		    break;
		case 13:
		    Thread.sleep(1000);
		    target = true;
		    s = "newJob";
		    newestJobId = Spkdb.newJob(conn, 
					      userId,
					       "Abstract: job 3",
					      33,
					      "1.01",
					      44,
					      "1.4.3",
					      xmlSource);
		    b = newestJobId != 0;
		    s += ": job number " + newestJobId;
		    break;
		case 14:
		    target = true;
		    s = "userJobs, maxNum = 1";
		    rs = Spkdb.userJobs(conn, userId, 1);
		    if (rs.next()) {
			b = rs.getLong("job_id") == newestJobId;
		    } 
		    else {
			s += ": no record for userId = " + userId;
			b = false;
		    } 

		    break;
		case 15:
		    b = target = true;
		    s = "userJobs, maxNum = 3";
		    rs = Spkdb.userJobs(conn, userId, 3);
		    long jj = newestJobId;

		    while (rs.next()) {
			long j;
			if ((j = rs.getLong("job_id")) != jj--) {
			    s += "; jobId" + j + " is out of order";
                            b = false;
			    break;
			}
		    }
		    s += "; " + (newestJobId - jj) + " were returned";
		    break;
		case 16:
		    b = target = true;
		    s = "endJob";
		    b = Spkdb.endJob(conn, newerJobId, "srun", "job report");
		    break;
		case 17:
		    b = target = false;
		    s = "endJob";
		    b = Spkdb.endJob(conn, newerJobId, "xxxx", "job report");
		    break;
		case 18:
		    target = true;
		    s = "jobReport";
		    String report = Spkdb.jobReport(conn, newerJobId);
		    b = report.compareTo("job report") == 0;
		    break;
		case 19:
		    target = false;
		    s = "jobReport";
		    report = Spkdb.jobReport(conn, newestJobId);
		    b = report.compareTo("job report") == 0;
		    break;
		default:
		    break;
		}
	    } catch (Exception e) {
		s += " >> " + e;
		b = false;
	    }
	    if (i <= maxTests)
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
