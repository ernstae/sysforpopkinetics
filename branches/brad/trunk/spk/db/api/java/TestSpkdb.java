import rfpk.spk.spkdb.*;
import java.sql.*;

public class TestSpkdb {
    public static void main(String args[]) {
	String host = args[0];
	String username = "air";
	String password = "codered";
	String firstName = "Mike";
	String surname = "Jordan";
        String email = "air@u.w.e";
	final int maxTests = 63;
	String xmlSource = "<spksource>\n\tline1\n\tline2\n</spksource>";
	boolean b = true;
	boolean target = true;
	String s = "connection";
	int i = 1;
	int count = 0;
 	long datasetId = 0;
	long newDatasetId = 0;
	long newerDatasetId = 0;
	long newestDatasetId = 0;
 	long modelId = 0;
	long newModelId = 0;
	long newerModelId = 0;
	long newestModelId = 0;
	long userId = 0;
	long jobId = 0;
        long groupId = 0;
	long newerJobId = 0;
	long newestJobId = 0;
	
	Connection conn;

	try {
	    conn = Spkdb.connect("spktest", host, "tester", "tester");
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
			String n[] = {"username", "password", "first_name", "surname", "email", "test"};
			String v[] = { username,  password, firstName, surname, email, "1" };
			s = "newUser";
			userId = Spkdb.newUser(conn, n, v);   
			s += " is user number " + userId;
			b = userId == 1;
                        String u[] = {"water", "secret", "unknown", "unknown", "water@u.w.e", "0"};
                        userId = Spkdb.newUser(conn, n, u);
                        b = b && userId == 2;
                        s += " and another new user is user number " + userId;
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
		    s = "updateUser";
		    {
			String n[] = {"first_name", "password"};
			String v[] = {"Alex",       "codeblue"   };
			b = Spkdb.updateUser(conn, userId, n, v);
		    }
		    break;
		case 10:
		    target = true;
		    s = "updateUser";
		    {
			String n[] = {"password",  "first_name"};
			String v[] = {"codegreen", "Gary"   };
			b = Spkdb.updateUser(conn, userId, n, v);
		    }
		    break;
		case 11:
		    target = true;
		    s = "getUser";
		    ResultSet rs = Spkdb.getUser(conn, username);
		    if (rs.next()) {
			String name = rs.getString("first_name");
			b = name.compareTo("Mike") == 0;
		    } 
		    else {
			s += ": no record for userId=" + userId;
			b = false;
		    } 
		    break;
		case 12:
		    target = true;
		    s = "newJob";
		    jobId = Spkdb.newJob(conn, 
					 userId,
					 "Abstract: Job 1",
					 33,
					 "1.01",
					 44,
					 "1.4.3",
					 xmlSource,
				         "fo",
					 0,
					 false,
                                         false,
                                         0);
		    b = jobId != 0;
		    s += ": job number " + jobId;
		    break;
		case 13:
		    target = true;
		    s = "getJob";
		    rs = Spkdb.getJob(conn, jobId);
		    if (rs.next()) {
			String name = rs.getString("state_code");
			b = name.compareTo("q2c") == 0;
		    } 
		    else {
			s += ": no record for jobId=" + jobId;
			b = false;
		    } 
		    break;
		case 14:
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
					      xmlSource,
					      "la",
					      0,
					      false,
                                              false,
                                              0);
		    b = newerJobId != 0;
		    s += ": job number " + newerJobId;

		    break;
		case 15:
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
					       xmlSource,
					       "eh",
					       0,
                                               false,
                                               false,
                                               0);
		    b = newestJobId != 0;
		    s += ": job number " + newestJobId;
		    break;
		case 16:
		    target = true;
		    s = "userJobs, maxNum = 1\n";
		    rs = Spkdb.userJobs(conn, userId, 1, 0, null, null, null, null, null);
		    if (rs.last()) {
			b = rs.getLong("job_id") == newestJobId;
			/*
			Blob report = rs.getBlob("xml_source");
			java.io.InputStream in = report.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    s += (char)c;
			}
			*/
		    } 
		    else {
			s += ": no record for userId = " + userId;
			b = false;
		    } 
		    
		    break;
		case 17:
		    b = target = true;
		    s = "userJobs, maxNum = 3";
 		    rs = Spkdb.userJobs(conn, userId, 3, newestJobId, null, null, null, null, null);
		    jobId = newestJobId;

		    count = 0;
                    rs.last();
		    do{
			long j;
			if ((j = rs.getLong("job_id")) >= jobId) {
			    s += "; jobId = " + j + " is out of order";
                            b = false;
			    break;
			}
			jobId = j;
			count++;
		    } while(rs.previous());
		    s += "; " + count + " were returned";
		    break;
		case 18:
		    b = target = true;
		    s = "endJob";
		    b = Spkdb.endJob(conn, newerJobId, "srun", "job report");
		    break;
		case 19:
		    b = target = false;
		    s = "endJob";
		    b = Spkdb.endJob(conn, newerJobId, "xxxx", "job report");
		    break;
		case 20:
		    target = true;
		    s = "getJob: test xml_source";
		    String source = new String();
		    rs = Spkdb.getJob(conn, newerJobId);
		    if (rs.next()) {
			Blob sourceBlob = rs.getBlob("xml_source");
			java.io.InputStream in = sourceBlob.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    source += (char)c;
			}
		    } 
		    else {
			s += ": no record for userId = " + userId;
			b = false;
		    } 
		    b = source.compareTo(xmlSource) == 0;
		    break;
		case 21:
		    target = true;
		    s = "getJob: test report";
		    String report = new String();
		    rs = Spkdb.getJob(conn, newerJobId);
		    if (rs.next()) {
			Blob reportBlob = rs.getBlob("report");
			if (reportBlob == null) {
			    s += ": no report available";
			    b = false;
			    break;
			}
			java.io.InputStream in = reportBlob.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    report += (char)c;
			}
		    } 
		    else {
			s += ": no record for userId = " + userId;
			b = false;
		    } 
		    b = report.compareTo("job report") == 0;
		    break;
		case 22:
		    target = false;
		    s = "getJob";
		    rs = Spkdb.getJob(conn, newestJobId);
		    report = new String();
		    if (rs.next()) {
			Blob reportBlob = rs.getBlob("report");
			if (reportBlob == null) {
			    s += ": no report available";
			    b = false;
			    break;
			}
			java.io.InputStream in = reportBlob.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    report += (char)c;
			}
		    } 
		    else {
			s += ": no record for userId = " + userId;
			b = false;
		    } 
		    b = report.compareTo("job report") == 0;
		    break;
		case 23:
		    target = true;
		    s = "newDataset";
		    datasetId 
			= Spkdb.newDataset(conn, userId, "T1", "Dataset T1", "1 2 4 3");
		    s += ": datasetId = " + datasetId;
		    b = datasetId > 0;
		    break;
		case 24:
		    target = false;
		    s = "newDataset";
		    newDatasetId 
			= Spkdb.newDataset(conn, userId, "T1", "Dataset: X", "1 5 4 3");
		    s += ": datasetId = " + newDatasetId;
		    b = newDatasetId > 0;
		    break;
		case 25:
		    target = true;
		    s = "getDataset";
		    rs = Spkdb.getDataset(conn, datasetId);
		    String archive = new String();
		    if (rs.next()) {
			Blob archiveBlob = rs.getBlob("archive");
			java.io.InputStream in = archiveBlob.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    archive += (char)c;
			}
		    } 
		    else {
			s += ": no record for datsetId = " + datasetId;
			b = false;
		    } 
		    b = archive.compareTo("1 2 4 3") == 0;
		    break;
		case 26:
		    target = true;
		    s = "updateDataset";
		    {
			String n[] = {"abstract"};
			String v[] = {"dataset T1"   };
			b = Spkdb.updateDataset(conn, datasetId, n, v);
		    }
		    break;
		case 27:
		    target = false;
		    s = "updateDataset";
		    {
			String n[] = {"dataset_id", "abstract"};
			String v[] = {"55", "dataset T2"   };
			b = Spkdb.updateDataset(conn, datasetId, n, v);
		    }
		    break;
		case 28:
		    target = false;
		    s = "updateDataset";
		    {
			String n[] = {"abstract", "dataset_id"};
			String v[] = {"dataset T2", "66"   };
			b = Spkdb.updateDataset(conn, datasetId, n, v);
		    }
		    break;
		case 29:
		    b = target = true;
		    s = "userDatasets, maxNum = 3";
		    newDatasetId
			= Spkdb.newDataset(conn, userId, "T2", "Dataset T2", "1 4 4 3");
		    newerDatasetId 
			= Spkdb.newDataset(conn, userId, "T3", "Dataset T3", "6 2 4 3");
		    newestDatasetId
			= Spkdb.newDataset(conn, userId, "T4", "Dataset T4", "1 2 4 8");
		    		    
 		    rs = Spkdb.userDatasets(conn, userId, 3, 0);
		    
		    count = 0;
                    rs.last();
		    do {
			long j = rs.getLong("dataset_id");
			if (count != 0 && j >= datasetId) {
			    s += "; datasetId" + j + " is out of order";
                            b = false;
			    break;
			}
			datasetId = j;
			count++;
		    } while (rs.previous());
		    s += "; " + count + " were returned";
		    break;
		case 30:
		    b = target = true;
		    s = "userDatasets, maxNum = 3";
                    datasetId = 4;
 		    rs = Spkdb.userDatasets(conn, userId, 3, datasetId);
		    count = 0;
                    rs.last();
		    do {
			long j;
			if ((j = rs.getLong("dataset_id")) >= datasetId) {
			    s += "; datasetId" + j + " is out of order";
                            b = false;
			    break;
			}
			datasetId = j;
			count++;
		    } while (rs.previous());
		    s += "; " + count + " were returned";
		    break;
		case 31:
		    target = true;
		    s = "newModel";
		    modelId 
			= Spkdb.newModel(conn, userId, "M1", "Model M1", "1 2 4 3");
		    s += ": modelId = " + modelId;
		    b = modelId > 0;
		    break;
		case 32:
		    target = false;
		    s = "newModel";
		    modelId 
			= Spkdb.newModel(conn, userId, "M1", "Model: X", "1 5 4 3");
		    s += ": modelId = " + modelId;
		    b = modelId > 0;
		    break;
		case 33:
		    target = true;
		    s = "getModel";
		    rs = Spkdb.getModel(conn, modelId);
		    archive = new String();
		    if (rs.next()) {
			Blob archiveBlob = rs.getBlob("archive");
			java.io.InputStream in = archiveBlob.getBinaryStream();
			int c;
			while ((c = in.read()) != -1) {
			    archive += (char)c;
			}
		    } 
		    else {
			s += ": no record for model_id = " + modelId;
			b = false;
		    } 
		    b = archive.compareTo("1 2 4 3") == 0;
		    break;
		case 34:
		    target = true;
		    s = "updateModel";
		    {
			String n[] = {"abstract"};
			String v[] = {"model M1"   };
			b = Spkdb.updateModel(conn, modelId, n, v);
		    }
		    break;
		case 35:
		    target = false;
		    s = "updateModel";
		    {
			String n[] = {"model_id", "abstract"};
			String v[] = {"55", "model M2"   };
			b = Spkdb.updateModel(conn, modelId, n, v);
		    }
		    break;
		case 36:
		    target = false;
		    s = "updateModel";
		    {
			String n[] = {"abstract", "model_id"};
			String v[] = {"model M2", "66"   };
			b = Spkdb.updateModel(conn, modelId, n, v);
		    }
		    break;
		case 37:
		    b = target = true;
		    s = "userModels, maxNum = 3";
		    newModelId
			= Spkdb.newModel(conn, userId, "T2", "Model T2", "1 4 4 3");
		    newerModelId 
			= Spkdb.newModel(conn, userId, "T3", "Model T3", "6 2 4 3");
		    newestModelId
			= Spkdb.newModel(conn, userId, "T4", "Model T4", "1 2 4 8");
		    		    
 		    rs = Spkdb.userModels(conn, userId, 3, 0);
		    
		    count = 0;
                    rs.last();
		    do {
			long j = rs.getLong("model_id");
			if (count != 0 && j >= modelId) {
			    s += "; modelId" + j + " is out of order";
                            b = false;
			    break;
			}
			modelId = j;
			count++;
		    } while (rs.previous());
		    s += "; " + count + " were returned";
		    break;
		case 38:
		    b = target = true;
		    s = "userModels, maxNum = 3";
                    modelId = 4;
 		    rs = Spkdb.userModels(conn, userId, 3, modelId);

		    count = 0;
                    rs.last();
		    do {
			long j;
			if ((j = rs.getLong("model_id")) >= modelId) {
			    s += "; modelId" + j + " is out of order";
                            b = false;
			    break;
			}
			modelId = j;
			count++;
		    } while (rs.previous());
		    s += "; " + count + " were returned";
		    break;
		case 39:
		    target = true;
		    s = "getEndTable";
		    rs = Spkdb.getEndTable(conn);
		    count = 0;
		    while (rs.next()) {
			count++;
		    }
		    b = count == 21;
		    break;
		case 40:
		    target = true;
		    s = "getMethodTable";
		    rs = Spkdb.getMethodTable(conn);
		    count = 0;
		    while (rs.next()) {
			count++;
		    }
		    b = count == 10;
		    break;
		case 41:
		    target = true;
		    s = "getStateTable";
		    rs = Spkdb.getStateTable(conn);
		    count = 0;
		    while (rs.next()) {
			count++;
		    }
		    b = count == 10;
		    break;
		case 42:
		    target = true;
		    s = "jobHistory";
		    rs = Spkdb.jobHistory(conn, 2);
		    count = 0;
		    while (rs.next()) {
			count++;
		    }
		    b = count == 2;
		    break;
                case 43:
		    target = true;
		    s = "setCheckpoint";
                    b = Spkdb.setCheckpoint(conn, 2L, "checkpoint");                   
                    break;
                case 44:
		    target = true;
		    s = "newJob(warm start)";
		    jobId = Spkdb.newJob(conn,
					 userId,
					 "Abstract: Job 4",
					 33,
					 "1.01",
					 44,
					 "1.4.3",
					 xmlSource,
					 "fo",
					 2,
					 true,
                                         true,
                                         0);
		    b = jobId != 0;
		    s += ": job number " + jobId;
		    break;
                case 45:
                    target = true;
                    s = "setStateCode";
                    b = Spkdb.setStateCode(conn, 3L, "q2r");
		    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = b && rs.getString("state_code").equals("q2r");
                    else
                        b = false;
                    break;
                case 46:
                    target = true;
                    s = "abortJob case 1 - q2c";
                    String state = Spkdb.abortJob(conn, 1L);                    
                    rs = Spkdb.getJob(conn, 1L);
                    if(rs.next())
                        b = b && rs.getString("state_code").equals("end")
                            && rs.getString("end_code").equals("abrt")
                            && state.equals("end");
                    else
	                b = false;
                    break;
		case 47:
                    target = true;
                    s = "abortJob case 2 - q2r";
                    state = Spkdb.abortJob(conn, 3L);
                    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = b && rs.getString("state_code").equals("end")
                            && rs.getString("end_code").equals("abrt")
                            && state.equals("end");
                    else
                        b = false;
                    break;
                case 48:
                    target = true;
                    s = "abortJob case 3 - cmp";
                    Spkdb.setStateCode(conn, 1L, "cmp");
                    state = Spkdb.abortJob(conn, 1L);
                    rs = Spkdb.getJob(conn, 1L);
                    if(rs.next())
                        b = b && rs.getString("state_code").equals("q2ac")
                            && state.equals("q2ac");
                    else
                        b = false;
                    break;
                case 49:
                    target = true;
                    s = "abortJob case 4 - run";
                    Spkdb.setStateCode(conn, 3L, "run");
                    state = Spkdb.abortJob(conn, 3L);
                    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = b && rs.getString("state_code").equals("q2ar")
                            && state.equals("q2ar");
                    else
                        b = false;
                    break;
                case 50:
                    target = true;
                    s = "abortJob case 5 - end";
                    state = Spkdb.abortJob(conn, 2L);
                    rs = Spkdb.getJob(conn, 2L);
                    if(rs.next())
                        b = rs.getString("state_code").equals("end")
                            && state == null;
                    else
                        b = false;
		    break;
                case 51:
                    target = true;
                    s = "abortJob case 6 - q2ac";
                    state = Spkdb.abortJob(conn, 1L);
                    rs = Spkdb.getJob(conn, 1L);
                    if(rs.next())
                        b = rs.getString("state_code").equals("q2ac")
                            && state == null;
                    else
                        b = false;
		    break;
                case 52:
                    target = true;
                    s = "abortJob case 7 - q2ar";
                    state = Spkdb.abortJob(conn, 3L);
                    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = rs.getString("state_code").equals("q2ar")
                            && state == null;
                    else
                        b = false;
		    break;
                case 53:
                    target = true;
                    s = "abortJob case 8 - acmp";
                    Spkdb.setStateCode(conn, 1L, "acmp");
                    state = Spkdb.abortJob(conn, 1L);
                    rs = Spkdb.getJob(conn, 1L);
                    if(rs.next())
                        b = rs.getString("state_code").equals("acmp")
                            && state == null;
                    else
                        b = false;
		    break;
                case 54:
                    target = true;
                    s = "abortJob case 9 - arun";
                    Spkdb.setStateCode(conn, 3L, "arun");
                    state = Spkdb.abortJob(conn, 3L);
                    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = rs.getString("state_code").equals("arun")
                            && state == null;
                    else
                        b = false;
		    break;
                case 55:
                    target = true;
                    s = "end-job mail notice 0";
                    rs = Spkdb.getJob(conn, 3L);
                    if(rs.next())
                        b = rs.getInt("mail") == 0;
                    else
                        b = false;
		    break;
                case 56:
                    target = true;
                    s = "end-job mail notice 1";
                    rs = Spkdb.getJob(conn, 4L);
                    if(rs.next())
                        b = rs.getInt("mail") == 1;
                    else
                        b = false;
		    break;
                case 57:
                    target = true;
                    s = "newGroup";
                    groupId = Spkdb.newGroup(conn, "testing");
                    b = groupId == 1;
                    break;
                case 58:
                    target = true;
                    s = "newGroupMember";
                    b = Spkdb.newGroupMember(conn, "air", groupId);
                    b = b && Spkdb.newGroupMember(conn, "water", groupId);
                    break;
                case 59:
                    target = true;
                    s = "getGroupUsers";
                    rs = Spkdb.getGroupUsers(conn, groupId);
                    rs.next();
                    b = rs.getString("username").equals("air");
                    rs.next();
                    b = b && rs.getString("username").equals("water");
                    break;
                case 60:
                    target = true;
                    s = "getUserById";
                    rs = Spkdb.getUserById(conn, 1);
                    rs.next();
                    b = rs.getString("username").equals("air");
                    rs = Spkdb.getUserById(conn, 2);
                    rs.next();
                    b = b && rs.getString("username").equals("water");
                    break;
                case 61:
                    target = true;
                    s = "setJobAbstract";
                    b = Spkdb.setJobAbstract(conn, 2, 1, "Abstract 1: updated");
                    rs = Spkdb.getJob(conn, 1);
                    rs.next();
                    b = b && rs.getString("abstract").equals("Abstract 1: updated");
                    break;
                case 62:
                    target = true;
                    s = "getEmailAddress";
                    rs = Spkdb.getEmailAddress(conn, "tester");
                    rs.next();
                    b = rs.getString("email").equals("air@u.w.e");
                    break;
                case 63:
                    target = true;
                    s = "setJobShareWith";
                    b = Spkdb.setJobShareWith(conn, 2, 3, 1);
                    rs = Spkdb.getJob(conn, 3);
                    rs.next();
                    b = b && rs.getLong("share_with") == 1;
                    break;
		default:
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

	System.out.println(okTests + " tests were ok out of a possible " + (maxTests + 1));
    }
    private static void ok(boolean b, int i, String m) {
	String s = b ? "ok:\t" : "not ok:\t";
	s += i + " - " + m;
	System.out.println(s);
	if (b)
	    okTests++;
    }
    static int okTests = 0;
}
