package rfpk.spk.spkdb;
import java.sql.*;
import java.util.*;
import java.util.regex.*;
import rfpk.spk.spkdb.*;
import java.io.*;
/**
defines a set of static methods which provide a java API 
for the Spk Database.  This class should never be instantiated.
 */

public abstract class Spkdb {
    private static Pattern pattern1;
    private static Pattern pattern2;
    /**
     Open a connection to a database.  This object must be passed as a parameter
     to other methods of this class.  A process may have several connections open
     at the same time.  When a connection is no longer used, it should be closed
     using the Spkdb.disconnect() method.
     @param dbName name of a database
     @param hostName domain name of host on which database resides
     @param dbUser username of a valid user of the database
     @param dbPassword user's password
     @return an object of a class that implements the java.sql.Connection interface
     @see #disconnect
     @see java.sql.Connection
     */
    public static Connection connect(String dbName, String hostName, String dbUser, String dbPassword) 
	throws SQLException, SpkdbException
    {
	String driverName = "com.mysql.jdbc.Driver";
	try {
	    Class.forName(driverName).newInstance();
	} catch (Exception e) {
	    throw new SpkdbException("Could not instantiate " + driverName); 
	}
	Connection conn = DriverManager.getConnection("jdbc:mysql://" +
						      hostName + "/" +
						      dbName + 
						      "?user=" + dbUser + 
						      "&password=" + dbPassword);
	return conn;
    }
    /**
       Close a database connection.
       @param conn open connection to a database
       @return true
       @see #connect
     */
    public static boolean disconnect(Connection conn) throws SQLException {
	conn.close();
	return true;
    }
    /**
       Submit a job.
       @param conn open connection to the database
       @param abstraction short description of the job
       @param datasetId key to a row in the dataset table
       @param datasetVersion rcs version code for the dataset
       @param modelId key to a rew in the model table
       @param modelVersion rcs version code for the model
       @param xmlSource source code for the job
       @return key to the new row in the job table
     */
    public static long newJob(Connection conn, 
			      long userId,
			      String abstraction,
			      long datasetId,
			      String datasetVersion,
			      long modelId,
			      String modelVersion,
			      String xmlSource)
	throws SQLException, SpkdbException, FileNotFoundException
    {
	long jobId = 0;
	java.util.Date date = new java.util.Date(); 
	long eventTime = date.getTime()/1000;
	long startTime = eventTime;
	String stateCode = "q2c";
	String sql = "insert into job (state_code, user_id, abstract, dataset_id, "
                                    + "dataset_version, model_id, model_version, "
                                    + "xml_source, start_time, event_time)"
                           + " values ('" + stateCode + "'," + userId + ",'" + abstraction + "'," + datasetId
                                   + ",'" + datasetVersion + "'," + modelId + ",'" + modelVersion
                                   + "', ?," + startTime + "," + eventTime + ");";
	PreparedStatement pstmt = conn.prepareStatement(sql);
	pstmt.setBinaryStream(1, new ByteArrayInputStream(xmlSource.getBytes()), xmlSource.length());
	pstmt.executeUpdate();
	ResultSet rs = pstmt.getGeneratedKeys();
	if (rs.next()) {
	    jobId = rs.getLong(1);
	}
	return jobId;
    }
    /**
       Get the status of a given job.
       @param conn open connection to the database
       @param jobId key to the given job in the job table
       @return Object of a class which implements the java.sql.ResultSet interface, 
       containing a subset of a single row of the job table.  The columns in this
       subset are state_code, event_time and end_code.
     */
    public static ResultSet jobStatus(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
	String sql = "select state_code, event_time, end_code from job where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Get the status of a set of jobs of a given user.
       @param conn open connection to the database 
       @param userId key to the given user in user table
       @param maxNum maximum number of jobs to provide status for
       @return Object of a class which implements the java.ResultSet interface, containing
       a sequence of subsets of rows of the job table.  Each subset contains the 
       following columns: job_id, abstact, state_code, start_time, event_time and end_code.
     */
    public static ResultSet userJobs(Connection conn, long userId, int maxNum)
	throws SQLException, SpkdbException 
    {
	String sql = "select job_id, abstract, state_code, start_time, event_time, end_code "
                     + "from job where user_id=" + userId 
                     + " order by job_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
        stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Record the end status of a job. 
       @param conn open connection to the database
       @param jobId key to the given job in the job table
       @param endCode the type of end that this job reached
       @param report final report 
       @return true
     */
    public static boolean endJob(Connection conn, long jobId, String endCode, String report)
	throws SQLException, SpkdbException
    {
	String sql = "select * from end where end_code='" + endCode + "';";
	PreparedStatement pstmt = conn.prepareStatement(sql);
	pstmt.execute();
	ResultSet rs = pstmt.getResultSet();
	if (!rs.next()) {
	    throw new SpkdbException("endCode = " + endCode + " is invalid");
	}
	sql = "update job set state_code='end', end_code='" + endCode + "', report=?"
	    + " where job_id =" + jobId + ";";
	pstmt = conn.prepareStatement(sql);
	pstmt.setBinaryStream(1, new ByteArrayInputStream(report.getBytes()), report.length());
	pstmt.executeUpdate();
	
	return true;
    }
    /**
       Get the final report of a job.
       @param conn open connection to the database
       @param jobId key to the given job in the job table
       @return final report
     */
    public static String jobReport(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
	String sql = "select state_code, report from job where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	rs.next();
	if (rs.getString("state_code").compareTo("end") != 0) {
	    throw new SpkdbException("no report because job is not in 'end' state");
	}
	Blob blobReport = rs.getBlob("report");
	long len = blobReport.length();
	byte[] byteReport = blobReport.getBytes(1L, (int)len);
	
	return new String(byteReport);
    }
    /**
       Add a new scientific dataset to the database.
       @param conn open connection to the database
       @param userId key to a user in the user table
       @param name name of this dataset (must be unique for the given user)
       @param abstraction short description of the dataset
       @param archive the entire data set in rcs-compatible format
       @return unique number, which is the key to the new row in the dataset table
     */
    public static long 
	newDataset(Connection conn, long userId, String name, String abstraction, String archive)
	throws SQLException, SpkdbException
    {
	long datasetId = 0;
	String sql
	    = "insert into dataset "
	    + "(user_id, name, abstract, archive) "
	    + "values (" + userId + ",'" + name + "','" + abstraction + "',?);";
	PreparedStatement pstmt = conn.prepareStatement(sql);
	pstmt.setBinaryStream(1, new ByteArrayInputStream(archive.getBytes()), archive.length());
	pstmt.executeUpdate();
	ResultSet rs = pstmt.getGeneratedKeys();
	if (rs.next()) {
	    datasetId = rs.getLong(1);
	}
	return datasetId;
    }       
    /**
       Get a dataset.
       @param conn open connection to the database
       @param datasetId key to a row in the dataset table
       @return dataset as an rcs-compatible archive 
     */
    public static String getDataset(Connection conn, long datasetId)
	throws SQLException, SpkdbException
    {
	String sql = "select archive from dataset where dataset_id=" + datasetId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	rs.next();
	Blob blobDataset = rs.getBlob("dataset");
	long len = blobDataset.length();
	byte[] byteDataset = blobDataset.getBytes(1L, (int)len);
	
	return new String(byteDataset);
    }       
    /**
       Update a row in the dataset table.
       @param conn open connection to the database
       @param datasetId key to a row in the database table
       @param name array of column names (cannot contain "dataset_id")
       @param value array of column values
       @return true or false
     */
    public static boolean updateDataset(Connection conn, long datasetId, String name[], String value[])
	throws SQLException, SpkdbException
    {
	 String nameList = name[0];
	 String valueList = "'" + value[0] + "'";
	 pattern1 = Pattern.compile("^dataset_id$");
	 if (pattern1.matcher(name[0]).find()) {
	     throw new SpkdbException("invalid attempt to change dataset_id");
	 }
	 String sql = "update dataset set " + name[0] + "='" + value[0] + "'";
	 for (int i = 1; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find()) {
		 throw new SpkdbException("invalid attempt to change dataset_id");
	     }
 	     sql += ", " + name[i] + "='" + value[i] + "'";
	 }
	 sql += " where dataset_id=" + datasetId + ";";
	 Statement stmt = conn.createStatement();
	 stmt.executeUpdate(sql);
	 return stmt.getUpdateCount() == 1;
    }
    /**
       Get datasets belonging to a given user
       @param conn open connection to the database
       @param userId key to the given user in the user table
       @param maxNum maximum number of datasets to return
       @return Object of a class which implements the java.ResultSet interface, containing
       a sequence of subsets of rows of the dataset table.  Each subset contains the 
       following columns: dataset_id, name, and abstract.

     */
    public static ResultSet userDatasets(Connection conn, long userId, int maxNum)
	throws SQLException, SpkdbException 
    {
	String sql = "select dataset_id, name, abstract "
                     + "from dataset where user_id=" + userId 
                     + " order by dataset_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
        stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Add a new scientific model to the database.
       @param conn open connection to the database
       @param userId key to a user in the user table
       @param name name of this model (must be unique for the given user)
       @param abstraction short description of the model
       @param archive the entire model in rcs-compatible format
       @return unique number, which is the key to the new row in the model table
     */
    public static long 
	newModel(Connection conn, long userId, String name, String abstraction, String archive)
	throws SQLException, SpkdbException
    {
	long modelId = 0;
	String sql
	    = "insert into model "
	    + "(user_id, name, abstract, archive) "
	    + "values (" + userId + ",'" + name + "','" + abstraction + "',?);";
	PreparedStatement pstmt = conn.prepareStatement(sql);
	pstmt.setBinaryStream(1, new ByteArrayInputStream(archive.getBytes()), archive.length());
	pstmt.executeUpdate();
	ResultSet rs = pstmt.getGeneratedKeys();
	if (rs.next()) {
	    modelId = rs.getLong(1);
	}
	return modelId;
    }       
    /**
       Get a model
       @param conn open connection to the database
       @param modelId key to a row in the model table
       @return model as an rcs-compatible archive 
     */
    public static String getModel(Connection conn, long modelId)
	throws SQLException, SpkdbException
    {
	String sql = "select archive from model where model_id=" + modelId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	rs.next();
	Blob blobModel = rs.getBlob("model");
	long len = blobModel.length();
	byte[] byteModel = blobModel.getBytes(1L, (int)len);
	
	return new String(byteModel);
    }       
    /**
       Update a row in the model table.
       @param conn open connection to the database
       @param modelId key to a row in the model table
       @param name array of column names (cannot contain "model_id")
       @param value array of column values
       @return true or false
     */
    public static boolean updateModel(Connection conn, long modelId, String name[], String value[])
	throws SQLException, SpkdbException
    {
	 String nameList = name[0];
	 String valueList = "'" + value[0] + "'";
	 pattern1 = Pattern.compile("^model_id$");
	 if (pattern1.matcher(name[0]).find()) {
	     throw new SpkdbException("invalid attempt to change model_id");
	 }
	 String sql = "update model set " + name[0] + "='" + value[0] + "'";
	 for (int i = 1; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find()) {
		 throw new SpkdbException("invalid attempt to change model_id");
	     }
 	     sql += ", " + name[i] + "='" + value[i] + "'";
	 }
	 sql += " where model_id=" + modelId + ";";
	 Statement stmt = conn.createStatement();
	 stmt.executeUpdate(sql);
	 return stmt.getUpdateCount() == 1;
    }
    /**
       Get models belonging to a given user
       @param conn open connection to the database
       @param userId key to the given user in the user table
       @param maxNum maximum number of models to return
       @return Object of a class which implements the java.ResultSet interface, containing
       a sequence of subsets of rows of the model table.  Each subset contains the 
       following columns: model_id, name, and abstract.

     */
    public static ResultSet userModels(Connection conn, long userId, int maxNum)
	throws SQLException, SpkdbException 
    {
	String sql = "select model_id, name, abstract "
                     + "from model where user_id=" + userId 
                     + " order by model_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
        stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Inserts a new user in the database, returning a unique key.
       @param conn open connection to the database
       @param name array of strings containing field names
       @param value array of values corresponding to field names in name
       @return long integer which is the unique key of the new row 
       @see #connect
     */
    public static long newUser(Connection conn, String name[], String value[])
	throws SQLException, SpkdbException
    {
	long userId = 0;
	boolean userFound = false;
	boolean passFound = false;
	pattern1 = Pattern.compile("^username$");
	pattern2 = Pattern.compile("^password$");
	String nameList = name[0];
	String valueList = "'" + value[0] + "'";
	for (int i = 1; i < name.length; i++) {
	    nameList += (", " + name[i]);
	    valueList += (", '" + value[i] + "'");
	}
	for (int i = 0; i < name.length; i++) {
	   userFound = userFound || pattern1.matcher(name[i]).find();
	   passFound = passFound || pattern2.matcher(name[i]).find();
	}
	if (!userFound || !passFound) {
	    throw new SpkdbException("username and/or password missing in name list");
	}
	String sql = "insert into user (" + nameList + ") values (" + valueList + ")";
	Statement stmt = conn.createStatement();
	stmt.executeUpdate(sql, Statement.RETURN_GENERATED_KEYS);
	ResultSet rs = stmt.getGeneratedKeys();
	if (rs.next()) {
	    userId = rs.getLong(1);
	}
	return userId;
    }
    /**
       Update a row in the user table.
       @param conn open connection to the database
       @param userId key to a row in the user table
       @param name array of column names (cannot include "user_id" or "username")
       @param value array of column values
       @return true or false
     */
    public static boolean updateUser(Connection conn, long userId, String name[], String value[])
	throws SQLException, SpkdbException
    {
	 String nameList = name[0];
	 String valueList = "'" + value[0] + "'";
	 pattern1 = Pattern.compile("^username$");
	 pattern2   = Pattern.compile("^user_id$");
	 if (pattern1.matcher(name[0]).find() || pattern2.matcher(name[0]).find()) {
	     throw new SpkdbException("invalid attempt to change username or user_id");
	 }
	 String sql = "update user set " + name[0] + "='" + value[0] + "'";
	 for (int i = 1; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find() || pattern2.matcher(name[0]).find()) {
		 throw new SpkdbException("invalid attempt to change username or user_id");
	     }
 	     sql += ", " + name[i] + "='" + value[i] + "'";
	 }
	 sql += " where user_id=" + userId + ";";
	 Statement stmt = conn.createStatement();
	 stmt.executeUpdate(sql);
	 return stmt.getUpdateCount() == 1;
    }
    /**
       Get a row from the user table
       @param conn open connection to the database
       @param username name which is an alternate key to the user table
       @return Object of a class which implements the java.sql.ResultSet interface, 
       containing a complete row of the user table.
     */
    public static ResultSet getUser(Connection conn, String username)
	throws SQLException, SpkdbException
    {
	String sql = "select * from user where username='" + username +"';";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();
	return rs;
    }
}
