package rfpk.spk.spkdb;
import java.sql.*;
import java.util.*;
import java.util.regex.*;
import rfpk.spk.spkdb.*;
import java.io.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
/**
defines a set of static methods which provide a java API 
for the Spk Database.  This class should never be instantiated.
 */

public abstract class Spkdb {
    private static Pattern pattern1;
    private static Pattern pattern2;
    private static Pattern pattern3;
    /**
     Open a connection to a database. The object returned  must be passed as a parameter
     to other methods of the Spkdb class.  A process may have several connections open
     at the same time.  When a connection is no longer used, it should be closed
     using the Spkdb.disconnect() method.
     @param dbName name of a database
     @param hostName domain name of host on which database resides
     @param dbUser username of a valid user of the database
     @param dbPassword user's password
     @return an object of type java.sql.Connection
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
       Submit a job
       @param conn open connection to the database
       @param userId key to a row in the user table
       @param abstraction short description of the job
       @param datasetId key to a row in the dataset table
       @param datasetVersion rcs version code for the dataset
       @param modelId key to a rew in the model table
       @param modelVersion rcs version code for the model
       @param xmlSource source code for the job
       @param methodCode key to a row in the method table
       @param parent the job_id of the job that is the parent; otherwise 0
       @param isWarmStart true for being a warm start job; false for otherwise
       @return key to the new row in the job table
     */
    public static long newJob(Connection conn, 
			      long userId,
			      String abstraction,
			      long datasetId,
			      String datasetVersion,
			      long modelId,
			      String modelVersion,
			      String xmlSource,
			      String methodCode,
			      long parent,
                              boolean isWarmStart)
	throws SQLException, SpkdbException, FileNotFoundException
    {
	long jobId = 0;
	java.util.Date date = new java.util.Date(); 
	long eventTime = date.getTime()/1000;
	long startTime = eventTime;
	String stateCode = "q2c";
        String sql;
        Blob checkpoint = null;
        if(isWarmStart)
        {
            sql = "select checkpoint from job where job_id=" + parent + ";";
            Statement stmt = conn.createStatement();
	    stmt.execute(sql);
	    ResultSet rs = stmt.getResultSet();
            if (!rs.next())
	        throw new SpkdbException("Checkpoint file was not found.");
            checkpoint = rs.getBlob("checkpoint");
            if (checkpoint == null)
	        throw new SpkdbException("Checkpoint file was not found.");
	    sql = "insert into job (state_code, user_id, abstract, dataset_id, "
                                    + "dataset_version, model_id, model_version, "
                                    + "xml_source, method_code, parent, start_time, event_time, checkpoint)"
                  + " values ('" + stateCode + "'," + userId + ", ?," + datasetId
                              + ",'" + datasetVersion + "'," + modelId + ",'" + modelVersion
                              + "', ?,'" + methodCode + "'," + parent + "," 
	                      + startTime + "," + eventTime + ", ?);";
        }
        else
        {
	    sql = "insert into job (state_code, user_id, abstract, dataset_id, "
                                    + "dataset_version, model_id, model_version, "
                                    + "xml_source, method_code, parent, start_time, event_time)"
                  + " values ('" + stateCode + "'," + userId + ", ?," + datasetId
                              + ",'" + datasetVersion + "'," + modelId + ",'" + modelVersion
                              + "', ?,'" + methodCode + "'," + parent + "," 
	                      + startTime + "," + eventTime + ");";
        }
	PreparedStatement pstmt = conn.prepareStatement(sql);
        pstmt.setString(1, abstraction);
	pstmt.setBinaryStream(2, new ByteArrayInputStream(xmlSource.getBytes()), xmlSource.length());
        if(isWarmStart && checkpoint != null) 
            pstmt.setBlob(3, checkpoint);
        pstmt.executeUpdate();
	ResultSet rs = pstmt.getGeneratedKeys();
	if (rs.next()) {
	    jobId = rs.getLong(1);
	}
	addToHistory(conn, jobId, stateCode, "unknown");
	return jobId;
    }
    /**
       Get the state transition history for a given job.
       @param conn open connection to the database
       @param jobId key to the given job in the job table
       @return Object of type java.sql.Resultset, containing a sequence of rows of the
       hhistory table, related to a given job.
     */
    public static ResultSet jobHistory(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
	String sql = "select * from history where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Get a given job.
       @param conn open connection to the database
       @param jobId key to the given job in the job table
       @return Object of type java.sql.ResultSet interface, containing a single row 
       of the job table.
     */
    public static ResultSet getJob(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
	String sql = "select * from job where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
    /**
       Get a sequence of jobs for a given user.
       @param conn open connection to the database 
       @param userId key to the given user in user table
       @param maxNum maximum number of jobs to provide status for
       @param leftOff least jobId previously returned (0 if first call in sequence)
       @return Object of type java.sql.Resultset, containing a sequence of rows of the
       job table, belonging to a given user. Three fields, xml_source, cpp_source, and
       report, which are defined in the SQL schema to have type "longblob" are returned
       as java.sql.Blob types.
     */
    public static ResultSet userJobs(Connection conn, long userId, int maxNum, long leftOff)
	throws SQLException, SpkdbException 
    {
	String
	    sql = "select job_id, abstract, state_code, start_time, event_time, "
	    + "end_code, model_id, model_version, dataset_id, dataset_version "
	    + "from job where user_id=" + userId;
	if (leftOff != 0) {
	    sql += " and job_id < " + leftOff;
	}
	sql += " order by job_id desc limit " + maxNum + ";";
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
	addToHistory(conn, jobId, "end", "unknown");
	
	return true;
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
	    + "values (" + userId + ", ?, ?, ?);";
	PreparedStatement pstmt = conn.prepareStatement(sql);
        pstmt.setString(1, name);
        pstmt.setString(2, abstraction);
	pstmt.setBinaryStream(3, new ByteArrayInputStream(archive.getBytes()), archive.length());
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
       @return Object of type java.sql.ResultSet, containing a row of the dataset table.
       The archive field, defined in the SQL schema as having type "longblob", is
       returned as type java.sql.Blob.
     */
    public static ResultSet getDataset(Connection conn, long datasetId)
	throws SQLException, SpkdbException
    {
	String sql = "select * from dataset where dataset_id=" + datasetId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
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
       @param leftOff least datasetId previously returned (0 if first call in sequence)
       @return Object of a class which implements the java.sql.ResultSet interface, containing
       a sequence of rows of the job table belonging to the given user. Each row contains 
       all columns of the dataset table.  The archive field, which is defined in the SQL
       schema to have type "longblob", is returned as a java.sql.Blob type.
     */
    public static ResultSet userDatasets(Connection conn, long userId, int maxNum, long leftOff)
	throws SQLException, SpkdbException 
    {
	String
	    sql = "select * from dataset where user_id=" + userId;
	if (leftOff != 0) {
	    sql += " and dataset_id < " + leftOff;
	}
	sql += " order by dataset_id desc limit " + maxNum + ";";
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
	    + "values (" + userId + ", ?, ?, ?);";
	PreparedStatement pstmt = conn.prepareStatement(sql);
        pstmt.setString(1, name);
        pstmt.setString(2, abstraction);
	pstmt.setBinaryStream(3, new ByteArrayInputStream(archive.getBytes()), archive.length());
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
       @return Object of type java.sql.ResultSet, containing a single row of the
       model table.  The archive field, defined in the SQL schema has having
       type "longblob" is returned with type java.sql.Blob.
     */
    public static ResultSet getModel(Connection conn, long modelId)
	throws SQLException, SpkdbException
    {
	String sql = "select * from model where model_id=" + modelId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
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
       Get a sequence of  models belonging to a given user
       @param conn open connection to the database
       @param userId key to the given user in the user table
       @param maxNum maximum number of models to return
       @param leftOff least modelId previously returned (0 if first call in sequence)
       @return Object of a class which implements the java.sql.ResultSet interface, containing
       a sequence of rows of the job table belonging to the given user. Each row contains 
       all columns of the model table.  The archive field, which is defined in the SQL
       schema to have type "longblob" is returned as a java.sql.Blob type.
     */
    public static ResultSet userModels(Connection conn, long userId, int maxNum, long leftOff)
	throws SQLException, SpkdbException 
    {
	String 
	    sql = "select * from model where user_id=" + userId;
	if (leftOff != 0) {
	    sql += " and model_id < " + leftOff;
	}
	sql += " order by model_id desc limit " + maxNum + ";";
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
	    if (pattern2.matcher(name[i]).find()) {
		passFound = true;
		valueList += (", MD5('" + value[i] + "')");
	    }
	    else 
		valueList += (", '" + value[i] + "'");
	}
	for (int i = 0; i < name.length; i++) {
	   userFound = userFound || pattern1.matcher(name[i]).find();
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
	 pattern2 = Pattern.compile("^user_id$");
	 pattern3 = Pattern.compile("^password$");

	 String sql = "update user set ";
	 for (int i = 0; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find() || pattern2.matcher(name[0]).find()) {
		 throw new SpkdbException("invalid attempt to change username or user_id");
	     }
	     if (i != 0)
		 sql += ", ";
	     if (pattern3.matcher(name[i]).find()) 
		 sql += name[i] + "=MD5('" + value[i] + "')";
	     else
		 sql += name[i] + "='" + value[i] + "'";
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
       containing one complete row of the user table.
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
    /**
       Get the entire end table
       @param conn open connection to the database
       @return Object of a class which implements the java.sql.ResultSet interface,
       containing a row for each row of the table.
     */
    public static ResultSet getEndTable(Connection conn)
	throws SQLException, SpkdbException
    {
	String sql = "select * from end;";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();
	return rs;
    }
    /**
       Get the entire method table
       @param conn open connection to the database
       @return Object of a class which implements the java.sql.ResultSet interface,
       containing a row for each row of the table.
     */
    public static ResultSet getMethodTable(Connection conn)
	throws SQLException, SpkdbException
    {
	String sql = "select * from method;";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();
	return rs;
    }
    /**
       Get the entire state table
       @param conn open connection to the database
       @return Object of a class which implements the java.sql.ResultSet interface,
       containing a row for each row of the table.
     */
    public static ResultSet getStateTable(Connection conn)
	throws SQLException, SpkdbException
    {
	String sql = "select * from state;";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();
	return rs;
    }
    public static boolean addToHistory(Connection conn, long jobId, String stateCode, String host)
	throws SQLException, SpkdbException
    {
	java.util.Date date = new java.util.Date(); 
	long eventTime = date.getTime()/1000;
	String sql = "insert into history (job_id, state_code, event_time, host) "
	         + "values(" + jobId + ", '" + stateCode + "'," + eventTime
	         + ", '" + host + "');";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	return true;
    }
    public static String md5sum(String password) {
	String pd = "";
	try {
	    MessageDigest algorithm = MessageDigest.getInstance("MD5");
	    algorithm.reset();
	    algorithm.update(password.getBytes());
	    byte[] hash = algorithm.digest();

	    for (int i = 0; i < hash.length; i++) {
		int v = hash[i] & 0xFF;
		if (v < 16)
		    pd += "0";
		pd += Integer.toString(v, 16).toLowerCase();
	    }
	}
	catch(NoSuchAlgorithmException e) {
	}
	return pd;
    }
}
