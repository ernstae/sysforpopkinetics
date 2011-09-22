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
    /** Open a connection to a database. The object returned  must be passed as a parameter
     *      to other methods of the Spkdb class.  A process may have several connections open
     *      at the same time.  When a connection is no longer used, it should be closed
     *      using the Spkdb.disconnect() method.
     * @return an object of type java.sql.Connection
     * @see #disconnect
     * @see java.sql.Connection
     * @param dbName name of a database
     * @param hostName domain name of host on which database resides
     * @param dbUser username of a valid user of the database
     * @param dbPassword user's password
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
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
    /**        Close a database connection.
     * @return true
     * @see #connect
     * @param conn open connection to a database
     * @throws SQLException a SQL exception.
     */
    public static boolean disconnect(Connection conn) throws SQLException {
	conn.close();
	return true;
    }
    /**        Submit a job.
     * @return key to the new row in the job table
     * @param conn open connection to the database
     * @param userId key to a row in the user table
     * @param abstraction short description of the job
     * @param datasetId key to a row in the dataset table
     * @param datasetVersion rcs version code for the dataset
     * @param modelId key to a rew in the model table
     * @param modelVersion rcs version code for the model
     * @param xmlSource source code for the job
     * @param methodCode key to a row in the method table
     * @param parent the job_id of the job that is the parent; otherwise 0
     * @param isWarmStart true for being a warm start job; false for otherwise
     * @param isMailNotice true for requesting end-job mail notice, false for otherwise
     * @param isParallel true for running the job in parallel mode, false for otherwise
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
     * @throws FileNotFoundException a file not found exception
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
                              boolean isWarmStart,
                              boolean isMailNotice,
                              int nTasks)
	throws SQLException, SpkdbException, FileNotFoundException
    {
	long jobId = 0;
	java.util.Date date = new java.util.Date(); 
	long eventTime = date.getTime()/1000;
	long startTime = eventTime;
	String stateCode = "q2c";
        String sql;
        Blob checkpoint = null;
        int mail = isMailNotice ? 1 : 0;
        if(isWarmStart)
        {
            sql = "select checkpoint from job where job_id=" + parent + ";";
            Statement stmt = conn.createStatement();
	    stmt.execute(sql);
	    ResultSet rs = stmt.getResultSet();
            if (!rs.next())
	        throw new SpkdbException("This job cannot restart because the\ncheckpoint file does not exist.");
            checkpoint = rs.getBlob("checkpoint");
            stmt.close();
            if (checkpoint == null)
	        throw new SpkdbException("This job cannot restart because the\ncheckpoint file does not exist.");
	    sql = "insert into job (state_code, user_id, abstract, dataset_id, "
                                    + "dataset_version, model_id, model_version, "
                                    + "xml_source, method_code, parent, start_time, event_time, checkpoint, mail, parallel)"
                  + " values ('" + stateCode + "'," + userId + ", ?," + datasetId
                              + ",'" + datasetVersion + "'," + modelId + ",'" + modelVersion
                              + "', ?,'" + methodCode + "'," + parent + "," 
	                      + startTime + "," + eventTime + ", ?," + mail + "," + nTasks + ");";
        }
        else
        {
	    sql = "insert into job (state_code, user_id, abstract, dataset_id, "
                                    + "dataset_version, model_id, model_version, "
                                    + "xml_source, method_code, parent, start_time, event_time, mail, parallel)"
                  + " values ('" + stateCode + "'," + userId + ", ?," + datasetId
                              + ",'" + datasetVersion + "'," + modelId + ",'" + modelVersion
                              + "', ?,'" + methodCode + "'," + parent + "," 
	                      + startTime + "," + eventTime + "," + mail + "," + nTasks + ");";
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
        pstmt.close();
	addToHistory(conn, jobId, stateCode, "unknown");
	return jobId;
    }
    /**        Get the state transition history for a given job.
     * @return Object of type java.sql.Resultset, containing a sequence of rows of the
     *        hhistory table, related to a given job.
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @throws SQLException a SQL exception.
     */
    public static ResultSet jobHistory(Connection conn, long jobId)
	throws SQLException
    {
	String sql = "select * from history where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);

	return rs;
    }
    /**        Get a given job.
     * @return Object of type java.sql.ResultSet interface, containing a single row
     *        of the job table.
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getJob(Connection conn, long jobId)
	throws SQLException
    {
	String sql = "select * from job where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);

	return rs;
    }
    /** Set job abstract.
     * @return true if the job's abstract is set as the specified, otherwise false.
     * @param userId job owner's user ID
     * @param abstraction job abstraction to set.
     * @param conn open connection to the database
     * @param jobId id number of the job.
     * @throws SQLException a SQL exception.
     */
    public static boolean setJobAbstract(Connection conn, long userId, long jobId, String abstraction)
        throws SQLException
    {
        String sql = "update job set abstract=? where job_id=" + jobId 
                     + " and user_id=" + userId;
        PreparedStatement pstmt = conn.prepareStatement(sql);
	pstmt.setString(1, abstraction);
        boolean ok = pstmt.executeUpdate() == 1;
        pstmt.close();
        return ok;
    }
    /** Set job share_with.
     * @return true if the job's share_with is set as the specified, otherwise false.
     * @param userId job owner's user ID.
     * @param shareWith  user_id of the user to share the job.
     * @param conn open connection to the database
     * @param jobId id number of the job.
     * @throws SQLException a SQL exception.
     */
    public static boolean setJobShareWith(Connection conn, long userId, long jobId, long shareWith)
        throws SQLException
    {
        String sql = "update job set share_with=" + shareWith + " where job_id=" + jobId 
                     + " and user_id=" + userId;
        Statement stmt = conn.createStatement();
        boolean ok = false;
        if(stmt.executeUpdate(sql) == 1)
            ok = true;
        else if(stmt.executeUpdate(sql) == 0)
        {
            sql = "update job set share_with=0 where job_id=" + jobId;
            ok = stmt.executeUpdate(sql) == 1;
        }
        stmt.close();
        return ok;
    }
    /**        Get a sequence of jobs for a given user.
     * @return Object of type java.sql.Resultset, containing a sequence of rows of the
     *        job table, belonging to a given user. Three fields, xml_source, cpp_source, and
     *        report, which are defined in the SQL schema to have type "longblob" are returned
     *        as java.sql.Blob types.
     * @param conn open connection to the database
     * @param userId key to the given user in user table
     * @param maxNum maximum number of jobs to provide status for
     * @param leftOff least jobId previously returned (0 if first call in sequence)
     * @param startID starting jobID.
     * @param startTime starting submission time.
     * @param keyWords key words either in job abstract, in model name on in dataset name.
     * @param modelID finding jobs that use this model.
     * @param datasetID finding jobs that use thos dataset.
     * @throws SQLException a SQL exception.
     */
    public static ResultSet userJobs(Connection conn, long userId, int maxNum, long leftOff,
                                     String startID, String startTime, String keyWords,
                                     String modelID, String datasetID)
	throws SQLException
    {
	String
	    sql = "select job_id, share_with, abstract, state_code, start_time, event_time, "
	    + "end_code, model_id, model_version, dataset_id, dataset_version "
	    + "from job where (user_id=" + userId + " or share_with=" + userId + ")";
	if (leftOff != 0) {
	    sql += " and job_id < " + leftOff;
	}
        if(startID != null)
            sql += " and job_id <= " + startID;
        if(startTime != null)
            sql += " and start_time <= " + startTime;
        if(modelID != null)
            sql += " and model_id=" + modelID;
        if(datasetID != null)
            sql += " and dataset_id=" + datasetID;
        if(keyWords != null)
        {
            String[] words = keyWords.split(" ");
            sql = "select j.job_id, j.share_with, j.abstract, j.state_code, j.start_time, j.event_time, "
	          + "j.end_code, j.model_id, j.model_version, j.dataset_id, j.dataset_version "
	          + "from job j, model m, dataset d where (j.user_id=" + userId + " or j.share_with=" + userId + ")" 
                  + " and j.model_id=m.model_id and j.dataset_id=d.dataset_id";
	    if (leftOff != 0)
	        sql += " and j.job_id < " + leftOff;
            if(startID != null)
                sql += " and j.job_id <= " + startID;
            if(startTime != null)
                sql += " and j.start_time <= " + startTime;
            if(modelID != null)
                sql += " and j.model_id=" + modelID;
            if(datasetID != null)
                sql += " and j.dataset_id=" + datasetID;
            sql += " and (";
            for(int i = 0; i < words.length; i++)
            {
                if(i != 0 ) sql += " or ";
                sql += "j.abstract like '%" + words[i] + "%'"
                       + " or m.name like '%" + words[i] + "%' or d.name like '%" + words[i] + "%'";
            }
            sql += ")";
        }
        sql += " order by job_id";
//	sql += " order by job_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);

	return rs;
    }
    /** Abort a job when the job is in one of the four possible states.
     * If the job's state code is 'q2c', set the state code to 'end' and end code to 'abrt'.
     * If the job's state code is 'cmp', set the state code to 'q2ac'.
     * If the job's state code is 'q2r', set the state code to 'end' and the end code to 'abrt'.
     * If the job's state code is 'run', set the state code to 'q2ar'.
     * Set the job's event time and state code to the job's history.
     * @return state code if it has been set to 'end', 'q2ac', or 'q2ar', otherwise null.
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @throws SpkdbException a Spkdb exception.
     * @throws SQLException a SQL exception.
     */
    public static String abortJob(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
        java.util.Date date = new java.util.Date(); 
	long eventTime = date.getTime()/1000;
        Statement stmt = conn.createStatement();
        String state = "end";
        String sql ="update job set state_code='" + state +"', end_code='abrt', event_time=" +
                    eventTime + " where job_id ='" + jobId + "' and state_code='q2c';";
        if(stmt.executeUpdate(sql) != 1)
        {
            state = "q2ac";
            sql ="update job set state_code='" + state +"', event_time=" + eventTime +
                 " where job_id ='" + jobId + "' and state_code='cmp';";
            if(stmt.executeUpdate(sql) != 1)
            {
                state = "end";
                sql ="update job set state_code='" + state +"', end_code='abrt', event_time=" + 
                     eventTime + " where job_id ='" + jobId + "' and state_code='q2r';";
                if(stmt.executeUpdate(sql) != 1)
                {
                    state = "q2ar";
                    sql ="update job set state_code='" + state +"', event_time=" + 
                         eventTime + " where job_id ='" + jobId + "' and state_code='run';";
                    if(stmt.executeUpdate(sql) != 1)
                        return null;
                }
            }
        }
        stmt.close();
        addToHistory(conn, jobId, state, "unknown");
	return state;
    }
    /** Record the end status of a job.
     * @return true if the job's state_code is set to "end" by this method, otherwise false.
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @param endCode the type of end that this job reached
     * @param report final report
     * @throws SpkdbException a Spkdb exception.
     * @throws SQLException a SQL exception.
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
	    + " where job_id =" + jobId + " and state_code!='end';";
	pstmt = conn.prepareStatement(sql);
	pstmt.setBinaryStream(1, new ByteArrayInputStream(report.getBytes()), report.length());
        boolean ok = pstmt.executeUpdate() == 1;
        pstmt.close();
	if(ok)
	    addToHistory(conn, jobId, "end", "unknown");
        else
            return false;
	return true;
    }    
    /**        Add a new scientific dataset to the database.
     * @return unique number, which is the key to the new row in the dataset table
     * @param conn open connection to the database
     * @param userId key to a user in the user table
     * @param name name of this dataset (must be unique for the given user)
     * @param abstraction short description of the dataset
     * @param archive the entire data set in rcs-compatible format
     * @throws SQLException a SQL exception.
     */
    public static long 
	newDataset(Connection conn, long userId, String name, String abstraction, String archive)
	throws SQLException
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
        pstmt.close();
	return datasetId;
    }       
    /**        Get a dataset.
     * @return Object of type java.sql.ResultSet, containing a row of the dataset table.
     *        The archive field, defined in the SQL schema as having type "longblob", is
     *        returned as type java.sql.Blob.
     * @param conn open connection to the database
     * @param datasetId key to a row in the dataset table
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getDataset(Connection conn, long datasetId)
	throws SQLException
    {
	String sql = "select * from dataset where dataset_id=" + datasetId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }       
    /**        Update a row in the dataset table.
     * @return true or false
     * @param conn open connection to the database
     * @param datasetId key to a row in the database table
     * @param name array of column names (cannot contain "dataset_id")
     * @param value array of column values
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
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
/*
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
	 boolean ok = stmt.getUpdateCount() == 1;
         stmt.close();
*/         
         String sql = "update dataset set " + name[0] + "=?";
	 for (int i = 1; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find()) {
		 throw new SpkdbException("invalid attempt to change dataset_id");
	     }
 	     sql += ", " + name[i] + "=?";
	 }
	 sql += " where dataset_id=" + datasetId + ";";
         PreparedStatement pstmt = conn.prepareStatement(sql);
         for(int i = 0; i < name.length; i++)
            pstmt.setString(i + 1, value[i]);
         pstmt.executeUpdate();
         boolean ok = pstmt.getUpdateCount() == 1;
         pstmt.close();
         return ok;
    }
    /**        Get datasets belonging to a given user
     * @return Object of a class which implements the java.sql.ResultSet interface, containing
     *        a sequence of rows of the job table belonging to the given user. Each row contains
     *        all columns of the dataset table.  The archive field, which is defined in the SQL
     *        schema to have type "longblob", is returned as a java.sql.Blob type.
     * @param conn open connection to the database
     * @param userId key to the given user in the user table
     * @param maxNum maximum number of datasets to return
     * @param leftOff least datasetId previously returned (0 if first call in sequence)
     * @throws SQLException a SQL exception.
     */
    public static ResultSet userDatasets(Connection conn, long userId, int maxNum, long leftOff)
	throws SQLException
    {
	String
	    sql = "select * from dataset where user_id=" + userId;
	if (leftOff != 0) {
	    sql += " and dataset_id < " + leftOff;
	}
        sql += " order by dataset_id";
//	sql += " order by dataset_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);

	return rs;
    }
    /**        Add a new scientific model to the database.
     * @return unique number, which is the key to the new row in the model table
     * @param conn open connection to the database
     * @param userId key to a user in the user table
     * @param name name of this model (must be unique for the given user)
     * @param abstraction short description of the model
     * @param archive the entire model in rcs-compatible format
     * @throws SQLException a SQL exception.
     */
    public static long 
	newModel(Connection conn, long userId, String name, String abstraction, String archive)
	throws SQLException
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
        pstmt.close();
	return modelId;
    }       
    /**        Get a model
     * @return Object of type java.sql.ResultSet, containing a single row of the
     *        model table.  The archive field, defined in the SQL schema has having
     *        type "longblob" is returned with type java.sql.Blob.
     * @param conn open connection to the database
     * @param modelId key to a row in the model table
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getModel(Connection conn, long modelId)
	throws SQLException
    {
	String sql = "select * from model where model_id=" + modelId + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }       
    /**        Update a row in the model table.
     * @return true or false
     * @param conn open connection to the database
     * @param modelId key to a row in the model table
     * @param name array of column names (cannot contain "model_id")
     * @param value array of column values
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
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
/*         
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
	 boolean ok = stmt.getUpdateCount() == 1;
         stmt.close();
*/
         String sql = "update model set " + name[0] + "=?";
	 for (int i = 1; i < name.length; i++) {
	     if (pattern1.matcher(name[i]).find()) {
		 throw new SpkdbException("invalid attempt to change model_id");
	     }
 	     sql += ", " + name[i] + "=?";
	 }
	 sql += " where model_id=" + modelId + ";";
         PreparedStatement pstmt = conn.prepareStatement(sql);
         for(int i = 0; i < name.length; i++)
            pstmt.setString(i + 1, value[i]);
         pstmt.executeUpdate();
         boolean ok = pstmt.getUpdateCount() == 1;
         pstmt.close();
         return ok;
    }
    /**        Get a sequence of  models belonging to a given user
     * @return Object of a class which implements the java.sql.ResultSet interface, containing
     *        a sequence of rows of the job table belonging to the given user. Each row contains
     *        all columns of the model table.  The archive field, which is defined in the SQL
     *        schema to have type "longblob" is returned as a java.sql.Blob type.
     * @param conn open connection to the database
     * @param userId key to the given user in the user table
     * @param maxNum maximum number of models to return
     * @param leftOff least modelId previously returned (0 if first call in sequence)
     * @throws SQLException a SQL exception.
     */
    public static ResultSet userModels(Connection conn, long userId, int maxNum, long leftOff)
	throws SQLException
    {
	String 
	    sql = "select * from model where user_id=" + userId;
	if (leftOff != 0) {
	    sql += " and model_id < " + leftOff;
	}
        sql += " order by model_id";
//	sql += " order by model_id desc limit " + maxNum + ";";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);

	return rs;
    }
    /**        Inserts a new user in the database, returning a unique key.
     * @return long integer which is the unique key of the new row
     * @see #connect
     * @param conn open connection to the database
     * @param name array of strings containing field names
     * @param value array of values corresponding to field names in name
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
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
        stmt.close();
	return userId;
    }
    /**        Update a row in the user table.
     * @return true or false
     * @param conn open connection to the database
     * @param userId key to a row in the user table
     * @param name array of column names (cannot include "user_id" or "username")
     * @param value array of column values
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
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
	 boolean ok = stmt.getUpdateCount() == 1;
         stmt.close();
         return ok;
    }
    /**        Get a row from the user table by username
     * @return Object of a class which implements the java.sql.ResultSet interface,
     *        containing one complete row of the user table.
     * @param conn open connection to the database
     * @param username name which is an alternate key to the user table
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getUser(Connection conn, String username)
	throws SQLException
    {
	String sql = "select * from user where username='" + username +"';";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /** Get a row from the user table by user_id
     * @return Object of a class which implements the java.sql.ResultSet interface,
     *        containing one complete row of the user table.
     * @param userId user ID
     * @param conn open connection to the database
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getUserById(Connection conn, long userId)
	throws SQLException
    {
	String sql = "select * from user where user_id=" + userId;
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /**        Get the entire end table
     * @return Object of a class which implements the java.sql.ResultSet interface,
     *        containing a row for each row of the table.
     * @param conn open connection to the database
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getEndTable(Connection conn)
	throws SQLException
    {
	String sql = "select * from end;";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /**        Get the entire method table
     * @return Object of a class which implements the java.sql.ResultSet interface,
     *        containing a row for each row of the table.
     * @param conn open connection to the database
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getMethodTable(Connection conn)
	throws SQLException
    {
	String sql = "select * from method;";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /**        Get the entire state table
     * @return Object of a class which implements the java.sql.ResultSet interface,
     *        containing a row for each row of the table.
     * @param conn open connection to the database
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
     */
    public static ResultSet getStateTable(Connection conn)
	throws SQLException, SpkdbException
    {
	String sql = "select * from state;";
	Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /** Add the job state to the history table
     * @return true
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @param stateCode state code of the job
     * @param host domain name of host on which the job resides
     * @throws SQLException a SQL exception.
     * @throws SpkdbException a Spkdb exception.
     */
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
    /**
     * Digest a text using MD5 algorithm
     * @param password text to be digested
     * @return digested text
     */
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
    /** Set state code to a job
     * @return true if successfull, false otherwise
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @param stateCode state code to set
     * @throws SpkdbException a Spkdb exception.
     * @throws SQLException a SQL exception.
     */
    public static boolean setStateCode(Connection conn, long jobId, String stateCode)
        throws SQLException, SpkdbException
    {
        if(stateCode == null || stateCode.equals(""))
            throw new SpkdbException("State code is missing.");
        String sql = "update job set state_code='" + stateCode + "' where job_id=" +
                     jobId + ";";
        Statement stmt = conn.createStatement();
        boolean ok = stmt.executeUpdate(sql) != 1;
        stmt.close();
	if(ok)
            return false;
        return true;   
    }
    /** Set checkpoint to a job
     * @return true if successfull, false otherwise
     * @param conn open connection to the database
     * @param jobId key to the given job in the job table
     * @param checkpoint to set
     * @throws SpkdbException a Spkdb exception.
     * @throws SQLException a SQL exception.
     */
    public static boolean setCheckpoint(Connection conn, long jobId, String checkpoint)
        throws SQLException, SpkdbException
    {
        if(checkpoint == null || checkpoint.equals(""))
            throw new SpkdbException("Checkpoint is missing.");
        String sql = "update job set checkpoint='" + checkpoint + "' where job_id=" +
                     jobId + ";";
        Statement stmt = conn.createStatement();
        boolean ok = stmt.executeUpdate(sql) != 1;
        stmt.close();
	if(ok)
            return false;
        return true;   
    }
    /** Inserts a new group in the database, returning a unique key.
     * @return long integer which is the unique key of the new row
     * @see #connect
     * @param conn open connection to the database
     * @param name name of the group    
     * @throws SQLException a SQL exception.
     */
    public static long newGroup(Connection conn, String name)
	throws SQLException
    {
	long groupId = 0;
	String sql = "insert into team (team_name) values ('" + name + "')";
	Statement stmt = conn.createStatement();
	stmt.executeUpdate(sql, Statement.RETURN_GENERATED_KEYS);
	ResultSet rs = stmt.getGeneratedKeys();
	if (rs.next()) {
	    groupId = rs.getLong(1);
	}
        stmt.close();
	return groupId;
    }
    /** Add a user to a group
     * @see #connect
     * @param conn open connection to the database
     * @param username username to be added to the group 
     * @param groupId group ID of the group
     * @return true if successfull, false otherwise
     * @throws SQLException a SQL exception.
     */
    public static boolean newGroupMember(Connection conn, String username, long groupId)
        throws SQLException
    {
        String sql = "update user set team_id=" + groupId + " where username='" + 
                     username + "'";
        Statement stmt = conn.createStatement();
        boolean ok = stmt.executeUpdate(sql) != 1;
        stmt.close();
	if(ok)
            return false;
        return true;   
    }
    /** Get group user names
     * @param conn open connection to the database
     * @param group_id group ID
     * @return usernames of the group
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getGroupUsers(Connection conn, long group_id)
        throws SQLException
    {
        String sql = "select username from user where team_id=" + group_id + 
                     " order by username";
        Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
    /** Get email addresses of users
     * @param conn open connection to the database
     * @param type user type: developer, tester or all
     * @return email address list
     * @throws SQLException a SQL exception.
     */
    public static ResultSet getEmailAddress(Connection conn, String type)
        throws SQLException, SpkdbException
    {
        String sql = null;
        if(type.equals("developer")) sql = "select email from user where dev=1";
        if(type.equals("tester")) sql = "select email from user where test=1";
        if(type.equals("all")) sql = "select email from user where contact=1";
        if(sql == null) throw new SpkdbException("The user type was not correctly specified.");
        Statement stmt = conn.createStatement();
	ResultSet rs = stmt.executeQuery(sql);
	return rs;
    }
}
