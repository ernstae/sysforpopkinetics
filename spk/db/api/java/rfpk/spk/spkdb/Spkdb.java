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
    private static Pattern userPattern;
    private static Pattern passPattern;
    /**
     Returns a connection to a database.  This object must be passed as a parameter
     to other methods of this class.  A process may have several connections open
     at the same time.  When a connection is no longer used, it should be closed
     using the Spkdb.disconnect() method.
     @param dbName name of a database
     @param hostName domain name of host on which database resides
     @param dbUser username of a valid user of the database
     @param dbPassword user's password
     @return an object of a class that implements java.sql.Connection
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
       @see #connect
     */
    public static boolean disconnect(Connection conn) throws SQLException {
	conn.close();
	return true;
    }
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
    public static ResultSet jobStatus(Connection conn, long jobId)
	throws SQLException, SpkdbException
    {
	String sql = "select state_code, event_time, end_code from job where job_id=" + jobId + ";";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();

	return rs;
    }
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
       Inserts a new user in the database, returning a unique key.
       @param conn connection object obtained by a previous call on connect()
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
	userPattern = Pattern.compile("^username$");
	passPattern = Pattern.compile("^password$");
	String nameList = name[0];
	String valueList = "'" + value[0] + "'";
	for (int i = 1; i < name.length; i++) {
	    nameList += (", " + name[i]);
	    valueList += (", '" + value[i] + "'");
	}
	for (int i = 0; i < name.length; i++) {
	   userFound = userFound || userPattern.matcher(name[i]).find();
	   passFound = passFound || passPattern.matcher(name[i]).find();
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
    public static boolean updateUser(Connection conn, long userId, String name[], String value[])
	throws SQLException, SpkdbException
    {
	 String nameList = name[0];
	 String valueList = "'" + value[0] + "'";
	 userPattern = Pattern.compile("^username$");
	 if (userPattern.matcher(name[0]).find()) {
	     throw new SpkdbException("invalid attempt to change username");
	 }
	 String sql = "update user set " + name[0] + "='" + value[0] + "'";
	 for (int i = 1; i < name.length; i++) {
	     if (userPattern.matcher(name[i]).find()) {
		 throw new SpkdbException("invalid attempt to change username");
	     }
 	     sql += ", " + name[i] + "='" + value[i] + "'";
	 }
	 sql += " where user_id=" + String.valueOf(userId) + ";";
	 Statement stmt = conn.createStatement();
	 stmt.executeUpdate(sql);
	 return stmt.getUpdateCount() == 1;
    }
    public static ResultSet getUser(Connection conn, long userId)
	throws SQLException, SpkdbException
    {
	String sql = "select * from user where user_id=" + String.valueOf(userId) + ";";
	Statement stmt = conn.createStatement();
	stmt.execute(sql);
	ResultSet rs = stmt.getResultSet();
	return rs;
    }
}
