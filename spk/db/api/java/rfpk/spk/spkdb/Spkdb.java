package rfpk.spk.spkdb;
import java.sql.*;
import java.util.regex.*;
import rfpk.spk.spkdb.*;
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
	Connection con = DriverManager.getConnection("jdbc:mysql://" +
						      hostName + "/" +
						      dbName + 
						      "?user=" + dbUser + 
						      "&password=" + dbPassword);
	return con;
    }
    /**
       Close a database connection.
       @param con open connection to a database
       @see #connect
     */
    public static boolean disconnect(Connection con) throws SQLException {
	con.close();
	return true;
    }
    /**
       Inserts a new user in the database, returning a unique key.
       @param con connection object obtained by a previous call on connect()
       @param name array of strings containing field names
       @param value array of values corresponding to field names in name
       @return long integer which is the unique key of the new row 
       @see #connect
     */
    public static long newUser(Connection con, String name[], String value[])
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
	Statement stmt = con.createStatement();
	stmt.executeUpdate(sql, Statement.RETURN_GENERATED_KEYS);
	ResultSet rs = stmt.getGeneratedKeys();
	if (rs.next()) {
	    userId = rs.getLong(1);
	}
	return userId;
    }
}
