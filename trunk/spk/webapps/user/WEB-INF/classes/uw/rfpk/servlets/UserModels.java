package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import java.util.Vector;
import java.text.SimpleDateFormat;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo; 

/** This servlet receives a String array containing four String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the maximum number of models to provide status for.  The third String object is
 * the least model_id previously returned.  The fourth String object indicates if it is to 
 * get the model list of the model library.  The servlet calls database API method, userModels,
 * to get model status that includes id, name, newest version number, and abstract
 * of the models.  The servlet puts these data into a String[][] object and saves the user's
 * model id list in the Session object, MODELIDS.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class UserModels extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client
     * @exception ServletException a general exception a servlet can throw when it encounters difficulty
     * @exception IOException an I/O exception of some sort
     */
    public void service(HttpServletRequest req, HttpServletResponse resp)
	throws ServletException, IOException
    {
        // Get the user name of the session
        UserInfo user = (UserInfo)req.getSession().getAttribute("validUser");
        String username = user.getUserName();
        
        // Prepare output message
        String messageOut = "";
        String[][] userModels = null;
        
        // Get the input stream for reading data from the client
        ObjectInputStream in = new ObjectInputStream(req.getInputStream());  
       
        // Set the content type we are sending
        resp.setContentType("application/octet-stream");
        
        // Data will always be written to a byte array buffer so
        // that we can tell the server the length of the data
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
      
        // Create the output stream to be used to write the data
        // to our buffer
        ObjectOutputStream out = new ObjectOutputStream(byteOut);
     
        // Prepare for the return
        Vector modelList = new Vector();

        try
        {
            // Read the data from the client 
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0];
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
 	        int maxNum = Integer.parseInt(messageIn[1]); 
                long leftOff = Long.parseLong(messageIn[2]);
                if(messageIn[3].equals("true"))
                    username = "library";
           
                // Connect to the database
                ServletContext context = getServletContext();
                Connection con = Spkdb.connect(context.getInitParameter("database_name"),
                                               context.getInitParameter("database_host"),
                                               context.getInitParameter("database_username"),
                                               context.getInitParameter("database_password"));                
 
                // Get user id
                ResultSet userRS = Spkdb.getUser(con, username);
                userRS.next();
                long userId = userRS.getLong("user_id");
 
                // Get user jobs
                ResultSet userModelsRS = Spkdb.userModels(con, userId, maxNum, leftOff);  
             
                // Fill in the List
                SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
                while(userModelsRS.next())
                {                  
                    // Get model id
                    long modelId = userModelsRS.getLong("model_id"); 
                    
                    // Get model archive
                    ResultSet modelRS = Spkdb.getModel(con, modelId);
                    modelRS.next();
                    String modelArchive = modelRS.getString("archive");
                    Archive archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes()));
                    
                    // Fill in the list 
                    String[] model = new String[4];
                    model[0] = String.valueOf(modelId); 
                    model[1] = userModelsRS.getString("name");
                    model[2] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                    model[3] = userModelsRS.getString("abstract");
                    modelList.add(model);
                }
                
                // Disconnect to the database
                Spkdb.disconnect(con);
                
                // Put the list in the String[][]
                int nModel = modelList.size(); 
                if(nModel > 0)
                {
                    userModels = new String[nModel][4]; 
                    Vector modelIds = (Vector)req.getSession().getAttribute("MODELIDS");
                    if(modelIds == null)
                        modelIds = new Vector();              
                    for(int i = 0; i < nModel; i++)
                    {
                        userModels[i] = (String[])modelList.get(i);
                        modelIds.add(userModels[i][0]); 
                    }
                    req.getSession().setAttribute("MODELIDS", modelIds);                    
                }
                else
                    messageOut = "No model was found in the database";
            }
            else
            {
                // Write the outgoing messages
                messageOut = "Authentication error.";              
            }           
        }      
        catch(SQLException e)
        {
            messageOut = e.getMessage();
        }    
        catch(SpkdbException e)
        {
            messageOut = e.getMessage();
        }
        catch(ClassNotFoundException e)
        {
            messageOut = e.getMessage();
        } 
        catch(ParseException e)
        { 
            messageOut = e.getMessage();
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(userModels);

        // Flush the contents of the output stream to the byte array
        out.flush();
        
        // Get the buffer that is holding our response
        byte[] buf = byteOut.toByteArray();
        
        // Notify the client how much data is being sent
        resp.setContentLength(buf.length);
        
        // Send the buffer to the client
        ServletOutputStream servletOut = resp.getOutputStream();
        
        // Wrap up
        servletOut.write(buf);
        servletOut.close();
    }
}

