package uw.rfpk.mda.nonmem;

import java.net.*;
import java.nio.*;
import java.io.*;

/**
 * This class handles the network communications.
 * @author Jiaji Du
 * @version 1.0
 */
public class Network
{
    /**
     * This is the constructor with two arguments creating an url connection
     * to the server.
     * @param url 
     * @param sessionId
     * @exception Exception
     */
    public Network(String url, String sessionId) throws Exception
    {
        // Create an url connection
        URL urls = new URL(url + ";jsessionid=" + sessionId);
        con = urls.openConnection();            
        con.setUseCaches(false);
        con.setDoOutput(true);
        con.setDoInput(true);
    }

    /**
     * This method sends messages to the server.
     * Then, it gets messages from the server.
     * @param messagesOut A String array containing messages to be sent out
     * @param nMessages An int the number of messages to get from the server
     * @return messagesIn A String array containing messages to be received 
     * @exception IOException
     */
    public String[] talk(String[] messagesOut, int nMessages)
	throws IOException
    {
        // Prepare for the return
        String[] messagesIn = new String[nMessages];      
       
        // Put the outgoing messages in a byte array
        byte[][] bytes;
        bytes = new byte[messagesOut.length][];
        int totalLength = 0;

        for(int i = 0; i < messagesOut.length; i++)
        {
            bytes[i] = messagesOut[i].getBytes();
            totalLength += bytes[i].length;
        }

        ByteBuffer byteBufferOut = ByteBuffer.allocate(4 * messagesOut.length + totalLength);

        for(int i = 0; i < messagesOut.length; i++)
        {
            byteBufferOut.putInt(bytes[i].length);
            byteBufferOut.put(bytes[i]);
        }

        byte[] original = byteBufferOut.array();

        // Send the message out
        con.setRequestProperty("Content-type", "application/octet-stream");   
        con.setRequestProperty("Content-length", "" + original.length);
        OutputStream dataOut = new DataOutputStream(con.getOutputStream()); 
        dataOut.write(original);
        dataOut.flush();                                                        
        dataOut.close();

        // Get the incoming messages
        InputStream in = new DataInputStream(con.getInputStream());
        byte[] encryptedIn = new byte[con.getContentLength()];
        in.read(encryptedIn);
        in.close();

        // Recover the incoming messages from the byte array
        ByteBuffer byteBufferIn = ByteBuffer.allocate(encryptedIn.length);
        byteBufferIn.put(encryptedIn);
        byteBufferIn.rewind();

        for(int j = 0; j < nMessages; j++)
        {
            byte[] message = new byte[byteBufferIn.getInt()];
            byteBufferIn.get(message);
            messagesIn[j] = new String(message);
        }     
	   
        return messagesIn;        
    }

    // URL connection
    private URLConnection con = null;
}
