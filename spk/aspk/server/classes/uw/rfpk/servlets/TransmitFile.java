package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.security.*;
import javax.crypto.*;
import java.nio.*;
import javax.swing.JOptionPane;

/**
 * This servlet receives a stream and put it in a byte array.  The byte array
 * is decrypted and the results is contained in another byte array.  The first
 * four-bytes of the resulting byte array is an int value that is the length 
 * of the first message.  The following bytes of the length is the content of
 * the first message.  The next four-bytes is another int value that is the 
 * length of the second message.  The messages are converted into String objects.
 * Currently, there is only one messages contained in the incoming stream.
 * The servlet uses the String object as the filename to open the requested
 * file in the directory /home/jiaji/User/USER_NAME.  The servlet then creats
 * two String objects.  The first String object is the coontent of the file and 
 * the last String object is the processing result message to the client.  If the
 * requested file does not exist, the first object will be only a character '/r'.
 * The servlet encodes these outgoing messages in the same way as the incoming 
 * messages before sending them out as a stream.
 * @author Jiaji Du
 * @version 1.0
 */
public class TransmitFile extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client
     * @exception ServletException
     * @exception IOException
     */
    public void service(HttpServletRequest req, HttpServletResponse resp)
	throws ServletException, IOException
    {
        try
	{
            // Get the user name of the session for saving the file
            String user = (String)req.getSession().getAttribute("USER_NAME");

            // Get the incoming messages
            InputStream in = new DataInputStream(req.getInputStream());       
            byte[] encryptedIn = new byte[req.getContentLength()];
            in.read(encryptedIn);

            // Get the secret key of the session for encryption and decryption
            SecretKey key = (SecretKey)req.getSession().getAttribute("KEY");

            // Decrypt the incoming messages
            Cipher cipherIn = Cipher.getInstance("Blowfish", "SunJCE");
            cipherIn.init(Cipher.DECRYPT_MODE, key); 
            byte[] decryptedIn = crypt(encryptedIn, cipherIn);

            // Recover the incoming message from the byte array
            ByteBuffer byteBuffer = ByteBuffer.allocate(decryptedIn.length);
            byteBuffer.put(decryptedIn);
            byteBuffer.rewind();
            byte[] messageIn = new byte[byteBuffer.getInt()];
            byteBuffer.get(messageIn);

            // Read the incoming message
            String fileName = new String(messageIn);

            // Open the file in the user's directory
            String pathName = "/home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/user/" + user + "/" + fileName;
            File oldFile = new File(pathName);  
            String message1, message2;
            if(oldFile.exists())
	    {
                BufferedReader fileIn = new BufferedReader(new FileReader(oldFile));
                String fileText = "";
                while(true)
	        {
                    int i = fileIn.read();
                    if(i != -1)
                        fileText += (char)i;
                    else
                        break;
                }
                fileIn.close();

                // Write the outgoing messages
                message1 = fileText;
                message2 = "The file " + fileName + " is transmitted.";
            }
            else
	    {
                // Write the outgoing messages
                message1 = "";
                message2 = "The file " + fileName + " does not exist.";
            }

            // Put the outgoing messages in a byte array
            byte[] messageOut1 = message1.getBytes();
            byte[] messageOut2 = message2.getBytes();
            byteBuffer = ByteBuffer.allocate(8 + messageOut1.length + messageOut2.length);
            byteBuffer.putInt(messageOut1.length);
            byteBuffer.put(messageOut1);
            byteBuffer.putInt(messageOut2.length);
            byteBuffer.put(messageOut2);
            byte[] decryptedOut = byteBuffer.array();

            // Encrypt the outgoing mesage
            Cipher cipherOut = Cipher.getInstance("Blowfish", "SunJCE");
            cipherOut.init(Cipher.ENCRYPT_MODE, key);
            byte[] encryptedOut = crypt(decryptedOut, cipherOut);

            // Send the message out
            resp.setContentType("application/octet-stream");
            resp.setContentLength(encryptedOut.length);
            ServletOutputStream servletOut = resp.getOutputStream();
            servletOut.write(encryptedOut);
            servletOut.close();
        }
        catch(GeneralSecurityException e)
        {
            JOptionPane.showMessageDialog(null, 
                                          "GeneralSecurityException = " + e,   
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);         
        }
    }

    /**
     * Uses a cipher to transform the bytes in a byte array
     * and returns the transformed bytes to another byte array.
     * @param in the input bytes in an array
     * @param cipher the cipher that transforms the bytes
     * @return out the output bytes in an array
     * @exception IOException
     * @exception GeneralSecurityException
     */
    public static byte[] crypt(byte[] in, Cipher cipher) 
                         throws IOException, GeneralSecurityException
    {
        int blockSize = cipher.getBlockSize();
        int outputSize = cipher.getOutputSize(blockSize);
        int nBlock = in.length / blockSize;
        int nFinal = in.length % blockSize;
        int nTotal = 0;

        byte[] outTotal = new byte[nBlock * outputSize + outputSize];
        byte[] inBytes = new byte[blockSize];
        byte[] outBytes = new byte[outputSize];

        for(int i = 0; i < nBlock; i++)
        {
            System.arraycopy(in, i * blockSize, inBytes, 0, blockSize);
            int outLength = cipher.update(inBytes, 0, blockSize, outBytes);
            System.arraycopy(outBytes, 0, outTotal, nTotal, outLength);
            nTotal += outLength;
        }
       
        if (nFinal > 0)
        {
            System.arraycopy(in, nBlock * blockSize, inBytes, 0, nFinal);
            outBytes = cipher.doFinal(inBytes, 0, nFinal);
        }
        else
        {
            outBytes = cipher.doFinal();
        }

        System.arraycopy(outBytes, 0, outTotal, nTotal, outBytes.length);
        nTotal += outBytes.length;
      
        byte[] out = new byte[nTotal];
        System.arraycopy(outTotal, 0, out, 0, nTotal);

        return out;
    }
}
