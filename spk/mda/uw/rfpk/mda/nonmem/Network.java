package uw.rfpk.mda.nonmem;

import java.net.*;
import java.security.*;
import javax.crypto.*;
import java.nio.*;
import java.io.*;

/**
 * This class handles the secured network communications.
 * @author Jiaji Du
 * @version 1.0
 */
public class Network
{
    /**
     * This is the constructor with three arguments creating an url connection
     * to the server.
     * @param url 
     * @param sessionId
     * @param secretKey
     */
    public Network(String url, String sessionId, SecretKey secretKey) throws Exception
    {
        // Create an url connection
        URL urls = new URL(url + ";jsessionid=" + sessionId);
        con = urls.openConnection();            
        con.setUseCaches(false);
        con.setDoOutput(true);
        con.setDoInput(true);

        // Set the key
        key = secretKey;
    }

    /**
     * This private method encrypts and sends messages to the server.
     * Then, it gets and decrypts messages from the server.
     * @param messagesIn to send to the server in a String array
     * @param nMessages the number of messages to get
     * @return messagesOut to get in a String array of size nMesages
     * @exception IOException
     * @exception GeneralSecurityException
     */
    public String[] talk(String[] messagesIn, int nMessages)
	throws IOException, GeneralSecurityException
    {
        // Prepare for the return
        String[] messagesOut = new String[nMessages];      
       
        // Put the outgoing messages in a byte array
        byte[][] bytes;
        bytes = new byte[messagesIn.length][];
        int totalLength = 0;

        for(int i = 0; i < messagesIn.length; i++)
        {
            bytes[i] = messagesIn[i].getBytes();
            totalLength += bytes[i].length;
        }

        ByteBuffer byteBufferOut = ByteBuffer.allocate(4 * messagesIn.length + totalLength);

        for(int i = 0; i < messagesIn.length; i++)
        {
            byteBufferOut.putInt(bytes[i].length);
            byteBufferOut.put(bytes[i]);
        }

        byte[] original = byteBufferOut.array();

        // Encrypt the outgoing mesage
        Cipher cipherOut = Cipher.getInstance("Blowfish", "SunJCE");
        cipherOut.init(Cipher.ENCRYPT_MODE, key);
        byte[] encryptedOut = crypt(original, cipherOut);
        
        // Send the message out
        con.setRequestProperty("Content-type", "application/octet-stream");   
        con.setRequestProperty("Content-length", "" + encryptedOut.length);
        OutputStream dataOut = new DataOutputStream(con.getOutputStream()); 
        dataOut.write(encryptedOut);
        dataOut.flush();                                                        
        dataOut.close();

        // Get the incoming messages
        InputStream in = new DataInputStream(con.getInputStream());
        byte[] encryptedIn = new byte[con.getContentLength()];
        in.read(encryptedIn);
        in.close();

        // Decrypt the incoming messages
        Cipher cipherIn = Cipher.getInstance("Blowfish", "SunJCE");
        cipherIn.init(Cipher.DECRYPT_MODE, key); 
        byte[] decrypted = crypt(encryptedIn, cipherIn);

        // Recover the incoming messages from the byte array
        ByteBuffer byteBufferIn = ByteBuffer.allocate(decrypted.length);
        byteBufferIn.put(decrypted);
        byteBufferIn.rewind();

        for(int j = 0; j < nMessages; j++)
        {
            byte[] message = new byte[byteBufferIn.getInt()];
            byteBufferIn.get(message);
            messagesOut[j] = new String(message);
        }     
	   
        return messagesOut;        
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

    // URL connection
    private URLConnection con = null;

    // Secret key
    private SecretKey key = null;
}
