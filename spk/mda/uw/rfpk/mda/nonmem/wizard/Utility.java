/*
 * Utility.java
 *
 * Created on September 9, 2003, 9:01 AM
 */

package uw.rfpk.mda.nonmem.wizard;

import javax.swing.JButton;
import javax.swing.DefaultListModel;
import javax.swing.JOptionPane;
import java.util.Vector;
import java.util.StringTokenizer;
import java.io.*;

/**
 * This class defines static functions used by steps
 * @author  jiaji
 */
public class Utility {
    
    /** Creates a new instance of Utility */
    public Utility() {
    }
    
    /** Eliminates comments in a text
     * @param text A String object containing the text to have comments eliminited
     * @return A String object containing the text without comments
     */    
    public static String eliminateComments(String text)
    {
        StringBuffer buffer = new StringBuffer();
        boolean b = true;
        for(int i = 0; i < text.length(); i++)
        {
            char ch = text.charAt(i);
            if(ch == ';') b = false;
            if(ch == '\n') b = true;
            if(b) buffer.append(ch);
        }
        return buffer.toString();
    } 
        
    /** Find substrings that has the patten of s(i), where s is a character string 
     * i is an integer number.  Than find the largest i among all the substrings.
     * @param record The String object containing the text to search in
     * @param s A String object containing the character string described above 
     * @return An int, the largest number found as described above
     */    
    public static int find(String record, String s)
    {
        String[] strs = ("a" + record).split(s); 
        int m = 0, n = 0;
        for(int i = 1; i < strs.length; i++)
        {   
            int startingIndex = strs[i].indexOf("(");
            int endingIndex = strs[i].indexOf(")");
            if(startingIndex != -1 && endingIndex != -1)
                m = Integer.parseInt(strs[i].substring(startingIndex + 1, endingIndex).trim());
            if(m > n) n = m;
        }      
        return n;
    }
    
    /** Prepare the default $PK code
     * @param advan An int, the ADVAN number
     * @param trans An int, the TRANS number
     * @return A String object, the default $PK code
     */    
    public static String defaultPK( int advan, String trans)
    {
        String pk = null;
        switch(advan)
        {
            case 1:
                if(trans.equals("TRANS1"))
                    pk = "K=";
                if(trans.equals("TRANS2"))
                    pk = "CL=";
                pk = pk + "\nV=\nS1=1\nS2=1\nF1=1\nF0=1\nALAG1=0";
                break;
            case 2: 
                if(trans.equals("TRANS1"))
                    pk = "K=";
                if(trans.equals("TRANS2"))
                    pk = "CL=";
                pk = pk + "\nV=\nKA=\nS1=1\nS2=1\nS3=1\nF1=1\nF2=1\nF0=1\nALAG1=0\nALAG2=0";
                break;
            case 3:
                if(trans.equals("TRANS1"))
                    pk = "K=\nK12=\nK21=";
                if(trans.equals("TRANS3"))
                    pk = "CL=\nV=\nQ=\nVSS=";
                if(trans.equals("TRANS4"))
                    pk = "CL=\nV1=\nQ=\nV2=";
                if(trans.equals("TRANS5"))
                    pk = "AOB=\nALPHA=\nBETA=";
                pk = pk + "\nS1=1\nS2=1\nS3=1\nF1=1\nF2=1\nF0=1\nALAG1=0\nALAG2=0";                    
                break;
            case 4: 
                if(trans.equals("TRANS1"))
                    pk = "KA=\nK=\nK23=\nK32=";
                if(trans.equals("TRANS3"))
                    pk = "CL=\nV=\nQ=\nVSS=\nKA=";
                if(trans.equals("TRANS4"))
                    pk = "CL=\nV2=\nQ=\nV3=\nKA";
                if(trans.equals("TRANS5"))
                    pk = "AOB=\nALPHA=\nBETA=\nKA=";
                pk = pk + "\nS1=1\nS2=1\nS3=1\nS4=1\nF1=1\nF2=1\nF3=1\nF0=1\nALAG1=0\nALAG2=0\nALAG3=0";  
                break;
            case 10:
                pk = "VM=\nKM=\nS1=1\nS2=1\nF1=1\nF0=1\nALAG1=0";
                break;
            case 11:
                if(trans.equals("TRANS1"))
                    pk = "K=\nK12=\nK21=\nK13=\nK31=";
                if(trans.equals("TRANS4"))
                    pk = "CL=\nV1=\nQ2=\nV2=\nQ3=\nV3=\nKA";
                if(trans.equals("TRANS6"))
                    pk = "ALPHA=\nBETA=\nGAMMA=\nK21=\nK31=";
                pk = pk + "\nS1=1\nS2=1\nS3=1\nS4=1\nF1=1\nF2=1\nF3=1\nF0=1\nALAG1=0\nALAG2=0\nALAG3=0";                      
                break;
            case 12:
                if(trans.equals("TRANS1"))
                    pk = "K=\nK23=\nK32=\nK24=\nK42=";
                if(trans.equals("TRANS4"))
                    pk = "CL=\nV1=\nQ2=\nV2=\nQ3=\nV3=\nKA";
                if(trans.equals("TRANS6"))
                    pk = "ALPHA=\nBETA=\nGAMMA=\nK32=\nK42=\nKA=";
                pk = pk + "\nS1=1\nS2=1\nS3=1\nS4=1\nF1=1\nF2=1\nF3=1\nF0=1\nALAG1=0\nALAG2=0\nALAG3=0";           
                break;
            default:
                pk = "";
        }
        return pk;
    }
    
    /** Set "Up" and "Down" buttons 
     * @param index An int, the selected index in the list
     * @param model A DefaultListModel object, the model for the list,
     * @param upButton A JButton object, the "Up" button
     * @param downButton A JButton object, the "Down" button
     */    
    public static void setUpDownButton(int index, DefaultListModel model, JButton upButton, JButton downButton)
    {
        upButton.setEnabled(index > 0);
        downButton.setEnabled(index < model.getSize() - 1);
    }
    
    /** Determine if a character sting represents a floating point number
     * @param s A String object containing the the character string 
     * @return A boolean, true for the string is a floating point number,
     * false for otherwise
     */    
    public static boolean isFloatNumber(String s)
    {
        // Check string length
        if(s.length() == 0) return false; 
        // Check sign
        if(s.startsWith("+") || s.startsWith("-"))
            s = s.substring(1); 
        // Check other characters
        int dot = 0;
        for(int i = 0; i < s.length(); i++)
        {
            char ch = s.charAt(i);
            if(!Character.isDigit(ch) && ch != '.')
                return false;
            if(ch == '.')
                dot++;
            if(dot == 2)
                return false;
        }
        // Check "0." 
        if(s.charAt(0) == '0' && s.length() > 1 && s.charAt(1) != '.')
            return false;
        return true;
    }
    
    /** Determine if a character sting represents an integer number
     * @param s A String object containing the the character string 
     * @return A boolean, true for the string is an integer number,
     * false for otherwise
     */    
    public static boolean isPosIntNumber(String s)
    {
        // Check string length
        if(s.length() == 0) return false; 
        // Check sign
        if(s.startsWith("+"))
            s = s.substring(1); 
        // Check other characters
        for(int i = 0; i < s.length(); i++)
        {
            char ch = s.charAt(i);
            if(!Character.isDigit(ch) && ch != '+')
                return false;
        }
        // Check '0'
        if(s.startsWith("0"))
            return false;
        return true;
    }
    
    public static int parseData(String filename, Vector data)
    {
        Vector indData = new Vector();
        int nTokens = 0;
        try
	{
            // Read in the data file
            BufferedReader in = new BufferedReader(new FileReader(filename));
            // Read the first line
            String firstLine = in.readLine();
            if(firstLine == null) 
                return -1;
            
            // Tokenize the first line and put them in a String[]
            StringTokenizer lineToken = new StringTokenizer(firstLine, " ,\t", false);
            nTokens = lineToken.countTokens();
            String[] firstLineTokens = new String[nTokens];
            for(int i = 0; i < nTokens; i++)
                firstLineTokens[i] = lineToken.nextToken();

            // The first token of the first line is the individual ID
            String firstToken = firstLineTokens[0];
          
            // Add the String[] containing the first line tokens in Vector indData     
            indData.add(firstLineTokens);

            // Process the following lines
            boolean done = false;
            while(!done)
            {
                // Read a line
                String line = in.readLine();
    
                if(line == null) 
                { 
                    done = true;
                    data.add(indData);
		}
                else
		{
                    // Tokenize the line and put the tokens in a String[]
                    lineToken = new StringTokenizer(line, " ,\t", false);
                    String[] tokens = new String[nTokens];
                    if(lineToken.countTokens() != nTokens)
                        return -1;
                    
                    for(int i = 0; i < nTokens; i++)
                        tokens[i] = lineToken.nextToken();

                    // If the individual ID is the same as the previous line
                    if(tokens[0].equals(firstToken))
                    {
                        // Add the line of tokens to the Vector indData
                        indData.add(tokens);
		    }
                    // Otherwise
                    else
		    {
                        // Add the Vector indData to Vector data
                        data.add(indData);

                        // Create a new Vector indData for the new individual
                        indData = new Vector();

                        // Add the line of tokens to the new indData
                        indData.add(tokens);

                        // Update the first token that is the individual ID
                        firstToken = tokens[0];           
		    }                      
		}
            }
            in.close();
	}
        catch(IOException ioe )
        {
            JOptionPane.showMessageDialog(null, "Error openning data file.",   
                                          "File Error",    
                                          JOptionPane.ERROR_MESSAGE);            
        }

        return nTokens;
    }
    
    /** Get a help document
     * @param name A String object containing the name of the help document
     * @return A String object containing the text of the help document
     */    
    public String getHelpDocument(String name)
    {            
        StringBuffer buffer = new StringBuffer();
        try
        {
            InputStream in = getClass().getResourceAsStream(
                             "/uw/rfpk/mda/nonmem/wizard/" + name + ".help"); 
            BufferedReader reader = new BufferedReader(new InputStreamReader(in));
            boolean done = false;
            while(!done)
            {
                // Read a line
                String line = reader.readLine();                            
                if(line == null) 
                    done = true;
                else
                    buffer.append(line).append("\n");
            }	    
            reader.close();
            in.close();
        }
        catch(IOException ioe )
	{
        } 
        return buffer.toString();
    }     
}
