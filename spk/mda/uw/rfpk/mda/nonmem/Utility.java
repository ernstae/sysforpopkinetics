/**********************************************************************
From:   Resource Facility for Population Kinetics                    
        Department of Bioengineering Box 352255                      
        University of Washington                                     
        Seattle, WA 98195-2255                                       

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
**********************************************************************/
package uw.rfpk.mda.nonmem;

import javax.swing.JButton;
import javax.swing.DefaultListModel;
import javax.swing.JOptionPane;
import java.util.Vector;
import java.util.StringTokenizer;
import java.io.*;
import javax.jnlp.*;
import java.net.URL;
import org.apache.xerces.parsers.DOMParser;
import org.apache.xerces.dom.DocumentImpl;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

/**
 * This class defines static functions used by steps.
 * @author  Jiaji Du
 */
public class Utility {
    
    /** Creates a new instance of Utility */
    public Utility() {
    }
    
    /** Eliminates comments in a text.
     * @param text a String object containing the text to have comments eliminited.
     * @return a String object containing the text without comments.
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
     * @param record a String object containing the text to search in.
     * @param s a String object containing the character string described above. 
     * @return an int, the largest number found as described above.
     */    
    public static int find(String record, String s)
    {
        String[] strs = ("a" + record).split(s); 
        int m = 0, n = 0;
        for(int i = 1; i < strs.length; i++)
        {   
            String str = strs[i].trim();
            int endIndex = str.length();
            if(str.startsWith("("))
            {
                str = str.substring(1).trim();
                if(str.indexOf(")") != -1)
                    endIndex = str.indexOf(")");
                else
                    return 0;
            }
            else
            {
                for(int j = 0; j < str.length(); j++)
                    if(!Character.isDigit(str.charAt(j)))
                    {
                        endIndex = j;
                        break;
                    }
            }
            if(endIndex > 0 && isPosIntNumber(str.substring(0, endIndex).trim()))
                m = Integer.parseInt(str.substring(0, endIndex).trim());
            else
                return 0;
            if(m > n) n = m;
        }      
        return n;
    }
    
    /** Prepare the default $PK code.
     * @param advan an int, the ADVAN number.
     * @param trans an int, the TRANS number.
     * @return a String object, the default $PK code.
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
    
    /** Set "Up" and "Down" buttons.
     * @param index an int, the selected index in the list.
     * @param model a DefaultListModel object, the model for the list.
     * @param upButton a JButton object, the "Up" button.
     * @param downButton a JButton object, the "Down" button.
     */    
    public static void setUpDownButton(int index, DefaultListModel model, JButton upButton, JButton downButton)
    {
        upButton.setEnabled(index > 0);
        downButton.setEnabled(index < model.getSize() - 1);
    }
    
    /** Determine if a character sting represents a floating point number.
     * @param s a String object containing the the character string.
     * @return a boolean, true for the string is a floating point number,
     * false for otherwise.
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
    
    /** Determine if a character sting represents an integer number.
     * @param s a String object containing the character string.
     * @return a boolean, true for the string is an integer number,
     * false for otherwise.
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
    
    /** Parse the data XML document and put the data in a Vector object.
     * @return an int value that is the number of data columns in the data file.
     * In caes of error, -1 is returned.
     * @param dataXML the data in XML format.
     * @param data a Vector in which each contained object is also a Vector that
     * contains the data of one individual.  The individual data vector contains
     * String arrays.  The number of arrays is the number of the data records
     * for the individual.  Each array contains data items of number of columns.
     * @param isInd a boolean true for individual analysis, false for population analysis.
     */        
    public static int parseDataXML(String dataXML, Vector data, boolean isInd)
    {
        int nDataCol = -1;
        Document docData = null;
        Element row, value;
        try
        {
            // Parse the XML documents
            DOMParser parser = new DOMParser(); 
            parser.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes()))); 
            docData = parser.getDocument();            
        }
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e, "SAXException", JOptionPane.ERROR_MESSAGE);
            return -1;
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            return -1;
        }    
        
        //Get root element of spkdata
        Element spkdata = docData.getDocumentElement();  
        
        // Get rows
        NodeList rowList = spkdata.getElementsByTagName("row"); 
        if(rowList.getLength() > 1)
        {
            Vector indData = new Vector();
            
            // Put the first row in a String[]
            row = (Element)rowList.item(1);
            NodeList valueList = row.getElementsByTagName("value");
            nDataCol = valueList.getLength();

            String[] firstRowItems = new String[nDataCol];
            for(int j = 0; j < nDataCol; j++)
            {
                value = (Element)valueList.item(j);
                firstRowItems[j] = value.getFirstChild().getNodeValue();                 
            }
            indData.add(firstRowItems);            
            
            String firstItem = null;
            if(!isInd)
                firstItem = firstRowItems[0];       
            
            for(int i = 2; i < rowList.getLength(); i++)
            {
                row = (Element)rowList.item(i);
                valueList = row.getElementsByTagName("value");

                String[] items = new String[nDataCol];
                for(int j = 0; j < nDataCol; j++)
                {
                    value = (Element)valueList.item(j);
                    items[j] = value.getFirstChild().getNodeValue();                 
                }
                
                if(isInd)
                    indData.add(items);
                else
                {
                    if(items[0].equals(firstItem))
                        indData.add(items);
                    else
                    {
                        data.add(indData);
                        indData = new Vector();
                        indData.add(items);
                        firstItem = items[0];
                    }
                }
            }
            data.add(indData);
        }        
        return nDataCol;        
    }  
        
    /** Parse the data file and put the data in a Vector object.
     * @param filename a String object containing the data file name.
     * @param data a Vector in which each contained object is also a Vector that
     * contains the data of one individual.  The individual data vector contains 
     * String arrays.  The number of arrays is the number of the data records 
     * for the individual.  Each array contains data items of number of columns.
     * @param isInd a boolean true for individual analysis, false for population analysis.
     * @return a int value that is the number of data columns in the data file.
     * In case of error, -1 is returned.
     */        
    public static int parseDataFile(String filename, Vector data, boolean isInd)
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
            
            String firstToken = null;
            if(!isInd)
            {
                // The first token of the first line is the individual ID
                firstToken = firstLineTokens[0];
            }
            
            // Add the String[] containing the first line tokens in Vector indData     
            indData.add(firstLineTokens);

            // Process the following lines
            boolean done = false;
            while(!done)
            {
                // Read a line
                String line = in.readLine();
    
                if(line == null || line.trim().equals(""))                   
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

                    if(isInd)
                    {
                        // Add the line of tokens to the Vector indData
                        indData.add(tokens);                         
                    }
                    else
                    {
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
            }
            in.close();
	}
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e,   
                                          "File Error",    
                                          JOptionPane.ERROR_MESSAGE);
        }

        return nTokens;
    }    
    
    /** Determine if a character sting includes character '<' or '>'.
     * @param text a String object containing the character string to be checked.
     * @param name a String object containing the character string as the name.
     * @return a boolean, true for the string includes the character,
     * false for otherwise.
     */    
    public static boolean checkTag(String text, String name)
    {
        if(text.indexOf("<") != -1)
        {
            JOptionPane.showMessageDialog(null, name + " may not include '<'",  
                                          "Input Error",               
                                          JOptionPane.ERROR_MESSAGE);
            return true;
        }                
        else if(text.indexOf(">") != -1)
        {
            JOptionPane.showMessageDialog(null, name + " may not include '>'",  
                                          "Input Error",               
                                          JOptionPane.ERROR_MESSAGE);
            return true;
        }
        return false;
    }

    /** Determine if a character sting is a name of a standard data item.
     * @return a boolean, true for it is a name of a standard data item,
     * false for otherwise.
     * @param item a String object containing a data item name.
     */      
    public static boolean isStdItem(String item)
    {
        String[] stdItems = new String[] { "DV", "MDV", "EVID", "TIME", "DATE", 
                                           "DATE1", "DATE2", "DATE3", "AMT", "RATE", "SS", 
                                           "ADDL", "II", "ABS", "LAG", "UPPER", "LOWER", "L1", 
                                           "L2", "CMT", "PCMT", "CALL", "CONT" }; 
        for(int i = 0; i < stdItems.length; i++)
            if(stdItems[i].equals(item))
                return true;
        return false;    
    }
    
    /** This function format the XML file for better deadability.
     * @return a String object as the formated file content.
     * @param text a String object containing a one-line XML document.
     */    
    public static String formatXML(String text)
    {
        StringBuffer buffer = new StringBuffer();
        int length = text.length();
        char pre = 'p';
        String indent = "\n";
        for(int i = 0; i < length; i++)
	{
            char c = text.charAt(i);
	    if(c == '<' && pre == '>' )
            {
                if(text.charAt(i + 1) != '/')
                    indent += "   ";
                buffer.append(indent); 
            }
            if(c == '/')
                if(pre == '<' || text.charAt(i + 1) == '>') 
                    if(indent.length() >= 3)
                        indent = indent.substring(0, indent.length() - 3);
            buffer.append(c);  
            if(c == '\n' && pre != '>' && text.charAt(i + 1) == '<')
                buffer.append(indent.substring(1));
            pre = c;
	}
        return buffer.toString();
    } 
    
    /** This function format the data for better deadability.
     * @return a String object as the formated file content.
     * @param n sum of number of significent digits and three.
     * @param value a String object containg the data value.
     */      
    public static String formatData(int n, String value)
    {
        if(!value.startsWith("-"))
            value = " " + value;
        if(value.charAt(n) != '-')
        {
            StringBuffer sb = new StringBuffer(value);
            value = sb.insert(n, '+').toString();
        }
        return value; 
    }
    
    /** This function opens an URL.
     * @param url a url to display.
     */
    public static void openURL(String url)
    {
        // Find a browser
        if(System.getProperty("os.name").startsWith("Windows"))
        {
            try 
            {
               // Lookup the javax.jnlp.BasicService object
               BasicService bs = (BasicService)ServiceManager.lookup("javax.jnlp.BasicService");

               // Invoke the showDocument method
               bs.showDocument(new java.net.URL(url));
            } 
            catch(UnavailableServiceException ue) 
            {
                JOptionPane.showMessageDialog(null, ue, "UnavailableServiceException", 
                                              JOptionPane.ERROR_MESSAGE); 
            }
            catch(java.net.MalformedURLException me)
            {
                JOptionPane.showMessageDialog(null, me, "MalformedURLException", 
                                              JOptionPane.ERROR_MESSAGE);             
            }            
        }
        else
        {
            try
            {
                String path = System.getProperty("user.dir") + "/.browser";
                File file = new File(path);
                if(file.exists())
                {
                    BufferedReader reader = new BufferedReader(new FileReader(path));
                    Runtime.getRuntime().exec(new String[]{reader.readLine(), url});
                    return;
                }
                
                String[][] command = {{"which", "firebird"},
                                      {"which", "mozilla"},
                                      {"which", "netscape"},
                                      {"which", "opera"},
                                      {"which", "galeon"},
                                      {"which", "konqueror"},
                                      {"which", "lynx"}};
           
                Process process = null;
                int status = 1;
                int i = 0;
                while(status == 1 && i < command.length)
                {
                    process = Runtime.getRuntime().exec(command[i]);
                    status = process.waitFor();
                    i++;
                }
                if(status == 0)
                    Runtime.getRuntime().exec(new String[]{command[i - 1][1], url});
                else
                {
                    String browser = JOptionPane.showInputDialog(null, "Enter web browser name.");
                    if(browser != null)
                    {
                        process = Runtime.getRuntime().exec(new String[]{"which", browser.toLowerCase()});
                        if(process.waitFor() == 0)
                        {
                            Runtime.getRuntime().exec(new String[]{browser.trim().toLowerCase(), url});
                            BufferedWriter writer = new BufferedWriter(new FileWriter(file));
                            writer.write(browser.trim().toLowerCase());
                            writer.close();
                        }
                        else
                            JOptionPane.showMessageDialog(null, "Web browser, " + browser +
                                                          ", was not found on this computer.", 
                                                          "Web Browser Error", 
                                                          JOptionPane.ERROR_MESSAGE);
                    }
                }                    
            }
            catch(IOException e)
            {
                JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);                 
            }
            catch(InterruptedException e)
            {
                JOptionPane.showMessageDialog(null, e, "InterruptedException", JOptionPane.ERROR_MESSAGE);              
            }
        }
    }
    
    /** Check if math functions used in the step are compatible with NONMEM.
     * @param text the program to be checked.
     * @param step the step title.
     * @return a Vector containing the names of the functions not supported by NONMEM in the text. 
     */
    public static Vector checkMathFunction(String text, String step)
    {
        String[] functions = {"ABS", "ACOS", "ASIN", "ATAN", "ATAN2", "COS", "COSH",
                              "MAX", "MIN", "MOD", "SIN", "SINH", "TAN", "TANH"};
        Vector names = new Vector();                   
        for(int i = 0; i < functions.length; i++)
        {
            if(text.indexOf(functions[i]) != -1)
            {
                names.add(functions[i]);
                JOptionPane.showMessageDialog(null, "If you use '" + functions[i] + "' as a math function call" +
                                              " in the '" + step + "' your control file may be incompatible with NONMEM.",
                                              "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
        }
        return names;
    }
    
    /** check if parenthesis are mismatched.
     * @param text the program to be checked.
     * @param step the step title.
     * @return a Vector containing the line numbers of the lines with mismatched parenthesis.
     */
    public static Vector checkParenthesis(String text, String step)
    {
        String[] lines = text.split("\n");
        Vector mismatches = new Vector();
        int n;
        for(int i = 0; i < lines.length; i++)
        {
            n = 0;
            for(int j = 0; j < lines[i].length(); j++)
            {
                if(lines[i].charAt(j) == ';')
                    break;
                if(lines[i].charAt(j) == '(')
                    n++;
                if(lines[i].charAt(j) == ')')
                    n--;                
            }
            if(n != 0)
            {
                mismatches.add(new Integer(i));
                if(n > 0)
                    JOptionPane.showMessageDialog(null, "Parenthesis mismatch was found in '" + step +
                                                  "' line " + (i + 1) + ", ' ) ' is expected.", "Input Error",
                                                  JOptionPane.ERROR_MESSAGE);
                else
                    JOptionPane.showMessageDialog(null, "Parenthesis mismatch was found in '" + step +
                                                  "' line " + (i + 1) + ", ' ( ' is expected.\n" + 
                                                  "Please click the 'Back' button and correct it.", "Input Error",
                                                  JOptionPane.ERROR_MESSAGE);
            }
        }
        return mismatches;
    }
}
