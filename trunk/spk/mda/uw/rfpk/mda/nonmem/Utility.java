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

import uw.rfpk.mda.nonmem.Output;
import javax.swing.JButton;
import javax.swing.DefaultListModel;
import javax.swing.JOptionPane;
import java.util.Vector;
import java.util.regex.*;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.io.*;
import javax.jnlp.*;
import java.net.URL;
import javax.xml.parsers.*;
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
     * @param input a String object containing the text to search in.
     * @param s a String object containing the character string described above. 
     * @return an int, the largest number found as described above.
     */    
    public static int find(String input, String s)
    {
        String regExp = "\\b" + s + "\\s*\\(?(\\d+)\\)?[\\s|\\)|+|-|*|/|\n|$]";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        Matcher matcher = pattern.matcher(" " + input.toUpperCase());
        Vector<String> list = new Vector<String>();
        if(matcher.find())
        {
            list.add(matcher.group(1));
            while(matcher.find())
                if(list.indexOf(matcher.group(1)) == -1)
                    list.add(matcher.group(1));
        }
        return list.size();
    }

    /** Add ETA(n) to THETA(n) in a text, i.e., THETA(n)+ETA(n).
     * @param text a text to be modified.
     * @return the modified text.
     */
    public static String addEtaToTheta(String text)
    {
        String n;
        String regExp = "\\bTHETA\\s*\\(?(\\d+)\\)?[\\s|\\)|+|-|*|/|\n|$]";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        Matcher matcher = pattern.matcher(" " + text.toUpperCase());
        while(matcher.find())
        {
            n = matcher.group(1);
            regExp = "\\bTHETA\\s*\\(?" + n + "?[\\s|\\)|+|-|*|/|\n|$]";
            text = text.replaceAll(regExp, "(THETA(" + n + ")+ETA(" + n + "))");
        }
        return text;
    }
    
    /** Replace all occurrences of ETA(n) by EPS(n) in a text.
     * @param text a text to be modified.
     * @return the modified text.
     */
    public static String replaceEtaByEps(String text)
    {
        String n;
        String regExp = "\\bETA\\s*\\(?(\\d+)\\)?[\\s|\\)|+|-|*|/|\n|$]";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        Matcher matcher = pattern.matcher(" " + text.toUpperCase());
        while(matcher.find())
        {
            n = matcher.group(1);
            regExp = "\\bETA\\s*\\(?" + n + "?[\\s|\\)|+|-|*|/|\n|$]";
            text = text.replaceAll(regExp, "EPS(" + n + ")");
        }        
        return text;
    }
    
    /** Extract omega values from source.
     * @param source source text.
     * @return a String object of text of values: value value ... value,
     *         or null if omega is not found.
     */
    public static String getOmegaValues(String source)
    {
        StringBuffer values = new StringBuffer();
        int i1 = source.indexOf("<omega");
        int i2 = source.indexOf("</omega>");
        if(i1 == -1 || i2 == -1) return null;
        source = source.substring(i1, i2);
        i2 = 0;
        while(true)
        {
            i1 = source.indexOf("<value", i2);
            if(i1 == -1) break;
            i1 = source.indexOf(">", i1) + 1;
            i2 = source.indexOf("</value>", i1);
            if(i2 == -1) return null;
            values.append(source.substring(i1, i2));
            values.append(" ");
        }
        return values.toString().trim();   
    }
    
    /** Find bandwidth of a matrix in SPK format.
     * @param cov a String[] containing a row-major, lower-triangle cov matrix.
     * @return the bandwidth of the matrix.
     */
    public static int bandWidth(String[] cov)
    {
        if(cov[3].endsWith("F")) return 0;
        int dimension = Integer.parseInt(cov[1]);
        String[][] matrix = new String[dimension][];
        int k = 3;
        for(int i = 0; i < dimension; i++)
        {
            matrix[i] = new String[i + 1];
            for(int j = 0; j <= i; j++)
                matrix[i][j] = cov[k++];
        }
        for(int i = dimension - 1; i >= 0; i--)
        {
            if(zero(matrix[i][0]))
            {
                k = i + 1;
                for(int j = 1; j < dimension - i; j++)
                    if(!zero(matrix[k++][j]))
                        return i + 1;                    
            }
            else
                return i + 1;
        }
        return 0;
    }
    
    /** Convert a full matrix to a banded matrix by fixing the off-band elements.
     * @param cov a String[] containing a full cov matrix.
     * @param bandwidth bandwidth of the cov matrix. 
     */
    public static void bandedMatrix(String[] cov, int bandwidth)
    {
        int dimension = Integer.parseInt(cov[1]);
        String[][] matrix = new String[dimension][];
        int k = 3;
        for(int i = 0; i < dimension; i++)
        {
            matrix[i] = new String[i + 1];
            for(int j = 0; j <= i; j++)
                matrix[i][j] = cov[k++];
        }
        for(int i = dimension - 1; i >= bandwidth; i--)
        {
            k = i;
            for(int j = 0; j < dimension - i; j++)
            {
                matrix[k][j] = matrix[k][j] + "F";
                k++;
            }
        }
        k = 3;
        for(int i = 0; i < dimension; i++)
            for(int j = 0; j <= i; j++)
                cov[k++] = matrix[i][j];
    }
    
    private static boolean zero(String s)
    {
        for(int i = 0; i < s.length(); i++)
            if((int)s.charAt(i) >= 49 && (int)s.charAt(i) <= 57)
                return false;
        return true;
    }
    
    /** Convert a full matrix of unit bandwidth to a diagonal matrix.
     * @param cov a String[] containing a full cov matrix of unit bandwidth.
     * @return a String[] containing the converted diagonal matrix.
     */
    public static String[] diagonalMatrix(String[] cov)
    {
        int dimension = Integer.parseInt(cov[1]);
        String[] dia = new String[dimension + 3];
        dia[0] = "diagonal";
        dia[1] = cov[1];
        dia[2] = cov[2];
        int k = 3;
        int l = 3;
        for(int i = 0; i < dimension; i++)
            for(int j = 0; j <= i; j++)
            {
                if(i == j) dia[l++] = cov[k];
                k++;   
            }
        return dia;
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
        double d;
        try
        {
            d = Double.parseDouble(s);
        }
        catch(NumberFormatException e)
        {
            return false;   
        }
        return true;
        
/*        // Check string length
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
*/
    }
    
    /** Determine if a character sting represents an positive floating point number.
     * @param s a String object containing the the character string.
     * @return a boolean, true for the string is an positive floating point number,
     * false for otherwise.
     */    
    public static boolean isPosFloatNumber(String s)
    {        
        double d;
        try
        {
            d = Double.parseDouble(s);
        }
        catch(NumberFormatException e)
        {
            return false;   
        }
        if(d <= 0)
            return false;
        return true;
    }
    
    /** Determine if a character sting represents an positive integer number.
     * @param s a String object containing the character string.
     * @return a boolean, true for the string is an positive integer number,
     * false for otherwise.
     */    
    public static boolean isPosIntNumber(String s)
    {
        long i;
        try
        {
            i = Long.parseLong(s);   
        }
        catch(NumberFormatException e)
        {
            return false;   
        }
        if(i <= 0)
            return false;
        return true;
    }

    /** Determine if a character sting represents a  non-negative integer number.
     * @param s a String object containing the character string.
     * @return a boolean, true for the string is a non-negative integer number,
     * false for otherwise.
     */    
    public static boolean isNonNegIntNumber(String s)
    {
        int i;
        try
        {
            i = Integer.parseInt(s);   
        }
        catch(NumberFormatException e)
        {
            return false;   
        }
        if(i < 0)
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
     * @return a String array containing the data labels.
     */        
    public static String[] parseDataXML(String dataXML, Vector<Vector<String[]>> data)
    {
        int nDataCol = -1;
        String[] labels = null;
        Document docData = null;
        Element row, value;
        try
        {
            // Parse the XML documents
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docData = builder.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes())));
//            DOMParser parser = new DOMParser(); 
//            parser.parse(new InputSource(new ByteArrayInputStream(dataXML.getBytes()))); 
//            docData = parser.getDocument();            
        }
        catch(ParserConfigurationException e)
        {
            JOptionPane.showMessageDialog(null, e, "ParserConfigurationException", JOptionPane.ERROR_MESSAGE);
            return null;
        }
        catch(SAXException e)
        {
            JOptionPane.showMessageDialog(null, e, "SAXException", JOptionPane.ERROR_MESSAGE);
            return null;
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            return null;
        }    
        
        //Get root element of spkdata
        Element spkdata = docData.getDocumentElement();  
        
        // Get rows
        NodeList rowList = spkdata.getElementsByTagName("row"); 
        if(rowList.getLength() > 1)
        {
            Vector<String[]> indData = new Vector<String[]>();
            
            // Find the data lablel
            row = (Element)rowList.item(0);
            NodeList valueList = row.getElementsByTagName("value");
            nDataCol = valueList.getLength();
            labels = new String[nDataCol];
            for(int j = 0; j < nDataCol; j++)
            {
                value = (Element)valueList.item(j);
                labels[j] = value.getFirstChild().getNodeValue();                 
            }            
            
            // If first label is not ID, it is an individual dataset
            boolean isInd = !labels[0].equals("ID");
            
            // Put the first row in a String[]
            row = (Element)rowList.item(1);
            valueList = row.getElementsByTagName("value");
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
                        indData = new Vector<String[]>();
                        indData.add(items);
                        firstItem = items[0];
                    }
                }
            }
            data.add(indData);
        }        
        return labels;        
    }  
        
    /** Parse the data file and put the data in a Vector object.
     * @param filename a String object containing the data file name.
     * @param data a Vector in which each contained object is also a Vector that
     * contains the data of one individual.  The individual data vector contains 
     * String arrays.  The number of arrays is the number of the data records 
     * for the individual.  Each array contains data items of number of columns.
     * @param isInd a boolean true for individual analysis, false for population analysis.
     * @return a String array containing the data labels in the comment line.
     * In case of error, null is returned.
     */        
    public static String[] parseDataFile(String filename, Vector<Vector<String[]>> data, boolean isInd)
    {
        Vector<String[]> indData = new Vector<String[]>();
        String[] labels = null;
        try
	{
            // Read in the data file
            BufferedReader in = new BufferedReader(new FileReader(filename));
            // Read the first line
            String firstLine = in.readLine();
            if(firstLine == null) 
                return null;
            

            boolean hasComment = false;
            if(firstLine.startsWith("C") || firstLine.startsWith("c") || firstLine.startsWith("#"))
            {
                hasComment = true;
                firstLine = firstLine.substring(1).trim();
                StringTokenizer commentToken = new StringTokenizer(firstLine, " ,\t", false);
                int nLabels = commentToken.countTokens();
                labels = new String[nLabels];
                for(int i = 0; i < nLabels; i++)
                    labels[i] = commentToken.nextToken().toUpperCase();
                firstLine = in.readLine();
            }
                      
            // Tokenize the first line and put them in a String[]
            StringTokenizer lineToken = new StringTokenizer(firstLine, " ,\t", false);
            int nTokens = lineToken.countTokens();
            if(hasComment && nTokens != labels.length)
            {
                JOptionPane.showMessageDialog(null, "Data labels are not consistent with data columns",
                                              "Data file error", JOptionPane.ERROR_MESSAGE);
                return null;
            }
            if(!hasComment)
            {
                labels = new String[nTokens];
                for(int i = 0; i < nTokens; i++)
                     labels[i] = "";
            }
            
            String[] firstLineTokens = new String[nTokens];
            for(int i = 0; i < nTokens; i++)
            {
                firstLineTokens[i] = lineToken.nextToken();
                if(firstLineTokens[i].equals("."))
                    firstLineTokens[i] = "0.0";
                try
                {
                    Double.valueOf(firstLineTokens[i]);
                }
                catch(NumberFormatException e)
                {
                    JOptionPane.showMessageDialog(null, "A number format error was found in the data file." +
                                                  "\nColumn " + (i + 1) + ".",   
                                                  "Number Format Error", JOptionPane.ERROR_MESSAGE);
                    return null;
                }
            }
            
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
                    {
                        JOptionPane.showMessageDialog(null, "Number of columns is incorrect.", 
                                                      "Data file error", JOptionPane.ERROR_MESSAGE);
                        return null;
                    }
                    
                    for(int i = 0; i < nTokens; i++)
                    {
                        tokens[i] = lineToken.nextToken();
                        if(tokens[i].equals("."))
                            tokens[i] = "0.0";
                        try
                        {
                            Double.valueOf(tokens[i]);
                        }
                        catch(NumberFormatException e)
                        {
                            JOptionPane.showMessageDialog(null, "A number format error was found in the data file." +
                                                          "\nColumn " + (i + 1) + ".",   
                                                          "Number Format Error", JOptionPane.ERROR_MESSAGE);
                            return null;
                        }                        
                    }

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
                            indData = new Vector<String[]>();

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

        return labels;
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
        if(String.valueOf(text.charAt(text.indexOf(">") + 1)).equals("<"))
            text = text.replaceFirst("><", ">\n<");
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
        String[] functions = {"ABS", "ACOS", "ASIN", "ATAN", "ATAN2", "COSH",
                              "MAX", "MIN", "MOD", "SINH", "TAN", "TANH"};
        Vector<String> names = new Vector<String>();                   
        for(int i = 0; i < functions.length; i++)
        {
            if(Pattern.compile("\\b" + functions[i] + "\\b", Pattern.UNIX_LINES).matcher(text).find())
            {
                names.add(functions[i]);
                JOptionPane.showMessageDialog(null, "If you use '" + functions[i] + "' as a math function call" +
                                              " in " + step + " your control file may be incompatible with NONMEM.",
                                              "Warning Message", JOptionPane.WARNING_MESSAGE);
            }
        }
        return names;
    }

    /** Check data labels.
     * @param labels String array containing the data labels to check.
     * @return true if all the labes are good, false otherwise.
     */
    public static boolean checkDataLabels(String[] labels)
    {
        boolean ok = true;
        String[] phrases = {"A", "DADT", "THETA", "ETA", "EPS", "P", "PRED", "IPRED", "CPRED", "RES", "CRES", "IRES",
                            "CWRES", "IWRES", "WRES", "F", "Y", "[S|F|R|D]\\d+", "ALAG\\d+"};
        for(String label: labels)
        {
            for(String phrase: phrases)
                if(label.matches(phrase))
                {
                    JOptionPane.showMessageDialog(null, "'" + label + "' is a reserved phrase.\n" +
                                                  "It cannot be used as a data label",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    ok = false;
                    break;
                }
        }
        return ok;
    }
    
    /** Check if END IF is contained in the text.
     * @param text the code to be checked.
     * @param step the step title.
     */
    public static boolean checkENDIF(String text, String step)
    {                 
        if(Pattern.compile("\\bEND IF\\b", Pattern.UNIX_LINES).matcher(text).find())
        {
            JOptionPane.showMessageDialog(null, "A syntax error 'END IF' was found in " + step + "." +
                                          " The correct syntax is 'ENDIF'. Please correct it.",
                                          "Error Message", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;
    }
    
    /** check if parentheses are mismatched.
     * @param text the program to be checked.
     * @param step the step title.
     * @return a Vector containing the line numbers of the lines with mismatched parenthesis.
     */
    public static Vector checkParenthesis(String text, String step)
    {
        String[] lines = text.split("\n");
        Vector<Integer> mismatches = new Vector<Integer>();
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
                                                  "Please correct it.", "Input Error",
                                                  JOptionPane.ERROR_MESSAGE);
            }
        }
        return mismatches;
    }
    
    /** Check if any improper parentheses appear on the left hand side of an expression.
     * @param text the program to be checked.
     * @param step the step title.
     * @return a Vector containing the line numbers of the lines with improper parenthesis.
     */
    public static Vector checkLeftExpression(String text, String step)
    {
        if(step.equals("Model Equations") || step.equals("Model Parameters"))
            text = text.replaceAll("\\bP\\(\\d+\\)", "");
        if(step.equals("Differential Equation Structure"))
            text = text.replaceAll("\\bDADT\\(\\d+\\)", "");
        String[] lines = text.split("\n");
        Vector<Integer> errors = new Vector<Integer>();
        String regExp = "\\b\\w+\\(\\d+\\)\\s*=";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        int k;
        for(int i = 0; i < lines.length; i++)
        {
            k = lines[i].indexOf("=");
            if(k != -1)
            {
                String left = lines[i].substring(0, k + 1);
                if(pattern.matcher(left).find())               
                {
                    errors.add(new Integer(i));
                    JOptionPane.showMessageDialog(null, "The array element on the left hand side of\nline " + 
                                                  (i + 1) + " in '" + step + "' is invalid.", 
                                                  "Input Error",
                                                  JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return errors;
    }
    
    /** Generate a dataset by replacing DV by simulated data from parent job's report.
     * @param report the report XML as a String object.
     * @param source the source XML as a String object.
     * @param data the dataset XML as a String object
     * @return the new dataset as a String object.
     */
    public static String replaceDataDVbySimDV(String report, String source, String data)
    {
        String presentation_data = "<?xml version=\"1.0\"?>\n" + 
                                   report.substring(report.indexOf("<presentation_data "), 
                                                    report.lastIndexOf("</presentation_data>") + 20);
        String[] dataLabels = XMLReader.getDataLabels(source);
        String[][] dataset = XMLReader.getData(presentation_data, dataLabels);
        String description = data.substring(data.indexOf("<description>") + 13, 
                                            data.indexOf("</description>")).trim();
        return XMLWriter.setData(dataset, dataLabels, description) + "\n";     
    }
    
    /** Modefy the model by replacing the initial conditions by the estimates in 
     * parent job's report.
     * @param model the model XML as a String object.
     * @param report the report XML as a String object.
     * @return the modefied model XML as a String object.
     */
    public static String replaceModelParameters(String model, String report)
    {
        String parameter_out = "<?xml version=\"1.0\"?>\n<parameter_out>\n" +
                               report.substring(report.indexOf("<theta_out "), 
                                                report.lastIndexOf("</theta_out>") + 12) + "\n" +
                               report.substring(report.indexOf("<omega_out "), 
                                                report.lastIndexOf("</omega_out>") + 12) + "\n" +
                               report.substring(report.indexOf("<sigma_out "), 
                                                report.lastIndexOf("</sigma_out>") + 12) +
                               "\n</parameter_out>";
        
        Output output = new Output();
        if(new XMLReader().getParameters(parameter_out, output))
        {
            StringTokenizer records = new StringTokenizer(model, "$");
            int nTokens = records.countTokens();
            model = "";
            nOmega = 0;
            nSigma = 0;
            for(int i = 0; i < nTokens; i++)
            {
                String value = records.nextToken();
                if(value.startsWith("THETA"))
                    value = "THETA" + initTheta(value.substring(5).trim(), output.theta) + "\n";
                if(value.startsWith("OMEGA"))
                    value = "OMEGA " + initCov(value.substring(5).trim(), output.omega[nOmega++]) + "\n";
                if(value.startsWith("SIGMA"))
                    value = "SIGMA " + initCov(value.substring(5).trim(), output.sigma[nSigma++]) + "\n";                
                model += "$" + value;
            }
        }
        else
        {
            return null;
        }
        return model;
    }
    
    private static String initTheta(String original, String[] replacement)
    {
        String thetas = "";
        while(original.indexOf("\n\n") != -1)
            original = original.replaceAll("\n\n", "\n");
        String[] theta = original.split("\n");
        for(int i = 0; i < theta.length; i++)
        {
            if(theta[i].endsWith("FIXED)"))
                theta[i] = "\n(" + replacement + " FIXED)";
            else
                theta[i] = "\n" + theta[i].split(",")[0] + "," + replacement[i] + "," + theta[i].split(",")[2];
            thetas += theta[i];
        }
        return thetas;
    }
    
    private static String initCov(String original, String[][] replacement)
    {
        String cov = " " + original;
        if(original.indexOf("SAME") == -1)
        {
            int dimension = replacement.length;
            if(original.startsWith("BLOCK"))
            {
                cov = "BLOCK(" + dimension + ")";
                for(int i = 0; i < dimension; i++)
                    for(int j = 1; j < i + 2; j++)
                        cov += " " + replacement[i][j];
            }
            else
            {
                cov = "DIAGONAL(" + dimension + ")";
                for(int i = 0; i < dimension; i++)
                    cov += " " + replacement[i][i + 1];
            }
        }
        return cov.replaceAll("F", " FIXED");
    }
    
    /** Convert column majar element list to row major element list of a lower triangle matrix.
     * @param colList the element list in column major order.
     * @return rowList the element list in row major order.
     */
    public static String[] convertColToRowMajorOrder(String[] colList)
    {
        int n = ((int)Math.sqrt(1 + 8 * colList.length) - 1) / 2;
        String[] rowList = new String[colList.length];        
        int k = 0;
        for(int i = 0; i < n; i++)
            for(int j = 0; j <= i; j++)
                rowList[k++] = colList[n * j + i - j * (j + 1) / 2];        
        return rowList;
    }
    
    /** Get selected data items from a data block using comma as the delimiter.
     * @param dataLine a block of data with the data labels in the first row.
     * @param selectedLabels data labels of selected data items.
     * @return the selected data items.
     */ 
    public static String[][] getSelectedDataItems(String dataLine, String[] selectedLabels)
    {
        String[] rows = dataLine.trim().split("\n");
        int nRows = rows.length - 1;
        String[] allLabels = rows[0].split(",");
        int nColumns = allLabels.length;
        ArrayList<String> dataItems = new ArrayList<String>(nColumns);
        for(int i = 0; i < nColumns; i++)
            dataItems.add(allLabels[i]);
        int nItems = selectedLabels.length;
        String[][] data = new String[nRows][nItems];
        String[] row;
        for(int i = 0; i < nRows; i++)
        {
            row = rows[i + 1].split(",");
            for(int j = 0; j < nItems; j++)
                data[i][j] = row[dataItems.indexOf(selectedLabels[j])];
        }
        return data;
    }
    
    private static int nOmega = 0;
    private static int nSigma = 0;
    
    /** Diagonalize covariance matrix omega in sourece.
     * @param source original source XML.
     * @return modified source XML
     */
    public static String diagonalizeOmegaSource(String source)
    {
        int index1 = source.indexOf("<omega");
        int index2 = source.indexOf("</omega>");
        String front = source.substring(0, index1);
        String omega = source.substring(index1, index2);
        String back = source.substring(index2);
        omega = omega.replaceFirst("block", "diagonal");
        String[] rows = omega.split("\n");
        omega = rows[0] + "\n               <in>\n";
        int j = 2;
        for(int i = 2; i < rows.length - 1; i += j++)
            omega += "                  " + rows[i] + "\n";
        omega += "               </in>\n            ";
        return front + omega + back;   
    }
    
    /** Diagonalize covariance matrix omega in model.
     * @param model original model.
     * @return modified model
     */
    public static String diagonalizeOmegaModel(String model)
    {
        int index1 = model.indexOf("$OMEGA");
        int index2 = model.indexOf("\n", index1);
        String front = model.substring(0, index1);
        String omega = model.substring(index1, index2);
        String back = model.substring(index2);
        omega = omega.replaceFirst("BLOCK", "DIAGONAL");
        String[] tokens = omega.split(" ");
        omega = "$OMEGA " + tokens[1];
        int j = 2;
        for(int i = 2; i < tokens.length; i += j++)
            omega += " " + tokens[i];
        return front + omega + back;   
    }
    
    /** Check characters in a string.
     * @param str string to be checked.
     * @param place where the unacceptable character is found.
     * @return true if the ASCII code of every character is in the range of 32 and 126,
     *         otherwise false.
     */
    public static boolean checkCharacter(String str, String place) 
    {
        char[] ch = str.toCharArray();
        int c;
        for (int i = 0; i < ch.length; i++) 
        {
            c = ch[i];
	    if (c > 126 || c == 64)
            {
                JOptionPane.showMessageDialog(null, "Unacceptable character found in " + place +".",  
                                              "Input Error",            
                                              JOptionPane.ERROR_MESSAGE);
		return false;
            }
        }
        return true;
    }
    
    /** Remove missing values in a data array.
     * @param data a data array of double values.
     * @return the resulted data array or null if there is no data in the original array.
     */
    public static double[] removeMissingValue(double[] data)
    {
        double value;
        double[] dataOut = null;
        Vector<Double> temp = new Vector<Double>();
        int j = 0;
        int size = data.length;
        for(int i = 0; i < size; i++)
        {
            value = data[i];
            if(value == value && !String.valueOf(value).endsWith("Infinity"))
            temp.add(value);
        }
        if(temp.size() > 0)
        {
            dataOut = new double[temp.size()];
            int i = 0;
            for(double number : temp)
                dataOut[i++] = number;
        }
        else
            JOptionPane.showMessageDialog(null, "No data were found.");
        return dataOut;
    }
    
    /** Remove missing values in two data array. If a data element is missing in one array,
     *  the data element in the other array is also removed.  The data element before the 
     *  specified starting index and after the specified ending index are also removed. If
     *  log scale is specified, non-positive data are removed.
     * @param dataX a data array of double values for x.
     * @param dataY a data array of double values for y.
     * @param start starting index to pick data.
     * @param end ending index to pick data.
     * @param isLogX true for log scale, false for uniform scale in x.
     * @param isLogY true for log scale, false for uniform scale in y.
     * @return the two resulted data arrays or null if there is no data in the original arrays.
     *         The two returned arrays are bundled in an array: first for dataX, last for dataY.
     */
    public static double[][] removeMissingValue(double[] dataX, double[] dataY, int start, int end,
                                                boolean isLogX, boolean isLogY)
    {
        double valueX, valueY;
        double[][] dataOut = null;
        Vector<Double> tempX = new Vector<Double>();
        Vector<Double> tempY = new Vector<Double>();
        int j = 0;
        int size = dataX.length;
        for(int i = start; i <= end; i++)
        {
            valueX = dataX[i];
            valueY = dataY[i];
            if(valueX == valueX && !String.valueOf(valueX).endsWith("Infinity") &&
               valueY == valueY && !String.valueOf(valueY).endsWith("Infinity"))
            {
                if(!isLogX && !isLogY)
                {
                    tempX.add(valueX);
                    tempY.add(valueY);
                }
                else if(isLogX && !isLogY)
                {
                    if(valueX > 0)
                    {
                        tempX.add(valueX);
                        tempY.add(valueY);
                    }
                }
                else if(!isLogX && isLogY)
                {
                    if(valueY > 0)
                    {
                        tempX.add(valueX);
                        tempY.add(valueY);
                    }
                }    
                else
                {
                    if(valueX > 0 && valueY > 0)
                    {
                        tempX.add(valueX);
                        tempY.add(valueY);
                    }
                }
            }
        }
        if(tempX.size() > 0)
        {
            dataOut = new double[2][tempX.size()];
            int i = 0;
            for(double number : tempX)
                dataOut[0][i++] = number;
            i = 0;
            for(double number : tempY)
                dataOut[1][i++] = number;
        }
        return dataOut;
    }
    
    /** Check syntax error in IF conditions and correct them.
     * @param text the code to be checked.
     * @return correct code.
     */
    public static String correctIFConditions(String text)
    {
        int index = text.indexOf("IF");
        if(index != -1)
        {
            index = text.indexOf(".AND.");
            while(index > 0)
            {
                if(text.charAt(index - 1) != ' ')
                    text = text.substring(0, index) + " " + text.substring(index);
                index = text.indexOf(".AND.", index + 6);
            }
            index = text.indexOf(".OR.");
            while(index > 0)
            {
                if(text.charAt(index - 1) != ' ')
                    text = text.substring(0, index) + " " + text.substring(index);
                index = text.indexOf(".OR.", index + 5);
            }
        }
        return text;
    }
    
    /** get the number of sub tasks from source.xml.
     * @param source the source.xml text.
     * @param methodCode method code of the job.
     * @param methodClass method class of the job.
     * @return number of sub tasks of a parallel job.
     */
    public static int findNTasks(String source, String methodCode, String methodClass)
    {
        int nTasks = 0;
        int begin, end;
        if(methodCode.equals("eh") || methodCode.equals("la"))
        {
            begin = source.indexOf(" pop_size=\"", source.indexOf("<pop_analysis ")) + 11;
            end = source.indexOf("\"", begin);
            nTasks = Integer.parseInt(source.substring(begin, end));
        }
        if(methodClass.equals("le"))
        {
            begin = source.indexOf("<in>", source.indexOf("<theta "));
            end = source.indexOf("</in>", begin);
            nTasks += Utility.findNAlp(source.substring(begin, end));
            String text = new String(source);
            while(text.indexOf("<omega ") != -1)
            {
                begin = text.indexOf("<in>", text.indexOf("<omega "));
                end = text.indexOf("</in>", begin);
                nTasks += Utility.findNAlp(text.substring(begin, end));
                text = text.substring(end);
            }
            text = new String(source);
            while(text.indexOf("<sigma ") != -1)
            {
                begin = text.indexOf("<in>", text.indexOf("<sigma "));
                end = text.indexOf("</in>", begin);
                nTasks += Utility.findNAlp(text.substring(begin, end));
                text = text.substring(end);
            }
        }
        return nTasks;
    }
        
    private static int findNAlp(String input)
    {
        int nAlp = 0;
        String regExp = "<value";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        Matcher matcher = pattern.matcher(input);
        while(matcher.find())
            nAlp++;
        regExp = "<value fixed=\"yes\">";
        pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        matcher = pattern.matcher(input);
        while(matcher.find())
            nAlp--;
        return nAlp;
    }
    
    /** Test methods.
     * @param args argument not used.
     */    
    public static void main(String[] args)
    {            
        String source = "<pop_analysis pop_size=\"25\" sig_digits=\"3\">\n" +
            "<theta length=\"4\">\n" +
               "<in>\n" +
                  "<value fixed=\"no\">5</value>\n" +
                  "<value fixed=\"no\">500</value>\n" +
                  "<value fixed=\"no\">3</value>\n" +
                  "<value fixed=\"no\">200</value>\n" +
               "</in>\n" +
            "</theta>\n" +
            "<omega dimension=\"3\" same_as_previous=\"no\" struct=\"block\">\n" +
               "<in>\n" +
                  "<value fixed=\"no\">6</value>\n" +
                  "<value fixed=\"no\">.13</value>\n" +
                  "<value fixed=\"no\">.6</value>\n" +
                  "<value fixed=\"yes\">0</value>\n" +
                  "<value fixed=\"no\">-.057</value>\n" +
                  "<value fixed=\"no\">1</value>\n" +
               "</in>\n" +
            "</omega>\n" +
            "<sigma dimension=\"2\" same_as_previous=\"no\" struct=\"diagonal\">\n" +
               "<in>\n" +
                  "<value fixed=\"no\">.04</value>\n" +
                  "<value fixed=\"yes\">.000001</value>\n" +
               "</in>\n" +
            "</sigma>";
        System.out.println(findNTasks(source, "eh", "al"));
        System.out.println(findNTasks(source, "mc", "le"));
//        System.out.println(correctIFConditions("IF((T.GE.0.AND.T.LE.1).OR.(T.EQ.2.AND.T.EQ.3))"));
//        String[] dataLabels = {"ID", "DV", "TIME", "PRED", "A", "S1","ALAG2", "R3N"};
//        System.out.print(checkDataLabels(dataLabels));
        
//        double[] dataX = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
//        double[] dataY = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
//        dataX[2] = Double.parseDouble("NaN");
//        dataY[5] = Double.parseDouble("Infinity");
//        double[][] dataOut = removeMissingValue(dataX, dataY, 1, 8);
//        for(int i = 0; i < dataOut[0].length; i++)
//            System.out.println(dataOut[0][i] + " : " + dataOut[1][i]);
        
//        double[] data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
//        data[1] = Double.parseDouble("NaN");
//        data[3] = Double.parseDouble("Infinity");
//        data[5] = Double.parseDouble("+Infinity");
//        data[7] = Double.parseDouble("-Infinity");
//        data = removeMissingValue(data);
//        for(int i = 0; i < data.length; i++)
//            System.out.println(data[i]);
//        System.out.println(checkCharacter("øaB1*%$)>@/", "test")); //ø
//        checkENDIF("END IF", "STEP");
//        String model = "FRONT\n$OMEGA BLOCK(4) 1 2 3 4 5 6 7 8 9 10\nBACK";
//        System.out.println(diagonalizeOmegaModel(model));
        
//        String source = "front\n<omega struct=\"block\">\n<in>\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n</in>\n</omega>\nback";
//        System.out.println(diagonalizeOmegaSource(source));
        
//        String dataLine = "A,B,C,D\n11,12,13,14\n21,22,23,24\n31,32,33,34\n";
//        String[] selectedLabels = {"B", "A"};
//        String[][] data = getSelectedDataItems(dataLine,  selectedLabels);
//        for(int i = 0; i < data.length; i++)
//            System.out.println(data[i][0] + " " + data[i][1]);
        
//        String text = " IF(akhj(7)) a2(1) =";
//        checkLeftExpression(text, "");
//        String[] cov = {"block", "4", "no", "11", 
//                                            "0", "22", 
//                                            "0", "0", "33",
//                                            "0", "0", "0", "44"};
//        String[] dia = diagonalMatrix(cov);
//        for(int i = 0; i < dia.length; i++)
//            System.out.println(dia[i]);
        
//        String[] cov = {"", "4", "", "11", 
//                                     "21", "22", 
//                                     "31", "32", "33",
//                                     "41", "0", "43", "44"};
//        int bandwidth = bandWidth(cov);
//        System.out.println(bandwidth);
//        bandedMatrix(cov, bandwidth);
//        System.out.println(cov[3]);
//        System.out.println(cov[4] + " " + cov[5]);
//        System.out.println(cov[6] + " " + cov[7] + " " + cov[8]);
//        System.out.println(cov[9] + " " + cov[10] + " " + cov[11] + " " + cov[12]);
        
//        String text = "a<omega ><in>\n<value fixed=\"no\">1</value>\n<value fixed=\"no\">2</value>\n<value fixed=\"no\">3</value></in></omega>b";
//        System.out.println(getOmegaValues(text));
        
//        String text = "ETA(1)\nETA(2)\nETA(3)";
//        System.out.println(replaceEtaByEps(text));
        
//        String text = "THETA(1)\nTHETA(2)\nTHETA(3)";
//        System.out.println(addEtaToTheta(text));
        
//        String input = "ETA(4)*ETA(2) +  (ETA(5)\nETA(3) /BETA(1)\n";
//        System.out.println(find(input, "ETA"));
        
/*        
        String[] colList = {"11", "21", "22"};
        String list = colList[0];
        for(int i = 1; i < colList.length; i++)
            list += " " + colList[i];
        System.out.println(list);        
        String[] rowList = convertColToRowMajorOrder(colList);
        list = rowList[0];
        for(int i = 1; i < rowList.length; i++)
            list += " " + rowList[i];
        System.out.println(list);        
        colList = new String[]{"11", "21", "31", "22", "32", "33"};
        list = colList[0];
        for(int i = 1; i < colList.length; i++)
            list += " " + colList[i];
        System.out.println(list);        
        rowList = convertColToRowMajorOrder(colList);
        list = rowList[0];
        for(int i = 1; i < rowList.length; i++)
            list += " " + rowList[i];
        System.out.println(list);
        colList = new String[]{"11", "21", "31", "41", "22", "32", "42", "33", "43", "44"};
        list = colList[0];
        for(int i = 1; i < colList.length; i++)
            list += " " + colList[i];
        System.out.println(list);        
        rowList = convertColToRowMajorOrder(colList);
        list = rowList[0];
        for(int i = 1; i < rowList.length; i++)
            list += " " + rowList[i];
        System.out.println(list);
*/
    }
}
