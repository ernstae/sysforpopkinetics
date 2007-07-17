import java.io.*;
import java.util.*;

public class NonmemParser
{
    public Control getControl()
    {
        return control;
    }

    public Vector getData()
    {
        return data;
    }

    public String parseControl(String file)
    { 
        try
	{
            // Read in the control file
            BufferedReader in = new BufferedReader(new FileReader(file));
            String text = "";
            while(true)
            {
                int i = in.read();
                if(i != -1)
                    text += (char)i;
                else
                    break;
            }
            in.close();

            // Take out comments
            text = eliminateComments(text);

            // Parse the content
            StringTokenizer tokenRecord = new StringTokenizer(text, "$");
            while(tokenRecord.hasMoreTokens())
            {
                record = tokenRecord.nextToken(); 
                tokenOption = new StringTokenizer(record, " ");              
                recordName = tokenOption.nextToken();
                if(record.startsWith("PRO")) 
	        {
                    message = parsePRO();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(record.startsWith("INP"))
		{
                    message = parseINP();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("DAT")|| recordName.startsWith("INF"))
		{
                    message = parseDAT();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("SUB"))
		{
                    message = parseSUB();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("MOD")) 
		{                    
                    message = parseMOD();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.equals("PK")) 
		{                    
                    message = parsePK();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("THE"))
		{                    
                    message = parseTHE();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("OME")) 
		{                    
                    message = parseOME();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("ERR"))
		{                    
                    message = parseERR();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("SIG"))
		{                    
                    message = parseSIG();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("EST"))
		{                    
                    message = parseEST();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("COV"))
		{                    
                    message = parseCOV();
                    if(message.startsWith("Error"))
                        return message;
		}
                if(recordName.startsWith("TAB"))
		{                    
                    message = parseTAB();
                    if(message.startsWith("Error"))
                        return message;
		}
	        if(recordName.startsWith("SCA")) 
		{                    
                    message = parseSCA();
                    if(message.startsWith("Error"))
                        return message;
		}           
            }
        }
        catch(IOException ioe )
        {
        }
        return "Parsing control file done";
    }

    public String parseData()
    {
        Vector indData = new Vector();
        try
	{
            // Read in the control file
            BufferedReader in = new BufferedReader(new FileReader(control.DATA));
            // Read the first line
            String firstLine = in.readLine();
            if(firstLine == null) 
                return "Error in data file";
            
            // Tokenize the first line and put them in a String[]
            StringTokenizer lineToken = new StringTokenizer(firstLine, " ,\t", false);
            int nTokens = lineToken.countTokens();
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
                        return "Error in data file";
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
        }
        return "Parsing datafile done";
    }

    // Parse PRO
    private String parsePRO()
    {
        control.PROB = record.substring(recordName.length() + 2);
        return "";
    }

    // Parse INP
    private String parseINP()
    {
        int nTokens = tokenOption.countTokens();
        for(int i = 0; i < nTokens; i++)
            control.INPUT.add(tokenOption.nextToken().toLowerCase());
        return "";
    } 

    // Parse DAT
    private String parseDAT()
    {
        if(tokenOption.hasMoreTokens())
	{
	    control.DATA = tokenOption.nextToken();
	}
        else
	{
            return "Error in $DATA";  
        }  
        return "";    
    }                  
	
    // Parse SUB
    private String parseSUB()
    {
        StringTokenizer tokenSUB = new StringTokenizer(record, " ,=");
        tokenSUB.nextToken();
        String subName = null;
        String tolerance = null;
        int size = 2;
        while(tokenSUB.hasMoreTokens())
	{
            String token = tokenSUB.nextToken();
            if(token.startsWith("ADV"))
	    subName = token.toLowerCase();
	    if(token.startsWith("TOL"))
            if(tokenSUB.hasMoreTokens())
                tolerance = tokenSUB.nextToken();
            else
                return "Error in $SUB";
	}
            if(subName == null)
		return "Error in $SUB";
            if(tolerance == null)
                size = 1; 
            control.SUBROUTINES = new String[size];
            control.SUBROUTINES[0] = subName;
            if(size == 2)
                control.SUBROUTINES[1] = tolerance; 
        return "";
    }

    // Parse MOD
    private String parseMOD()
    {
        while(tokenOption.hasMoreTokens())
	{
	    String token = tokenOption.nextToken();
	    control.MODEL = control.MODEL.concat(token);
	    control.MODEL = control.MODEL.concat("\n");
        }
        return "";
    }

    private String eliminateComments(String text)
    {
        StringBuffer buffer = new StringBuffer();
        boolean b = true;
        for(int i = 0; i < text.length(); i++)
	{
            char ch = text.charAt(i);
             
            if(ch == ';')
	    {
                b = false;
	    }
	    if(ch == '\n')
	    {
	        b = true;
	    }
            if(b && ch != '\n') 
	    {
		buffer.append(ch);
            } 
        }
        return buffer.toString();
    }

    // Parse PK
    private String parsePK()
    {
        control.PK = "";
        while(tokenOption.hasMoreTokens())
	{   
	    String token = tokenOption.nextToken();
	    control.PK = control.PK.concat("\n");
	    control.PK = control.PK.concat(token);
	}
        return "";
    }

    // Parse THE
    private String parseTHE()
    {
        while(tokenOption.hasMoreTokens())
	{
            String token = tokenOption.nextToken();
	    String values = token.substring(1, token.length() - 1);
            StringTokenizer tokenValue = new StringTokenizer(values, ", ");                        
            int nTokens = tokenValue.countTokens();
            if(nTokens == 0)
	    {
                return "Error in $THETA";
	    }
            String[] theta = new String[4];
            String[] tokens = new String[nTokens];
            for(int i = 0; i < nTokens; i++)
                tokens[i] = tokenValue.nextToken();
            if(nTokens == 1)
	    {
                theta[0] = "";
                theta[1] = tokens[0];
                theta[2] = "";
                theta[3] = "no";
	    }
            if(nTokens == 2)
	    {
                if(tokens[1].equals("FIXED"))
                {
                    theta[0] = "";
                    theta[1] = tokens[0];
                    theta[2] = "";
                    theta[3] = "yes";
		}
                else
		{
                    theta[0] = tokens[0];
                    theta[1] = "";
                    theta[2] = tokens[1];
                    theta[3] = "no";
		}
	    }
            if(nTokens == 3)
            {
                theta[0] = tokens[0];
                theta[1] = tokens[1];
                theta[2] = tokens[2];
                theta[3] = "no";
            }
            if(nTokens == 4)
	    {
                if(tokens[3].equals("FIXED") &&  tokens[0].equals(tokens[1]) 
		   &&  tokens[2].equals(tokens[1]))
                {
                    theta[0] = tokens[0];
                    theta[1] = tokens[1];
                    theta[2] = tokens[2];
                    theta[3] = "yes";
		}
		else
		{
                    return "Error in $THETA";
		}
	    }			    
            control.THETA.add(theta);
        }
        return "";
    }

    // Parse OME
    private String parseOME()
    {
        String message = processBlocks(control.OMEGA, tokenOption);
        if(message.equals("Error"))
	    return message.concat(" in $OMEGA");
        return "";
    }

    // Parse ERR
    private String parseERR()
    {
	control.ERROR = "";
        while(tokenOption.hasMoreTokens())
	{   
	    String token = tokenOption.nextToken();
	    control.ERROR = control.ERROR.concat("\n");
	    control.ERROR = control.ERROR.concat(token);
        }
        return "";
    }

    // Parse SIG
    private String parseSIG()
    {
        String message = processBlocks(control.SIGMA, tokenOption);
        if(message.equals("Error"))
	    return message.concat(" in $SIGMA");
        return "";
    }

    // Parse EST
    private String parseEST()
    {
        String[] tokens = new String[tokenOption.countTokens()];
        for(int i = 0; i < tokens.length; i++)
            tokens[i] = tokenOption.nextToken();

        if(exist(tokens, "METHOD=0") || exist(tokens, "METHOD=ZERO") ||
           (!exist(tokens, "METHOD=0") && !exist(tokens, "METHOD=ZERO")))
	{
	    control.EST[0] = "fo";
        }
        else if(exist(tokens, "METHOD=1") || exist(tokens, "METHOD=CONDITIONAL"))
        {
            if(!exist(tokens, "LAPLACIAN"))
	    {
	        control.EST[0] = "foce";
            }
            else
                control.EST[0] = "laplace";
            }
        else
	{  
            return "Error in $ESTIMATION";
        }
                        
        if(record.indexOf("SIGDIGITS") == -1 && record.indexOf("NSIGDIGITS") == -1)
        {
            control.EST[1] = "0.001"; 
	}
	else
	{
            String value =  findValue(tokens, "SIGDIGITS");
            if(value != null)
	    {
                int power = Integer.parseInt(value);
                StringBuffer stringBuffer = new StringBuffer();
                stringBuffer.append("0.");
                for( int i = 0; i < power -1; i++)
                    stringBuffer.append('0');
                stringBuffer.append('1');
                control.EST[1] = String.valueOf(stringBuffer.toString());
            }
            else
	    {
                return "Error in $ESTIMATION";
	    }
        }

        control.EST[4] = "no";
        if(record.indexOf("MAXEVAL") == -1 && record.indexOf("MAXEVALS") == -1)
        {
        control.EST[2] = "TBD"; 
        }
        else
        {
            if(exist(tokens, "MAXEVAL=0"))
	    {
                control.EST[2] = "0";
	    }            
            if(record.indexOf("MAXEVALS") != -1)
	    {
                if(exist(tokens, "MAXEVALS=-1"))
	        control.EST[4] = "yes";
	        else
	        {
                    String value =  findValue(tokens, "MAXEVALS");
                    if(value != null)
		    {
                        control.EST[2] = value;
		    }
                    else
		    {
                        return "Error in $ESTIMATION";
		    }
	        }
	    }
            else if(record.indexOf("MAXEVAL") != -1)
	    {
                String value =  findValue(tokens, "MAXEVAL");
                if(value != null)
	        {
                    control.EST[2] = value;
	        }
                else
	        {
                    return "Error in $ESTIMATION";
	        }
	    }
            else
            {
                return "Error in $ESTIMATION";
	    }
        }

        if(record.indexOf("PRINT") == -1 || record.indexOf("PRINT=999") != -1
           || record.indexOf("PRINT=0") != -1)
        {
            control.EST[3] = "0";
        }
        else
        {
            String value = findValue(tokens, "PRINT");
            if(value != null && Integer.parseInt(value) >= 0)
	    {
                control.EST[3] = "1";
	    }
            else
	    {
                return "Error in $ESTIMATION";
	    }
        }

        if(exist(tokens, "POSTHOC") && !exist(tokens, "NOPOSTHOC"))
        {
            control.EST[5] = "yes";
        }
        else if(exist(tokens, "NOPOSTHOC") && !exist(tokens, "POSTHOC"))
        {
            if(!control.EST[0].equals("fo"))
            {  
                return "Error in $ESTIMATION";
            }
            control.EST[5] = "no";
        }
        else if(!exist(tokens, "NOPOSTHOC") && !exist(tokens, "POSTHOC"))
        {
            control.EST[5] = "no";
        }
        else
        {  
            return "Error in $ESTIMATION";
        }
        return "";
    }

    // Parse COV
    private String parseCOV()
    {
        String[] tokens = new String[tokenOption.countTokens()];
        for(int i = 0; i < tokens.length; i++)
            tokens[i] = tokenOption.nextToken();

        if(record.indexOf("MATRIX") == -1)
	{
            control.COV = "RSR";
        }
        else
	{
            if(exist(tokens, "MATRIX=R"))
                control.COV = "R";
        	else if(exist(tokens, "MATRIX=S"))
                    control.COV = "S";
	        else
                    return "Error in $COV";   
        }
        return "";
    }

    // ParseTAB
    private String parseTAB()
    { 
        // Create a String array to hold tokens of the record
        String[] tokens = new String[tokenOption.countTokens()];

        // Initialize index of token BY to -1
        int indexBy = -1;

        // Initialize parameters
        String filename = null;
        String header = "every";

        // Find options
        int length = tokens.length; 
        for(int i = 0; i < length; i++)
	{
            tokens[i] = tokenOption.nextToken();

            // Locate BY
            if(tokens[i].equals("BY"))
                indexBy = i;
     
            // Find FILE
            if(tokens[i].startsWith("FIL"))
	    {
                filename = tokenOption.nextToken(); 
                length--;
	    }
 
            // Find NOH
            if(tokens[i].startsWith("NOH"))
                header = "none"; 
			
            // Find ONE
            if(tokens[i].startsWith("ONE"))
                header = "one"; 
        }
         
        // Create a String[] that contains options except [list1][By list2]          
        String[] options = {"PRI", "NOP", "FIL", "NOH", "ONE", 
                            "UNC", "CON", "OMI", "BY"};

        // Generate list1
        int j = 0;

        // Locate list1
        while(j < tokens.length && exist(options, tokens[j]))
	    j++;

        // Put list1 in a Vector
        Vector list1 = new Vector();
        while(j < tokens.length && !exist(options, tokens[j]))
        {
            list1.add(tokens[j].toLowerCase());
	    j++;
        }

        // Add dv, pred, res, wres to list1
        list1.add("dv");
        list1.add("pred");
        list1.add("res");
        list1.add("wres");

        // Generate list2
	Vector list2 = null;
        if(indexBy != -1)
	{
            indexBy++;
    
            // Put list2 in a Vector
            list2 = new Vector();
            while(indexBy < tokens.length && !exist(options, tokens[indexBy]))
	    {
		list2.add(tokens[indexBy].toLowerCase());
                indexBy++;
            }
	}

	// Generate String[] table
        // The first element is filename and the second element is header
        // The following elements are for the list1
        // Each such element contains label=order
        int list1Size = list1.size();
        String[] table = new String[2 + list1Size];
        table[0] = filename;
        table[1] = header;
        for(int i = 0; i < list1Size; i++)
	{
            String label = (String)list1.get(i);
            if(list2 != null && list2.size() != 0)
	    {
		int order = locate(list2, label) + 1;
                if(order != 0)
	        {    
                    label = label.concat("=");
                    label = label.concat(String.valueOf(order));
		}
	    }
			
            table[i + 2] = label;
        }

        // Add the String[] table to Vector control.TABLE
        control.TABLE.add(table);
        return "";
    }

    // Parse SCA
    private String parseSCA()
    {		
        // Create a String array to hold tokens of the record
        String[] tokens = new String[tokenOption.countTokens()];

        // The following indexes are all inclusive
        int indexList1Start = -1;
        int indexList2Start = -1;
        int indexList3Start = -1;
        int indexList1End = -1;
        int indexList2End = -1;
        int indexList3End = -1;

        // Initialize parameters
        String unit_slope = "no";
        String y0_line = "no";
        String from = "1";
        String to = "900";

        // Find options
        int length = tokens.length;
        for(int i = 0; i < length; i++)
	{
            tokens[i] = tokenOption.nextToken();
     
            // Find UNIT
            if(tokens[i].startsWith("UNI"))
                unit_slope = "yes"; 
 
            // Find ORD0
            if(tokens[i].startsWith("ORD"))
                y0_line = "yes"; 

            // Find FROM, take next token as it value
            if(tokens[i].startsWith("FRO"))
	    {
                from = tokenOption.nextToken();
                length--;
            } 
	
            // Find TO, take next token as it value
            if(tokens[i].equals("TO"))
	    {
                to = tokenOption.nextToken();
                length--;
	    } 
        }
            
        // Create a String[] that contains options except list1 VS list2 [By list3]     
        String[] options = {"UNI", "ORD", "FRO", "TO", "UNC", "CON", "OMI"};
 
        // Locate list1 and list 2 in the String[] tokens
        int j = 0;

        // Find indexList1Start
        while(j < tokens.length && exist(options, tokens[j]))
	    j++;
        indexList1Start = j;

        // Find indexList2End
        while(j < tokens.length && !exist(options, tokens[j]) && !tokens[j].equals("BY"))
	    j++;
        indexList2End = j - 1;

        // Find indexList3Start if it exists
        if(tokens[j].equals("BY"))
            indexList3Start = j + 1;
          
        // Find indexList3End if it exists 
        while(j < tokens.length && !exist(options, tokens[j]))
	    j++;
        indexList3End = j - 1;

        // List1 and list2 each at least has one item
        if(indexList2End - indexList1Start < 1)
            return "Error in $SCATTERPLOT";

        // Find indexList1End and indexList2Start
        // If list1 and list2 each has one item and VS or * is omitted
        if(indexList2End - indexList1Start == 1)
	{
            indexList1End = indexList1Start;
            indexList2Start = indexList2End;
        }
 
        // Otherwise    
        else
	{
            // If list1 has no () and list2 has no ()
            if(!tokens[indexList1Start].startsWith("(") && !tokens[indexList2End].endsWith(")"))
            {
                j = indexList1Start + 1;
                while(j <= indexList2End && !tokens[j].equals("VS") && !tokens[j].equals("*"))
	            j++;
                if(j == indexList2End)
                    return "Error in $SCATTERPLOT";
                indexList1End = j - 1;
                indexList2Start = j + 1;
	    }
            // If list1 has ()
            else if(tokens[indexList1Start].startsWith("("))
	    {
                j = indexList1Start + 1;
                while(j <= indexList2End && !tokens[j].endsWith(")"))
	            j++;
                if(j == indexList2End)
                    return "Error in $SCATTERPLOT";
                indexList1End = j;
                indexList2Start = j + 1;
                if(tokens[indexList2Start].equals("VS") || tokens[indexList2Start].equals("*"))
                    indexList2Start++;

                // Remove ()
                tokens[indexList1Start] = tokens[indexList1Start].substring(1);
                tokens[indexList1End] = tokens[indexList1End].substring(0,tokens[indexList1End].length() - 1);  
		if(tokens[indexList2Start].startsWith("("))
                    tokens[indexList2Start] = tokens[indexList2Start].substring(1);
                if(tokens[indexList2Start].endsWith(")"))
                    tokens[indexList2End] = tokens[indexList2End].substring(0,tokens[indexList2End].length() - 1);
	    }
            // If list2 has () and list1 has no ()
            else
	    {
                j = indexList1Start + 1;
                while(j <= indexList2End && !tokens[j].startsWith("("))
	            j++;
                if(j == indexList1Start + 1)
                    return "Error in $SCATTERPLOT";
                indexList1End = j - 1;
                indexList2Start = j;
                if(tokens[indexList1End].equals("VS") || tokens[indexList1End].equals("*"))
                    indexList1End--;

                // Remove ()
                tokens[indexList2Start] = tokens[indexList2Start].substring(1);
                tokens[indexList2End] = tokens[indexList2End].substring(0,tokens[indexList2End].length() - 1);
	    }
	}
     
        // Generate String[][] scatterPlot
        String[][] scatterPlot;
        if(indexList3Start != -1)
            scatterPlot = new String[4][];
        else
            scatterPlot = new String[3][];

        // Put unit_slope, y0_line, from, to into scatterPlot  
        scatterPlot[0] = new String[4];
        scatterPlot[0][0] = unit_slope;
        scatterPlot[0][1] = y0_line;
        scatterPlot[0][2] = from;
        scatterPlot[0][3] = to;

        // Put list1 into scatterPlot
        scatterPlot[1] = new String[indexList1End - indexList1Start + 1];
        for(int i = indexList1Start; i <= indexList1End; i++)
            scatterPlot[1][i - indexList1Start] = tokens[i].toLowerCase();

        // Put list2 into scatterPlot
        scatterPlot[2] = new String[indexList2End - indexList2Start + 1];
        for(int i = indexList2Start; i <= indexList2End; i++)
            scatterPlot[2][i - indexList2Start] = tokens[i].toLowerCase();

        // Put list3 into scatterPlot
        if(indexList3Start != -1)
	{
            scatterPlot[3] = new String[indexList3End - indexList3Start + 1];
            for(int i = indexList3Start; i <= indexList3End; i++)
                scatterPlot[3][i - indexList3Start] = tokens[i].toLowerCase();
	}

        // Add the String[][] scatterPlot to Vector control.SCAT 
        control.SCAT.add(scatterPlot);
        return ""; 
    }
    
    private String eliminateSpacesAroundEqualSign(String text)
    {
        StringBuffer buffer = new StringBuffer();
        int nSpace = 0;
        boolean hasEqualSign = false;
        boolean isValid = true;
        boolean hasExtraSpace = false;
        for(int i = 0; i < text.length(); i++)
	{
            char ch = text.charAt(i);
             
            if(ch == ' ')
	    {
		isValid = !hasEqualSign;
                if(!hasEqualSign)
                    nSpace++;
	    }
	    if(ch == '=')
	    {
                isValid = true;
	        hasEqualSign = true;
                if(nSpace > 0)
                    hasExtraSpace = true;
	    }
            if(ch != ' ' && ch != '=')
	    {
                nSpace = 0;
                hasEqualSign = false;
                isValid = true;
	    }
            if(hasExtraSpace)
 	    {
                buffer.delete(buffer.length() - nSpace, buffer.length());
                hasExtraSpace = false;
            }
            if(isValid) 
	    {
		buffer.append(ch);
            }
        }
	return buffer.toString();
    }

    private String processBlocks(List record, StringTokenizer tokenOption)
    {
        // Place all tokens in a String array
        String[] tokens = new String[tokenOption.countTokens()];
                
        for(int i = 0; i < tokens.length; i++)
            tokens[i] = tokenOption.nextToken();
                        
        List vector = new Vector();

        // Find '(' and ')' in the first token
        int left = tokens[0].indexOf('(');
        int right = tokens[0].indexOf(')');
        if(left > right)
            return "Error";
                       
        // Form 1
        if((left == 8 && right == tokens[0].length() - 1 && tokens[0].startsWith("DIAGONAL"))
           || (left == 4 && right == tokens[0].length() - 1 && (tokens[0].startsWith("DIAG"))))
	{
	    vector.add("diagonal"); 
            vector.add(tokens[0].substring(left + 1, right));
            setInitValues(vector, tokens, 1);
        }
	if( !tokens[0].startsWith("DIAG") && !tokens[0].startsWith("BLOCK"))
        {
            vector.add("diagonal"); 
            vector.add(String.valueOf(tokens.length));
            setInitValues(vector, tokens, 0);
        }
                  
	// Form 2
        if(left == 5 && right == tokens[0].length() - 1 && tokens[0].startsWith("BLOCK"))
        {
            vector.add("block"); 
            vector.add(tokens[0].substring(left + 1, right));
            setInitValues(vector, tokens, 1);                      
	}

	// Form 3
	if(tokens[0].startsWith("BLOCK") && tokens[1].equals("SAME"))     
        {               
            int size = record.size(); 
            if(size > 0)
                vector = (Vector)record.get(size - 1);
            else
                return "Error;";
        } 

        record.add(vector);
        return "Completed";                
    }

    private void setInitValues(List vector, String[] tokens, int start)
    { 
        boolean nextFixed = false;
        for( int i = start; i < tokens.length; i++) 
        {
            if(nextFixed)
            {
                String token = tokens[i].substring(0, tokens[i].length() - 1);
                vector.add("F".concat(token));
                nextFixed = false;
            }
            else
            {   
                vector.add(tokens[i]);
            }
            if(tokens[i].equals("FIXED"))
	    {
                vector.remove(vector.size() - 1);
                vector.remove(vector.size() - 1);
		vector.add("F".concat(tokens[i - 1]));
            }
	    if(tokens[i].equals("FIXED)"))
	    {
                vector.remove(vector.size() - 1);
                vector.remove(vector.size() - 1);
                vector.add(tokens[i - 1].replace('(', 'F'));
            }
            if(tokens[i].equals("(FIXED"))
	    {
                vector.remove(vector.size() - 1);
                nextFixed = true;
            }
        }
    }

    private int locate(Vector vector, String string)
    {
        for(int i = 0; i < vector.size(); i++)
            if(string.startsWith((String)vector.get(i)))
                return i;
        return -1;
    }

    private boolean exist(String[] strings, String string)
    {
        for(int i = 0; i < strings.length; i++)
            if(string.startsWith(strings[i]))
                return true;
        return false;
    }

    private String findValue(String[] strings, String string)
    {
        for(int i = 0; i < strings.length; i++)
	{
            StringTokenizer tokenValue = new StringTokenizer(strings[i], "=");
            if(string.equals(tokenValue.nextToken()))
	    {
                if(tokenValue.hasMoreTokens())
                    return tokenValue.nextToken();
                else 
                    return null;
            }
        }  
        return null;
    }

    // Parsed control
    private Control control = new Control();

    // Parsed data
    private Vector data = new Vector();

    // Record
    private String record;

    // Record name
    private String recordName; 

    // Token option
    StringTokenizer tokenOption;

    // Message
    String message;
}
