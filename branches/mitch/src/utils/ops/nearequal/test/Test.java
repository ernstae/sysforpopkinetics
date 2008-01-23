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

import uw.rfpk.nearequal.NearEqual;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

/** This is the test program for the NearEqual application.
 *
 * @author Jiaji Du
 */
public class Test {
    
    public static void main(String[] args)
    {
        String baseDir = "/home/jiaji/r2/trunk/src/utils/ops/nearequal/";
        String cp = baseDir + "classes:" + baseDir + "lib/MDA.jar";
        String oldResult = baseDir + "test/old_result.xml";
        String newResult = baseDir + "test/new_result.xml";
        String oldSource = baseDir + "test/old_source.xml";
        String rErr = "1e-3";
        String aErr = "1e-4";
        String className = "uw/rfpk/nearequal/NearEqual";
        String norm;
        String paramOnly = "0";
        String pdfm = "1";
        Process process = null;
        String string = null;
        
        String[] msg1 = {"Value of objective (obj) is different.",
                         "    Old value: -1     New value: -1.003",
                         "Value of THETA 2 is different.",
                         "    Old value: 1     New value: 0.997",
                         "Value of OMEGA block 1 is different.  index = 3",
                         "    Old value: 1     New value: 1.004",
                         "Value of SIGMA block 1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of standard error (stderror) is different.  index = 2",
                         "    Old value: 1     New value: 1.004",
                         "Covariance matrix is different.  index = 3",
                         "    Old value: 1     New value: 0.996",
                         "Inverse covariance matrix is different.  index = 1",
                         "    Old value: 1     New value: 0.003",
                         "Correlation matrix is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Coefficient of variation is different.  index = 1",
                         "    Old value: 1     New value: 1.004",
                         "Confidence interval lower bound is different.  index = 1",
                         "    Old value: 1     New value: 1.004",
                         "Confidence interval upper bound is different.  index = 1",
                         "    Old value: 1     New value: 1.004",
                         "Value of presentation data at row 1, column 3, DV, is different.",
                         "    Old value: Infinity     New value: -Infinity",
                         "Value of presentation data at row 2, column 3, DV, is different.",
                         "    Old value: 3.0     New value: 3.009",
                         "Value of alpha center for THETA 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha center for OMEGA block1 is different.  index = 1",
                         "    Old value: 1     New value: 0.996",
                         "Value of alpha center for SIGMA block1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha step for OMEGA block2 is different.  index = 1",
                         "    Old value: 1     New value: 0.997",
                         "Value of likelihood lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood upper 3 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error upper 3 is different.",
                         "    Old value: 1     New value: 1.003"};
        String[] msg2 = {"Value of objective (obj) is different.",
                         "    Old value: -1     New value: -1.003",
                         "Value of THETA 2 is different.",
                         "    Old value: 1     New value: 0.997",
                         "Value of OMEGA block 2 is different.  index = 1",
                         "    Old value: 1     New value: 0.998",
                         "Value of SIGMA block 1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Inverse covariance matrix is different.  index = 1",
                         "    Old value: 1     New value: 0.003",
                         "Value of presentation data at row 1, column 3, DV, is different.",
                         "    Old value: Infinity     New value: -Infinity",
                         "Value of presentation data at row 2, column 3, DV, is different.",
                         "    Old value: 3.0     New value: 3.009",
                         "Value of alpha center for THETA 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha center for OMEGA block1 is different.  index = 1",
                         "    Old value: 1     New value: 0.996",
                         "Value of alpha center for SIGMA block1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha step for OMEGA block2 is different.  index = 1",
                         "    Old value: 1     New value: 0.997",
                         "Value of likelihood lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood upper 3 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error upper 3 is different.",
                         "    Old value: 1     New value: 1.003"};
        String[] msg3 = {"Value of objective (obj) is different.",
                         "    Old value: -1     New value: -1.003",
                         "Value of THETA 2 is different.",
                         "    Old value: 1     New value: 0.997",
                         "Value of OMEGA block 1 is different.  index = 3",
                         "    Old value: 1     New value: 1.004",
                         "Value of SIGMA block 1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Covariance matrix is different.  index = 3",
                         "    Old value: 1     New value: 0.996",
                         "Inverse covariance matrix is different.  index = 1",
                         "    Old value: 1     New value: 0.003",
                         "Correlation matrix is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of presentation data at row 1, column 3, DV, is different.",
                         "    Old value: Infinity     New value: -Infinity",
                         "Value of presentation data at row 2, column 3, DV, is different.",
                         "    Old value: 3.0     New value: 3.009",
                         "Value of alpha center for THETA 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha center for OMEGA block1 is different.  index = 1",
                         "    Old value: 1     New value: 0.996",
                         "Value of alpha center for SIGMA block1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha step for OMEGA block2 is different.  index = 1",
                         "    Old value: 1     New value: 0.997",
                         "Value of likelihood lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood upper 3 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error upper 3 is different.",
                         "    Old value: 1     New value: 1.003"};
        String[] msg4 = {"Value of objective (obj) is different.",
                         "    Old value: -1     New value: -1.003",
                         "Value of THETA 2 is different.",
                         "    Old value: 1     New value: 0.997",
                         "Value of SIGMA block 1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Inverse covariance matrix is different.  index = 1",
                         "    Old value: 1     New value: 0.003",
                         "Value of presentation data at row 1, column 3, DV, is different.",
                         "    Old value: Infinity     New value: -Infinity",
                         "Value of presentation data at row 2, column 3, DV, is different.",
                         "    Old value: 3.0     New value: 3.009",
                         "Value of alpha center for THETA 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha center for OMEGA block1 is different.  index = 1",
                         "    Old value: 1     New value: 0.996",
                         "Value of alpha center for SIGMA block1 is different.  index = 1",
                         "    Old value: 1     New value: 1.003",
                         "Value of alpha step for OMEGA block2 is different.  index = 1",
                         "    Old value: 1     New value: 0.997",
                         "Value of likelihood lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood upper 3 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error lower 1 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error center 2 is different.",
                         "    Old value: 1     New value: 1.003",
                         "Value of likelihood standard error upper 3 is different.",
                         "    Old value: 1     New value: 1.003"};
        
        try
        {
            for(int i = 1; i <= 4; i++)
            {
                int j = 0;
                boolean ok =true;
                norm = String.valueOf(i);
                String[] command = {"java", "-cp", cp, className, oldResult, newResult, oldSource, rErr, aErr, norm, paramOnly, pdfm};
                process = Runtime.getRuntime().exec(command);
                BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
                while ((string = stdInput.readLine()) != null) {
                    if(string.length() != 0)
                    {
                        switch(i)
                        {
                            case 1:
                                System.out.println("From test norm = " + norm + ":  " + msg1[j]);
                                if(j >= msg1.length || !string.equals(msg1[j++]))
                                {
                                    ok = false;
                                    System.out.println("Test norm 1 failed: " + string);
                                }
                                break;
                            case 2:
                                System.out.println("From test norm = " + norm + ":  " + msg2[j]);
                                if(j >= msg2.length || !string.equals(msg2[j++]))
                                {
                                    ok = false;
                                    System.out.println("Test norm 2 failed: " + string);
                                }
                                break;
                            case 3:
                                System.out.println("From test norm = " + norm + ":  " + msg3[j]);
                                if(j >= msg3.length || !string.equals(msg3[j++]))
                                {
                                    ok = false;
                                    System.out.println("Test norm 3 failed: " + string);
                                }
                                break;
                            case 4:
                                System.out.println("From test norm = " + norm + ":  " + msg4[j]);
                                if(j >= msg4.length || !string.equals(msg4[j++]))
                                {
                                    ok = false;
                                    System.out.println("Test norm 4 failed: " + string);
                                }
                        }
                    }
                }
                if(ok)
                    System.out.println("Test norm " + i + " is OK.");
            }
        }
        catch(IOException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
        finally
        {
            process.destroy();
        }
    }
}
