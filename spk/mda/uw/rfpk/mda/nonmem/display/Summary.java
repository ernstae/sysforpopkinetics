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
package uw.rfpk.mda.nonmem.display;

import java.text.DecimalFormat;
import uw.rfpk.mda.nonmem.Utility;
import java.util.Date;
import java.util.HashMap;
import java.text.SimpleDateFormat;
//import java.text.NumberFormat;

/**
 * This class makes a summary report.
 * @author  Jiaji Du
 */
public class Summary {
    
    /** Generate a summary report form the output. 
     * @param output a Output object.
     * @param isOnline true if MDA is online, false otherwise.
     * @param isDeveloper true if the user is a developer, false otherwise.
     * @param jobMethodCode the job method code.
     * @param methodTable a HashMap object containing the method table.
     * @return a String object containing the text of the summary report generated. 
     */
    public static String makeSummary(Output output, boolean isOnline, boolean isDeveloper, 
                                     String jobMethodCode, HashMap methodTable) 
    {
        // Preparation
        DecimalFormat f = new DecimalFormat("0.00E00");
//        NumberFormat p = NumberFormat.getPercentInstance();
//        p.setMaximumFractionDigits(1);
//        p.setMinimumFractionDigits(1);
        String par = "", ser = "", rse = "", sd = "", cv = "", n, lb = "", ub = ""; 
        String NA = "N/A";
        
        if(output.objective == null)
            output.objective = NA;
        
        String theta = ""; 
        String omega = "";
        String sigma = "";
        int k = 0;
        // Write theta
        if(output.theta != null)
        {
            theta = "THETA" + "\n";
            for(int i = 0; i < output.theta.length; i++)
            {
                n = String.valueOf(i + 1);        
                par = output.theta[i] != null ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.theta[i]))) : NA;
                ser = output.stdErrTheta != null && output.stdErrTheta[i] != null ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.stdErrTheta[i]))) : NA;
                rse = output.coefVariation != null && output.coefVariation[i] != null ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[i]))) : NA;
                lb = output.confInterval != null && output.confInterval[0][i] != null ? 
                     Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][i]))) : NA;
                ub = output.confInterval != null && output.confInterval[1][i] != null ? 
                     Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][i]))) : NA;
                theta = theta + getSpace(5 - n.length()) + n + "     " + par + getSpace(12 - par.length()) + ser +
                        getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub + "\n";  
                k++;
            }
        }
        
        // Write omega
        if(output.omega != null && output.omegaStruct != null)
        {
            for(int l = 0; l < output.omega.length; l++)
            {
                omega += "OMEGA" + "\n";
                if(output.omegaStruct[l].equals("block"))
                {
                    for(int j = 1; j < output.omega[l].length + 1; j++)
                    {
                        for(int i = j - 1; i < output.omega[l].length; i++)  
                        {
                            par = output.omega[l][i][j] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.omega[l][i][j]))) : NA;
                            ser = output.stdErrOmega != null && output.stdErrOmega[l][i][j] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.stdErrOmega[l][i][j]))) : NA;
                            rse = output.coefVariation != null && output.coefVariation[k] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                            lb = output.confInterval != null && output.confInterval[0][k] != null ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                            ub = output.confInterval != null && output.confInterval[1][k] != null ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k++]))) : NA;
                            cv = output.omega[l][i][j] != null && Double.parseDouble(output.omega[l][i][j]) >= 0 ? 
                                 Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(output.omega[l][i][j])))) : NA;
                            omega = omega + "  " + j + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                    getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub;
                            if(j == i + 1)
                                omega += getSpace(14 - ub.length()) + cv;                    
                            omega += "\n";                     
                        }
                    }
                }

                if(output.omegaStruct[l].equals("diagonal"))
                {
                    for(int i = 0; i < output.omega[l].length; i++)
                    {
                        par = output.omega[l][i][i + 1] != null ?
                              Utility.formatData(6, f.format(Double.parseDouble(output.omega[l][i][i + 1]))) : NA;
                        ser = output.stdErrOmega != null && output.stdErrOmega[l][i][i + 1] != null ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.stdErrOmega[l][i][i + 1]))) : NA;
                        rse = output.coefVariation != null && output.coefVariation[k] != null ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                        lb = output.confInterval != null && output.confInterval[0][k] != null ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                        ub = output.confInterval != null && output.confInterval[1][k] != null ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k++]))) : NA;
                        cv = output.omega[l][i][i + 1] != null && Double.parseDouble(output.omega[l][i][i + 1]) >= 0 ? 
                             Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(output.omega[l][i][i + 1])))) : NA;
                        omega = omega + "  " + (i + 1) + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub +
                                getSpace(14 - ub.length()) + cv + "\n";
                    }
                }
            }
        }
        
        // Write sigma
        if(output.sigma != null && output.sigmaStruct != null)
        {
            for(int l = 0; l < output.sigma.length; l++)
            {
                sigma += "SIGMA" + "\n";                
                if(output.sigmaStruct[l].equals("block"))
                {            
                    for(int j = 1; j < output.sigma[l].length + 1; j++)
                    {
                        for(int i = j - 1; i < output.sigma[l].length; i++)  
                        {                        
                            par = output.sigma[l][i][j] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.sigma[l][i][j]))) : NA;
                            ser = output.stdErrSigma != null && output.stdErrSigma[l][i][j] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.stdErrSigma[l][i][j]))) : NA;   
                            rse = output.coefVariation != null && output.coefVariation[k] != null ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                            lb = output.confInterval != null && output.confInterval[0][k] != null ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                            ub = output.confInterval != null && output.confInterval[1][k] != null ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k++]))) : NA;
                            sd = output.sigma[l][i][j] != null && Double.parseDouble(output.sigma[l][i][j]) >= 0 ? 
                                 Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(output.sigma[l][i][j])))) : NA;
                            sigma = sigma + "  " + j + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +               
                                    getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub;                   
                            if(j == i + 1)
                                sigma += getSpace(14 - ub.length()) + sd;
                            sigma += "\n";                    
                        }
                    }
                }
 
                if(output.sigmaStruct[l].equals("diagonal"))
                {        
                    for(int i = 0; i < output.sigma[l].length; i++)
                    {
                        par = output.sigma[l][i][i + 1] != null ?
                              Utility.formatData(6, f.format(Double.parseDouble(output.sigma[l][i][i + 1]))) : NA;
                        ser = output.stdErrSigma != null && output.stdErrSigma[l][i][i + 1] != null ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.stdErrSigma[l][i][i + 1]))) : NA;
                        rse = output.coefVariation != null && output.coefVariation[k] != null ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                        lb = output.confInterval != null && output.confInterval[0][k] != null ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                        ub = output.confInterval != null && output.confInterval[1][k] != null ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k++]))) : NA;
                        cv = output.sigma[l][i][i + 1] != null && Double.parseDouble(output.sigma[l][i][i + 1]) >= 0 ? 
                             Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(output.sigma[l][i][i + 1])))) : NA;
                        sigma = sigma + "  " + (i + 1) + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub +
                                getSpace(14 - ub.length()) + cv + "\n";
                    }
                }
            }
        }       
        
        // Galculate computing time
        String computingTime = NA;
        if(output.computingTimes != null && output.computingTimes.length > 0)
        {
            double elapsedTime = 0;
            for(int i = 0; i < output.computingTimes.length; i++)
            {
                if(output.computingTimes[i] != null && !output.computingTimes[i].equals(""))
                    elapsedTime += Double.parseDouble(output.computingTimes[i]);
            }
            if(elapsedTime != 0)
                computingTime = String.valueOf(elapsedTime) + " s";
        }
        
        String jobId = output.jobId != null ? output.jobId : NA;
        String jobAbstract = output.jobAbstract != null ? output.jobAbstract : NA;
        String submissionTime = output.submissionTime != null ? formatTime(output.submissionTime) : NA;
        String completionTime = output.completionTime != null ? formatTime(output.completionTime) : NA;        
        String analysis = output.analysis != null ? output.analysis : NA;
        String jobMethod = NA + " due to offline (Job Code: " + output.methodCode + ")"; 
        if(isOnline)
        {
            jobMethod = output.methodCode != null ? ((String[])methodTable.get(output.methodCode))[0] : NA;
        }           
        String modelName = output.modelName != null ? output.modelName : NA;
        String modelVersion = output.modelVersion != null ? output.modelVersion : NA;
        String modelAbstract = output.modelAbstract != null ? output.modelAbstract : NA;
        String dataName = output.dataName != null ? output.dataName : NA;
        String dataVersion = output.dataVersion != null ? output.dataVersion : NA;
        String dataAbstract = output.dataAbstract != null ? output.dataAbstract : NA;
        String objective = output.objective != null ? output.objective : NA;
        String objStdErr = output.objStdErr != null ? output.objStdErr : NA;
        String objectiveItem = null;
        String objStdErrItem = "";
        if(!((String[])methodTable.get(output.methodCode))[1].equals("le"))
            objectiveItem = "\n\nMinimum Value of Objective Function: " + objective;
        else
        {
            objectiveItem = "\n\nEstimate for Likelihood Function: " + objective;
            objStdErrItem = "\n\nStandard Error in Likelihood Function: " + objStdErr;
        }
        String errorMessage = "";
        if(output.error != null)
        {
            for(int i = 0; i < output.error.length; i++)
            {
                errorMessage += "\n<Error Message " + (i + 1) + ">\n" + output.error[i][0];
                if(isDeveloper)
                    errorMessage += "\nThis message was issued from file: " + output.error[i][1] + 
                                      " at line: " + output.error[i][2];
            }
        }
        else
            errorMessage = "\nNone";        
        String warningMessage = "";
        if(output.warning != null)
        {
            for(int i = 0; i < output.warning.length; i++)
            {
                warningMessage += "\n<Warning Message " + (i + 1) + ">\n" + output.warning[i][0];
                if(isDeveloper)
                    warningMessage += "\nThis message was issued from file: " + output.warning[i][1] + 
                                      " at line: " + output.warning[i][2];
            }
        }
        else
            warningMessage = "\nNone";
        
        // Write summary
        String summary = "Summary Report\n" +
                         "\n\nJob Identification number: " + jobId +
                         "\n\nJob Description: " + jobAbstract + 
                         "\n\nTime of Job Submisison: " + submissionTime + 
                         "\n\nTime of Job Completion: " + completionTime +
                         "\n\nSPK Computing Time: " + computingTime +
                         "\n\nAnalysis Type: " + analysis +
                         "\n\nAnalysis Method: " + jobMethod +                      
                         "\n\nModel Name: " + modelName +
                         "\n\nModel Version: " + modelVersion +
                         "\n\nModel Description: " + modelAbstract +
                         "\n\nDataset Name: " + dataName +
                         "\n\nDataset Version: " + dataVersion +
                         "\n\nDataset Description: " + dataAbstract +                    
                         "\n\nError Messages: " + errorMessage +
                         "\n\nWarning Messages: " + warningMessage +
                         objectiveItem + objStdErrItem +
                         "\n\nParameter Estimation Result: ";
        if(output.theta == null)
            summary += NA;
        else
        {
            String variability = "*It is residual variability.";
            if(output.analysis.equals("population"))
                variability = "*For OMEGA, it is interindividual variability. For SIGMA, it is residual variability.";
            summary += "\n" +
                         "=====================================================================================" + "\n\n" +
                         "Parameter  Estimate  Std. Error  Coef. of Var.  95% Confidence Interval  Variability*" + "\n" +
                         "                                                  L-bound     U-bound"                 + "\n" + 
                         theta + "\n" + omega + "\n";
            if(output.analysis.equals("population"))             
                summary += sigma + "\n";
            summary += variability + "\n\n" +
                       "=====================================================================================";                    
        }

        return summary;
    }
    
    // This function return spaces.
    private static String getSpace(int n)
    {
        String s = "";
        for(int i = 0; i < n; i++)
            s += " ";
        return s;  
    }
    
    // Format timestamp.
    private static String formatTime(String time)
    {
        Date date = new Date(Long.parseLong(time + "000"));
        SimpleDateFormat formatter = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
        return formatter.format(date); 
    }
    
}