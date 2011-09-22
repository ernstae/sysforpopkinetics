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

import uw.rfpk.mda.nonmem.Output;
import java.text.DecimalFormat;
import uw.rfpk.mda.nonmem.Utility;
import java.util.Date;
import java.util.HashMap;
import javax.swing.JOptionPane;
import java.text.SimpleDateFormat;
import java.text.NumberFormat;

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
                                     HashMap methodTable) 
    {
        if(output == null)
        {
            JOptionPane.showMessageDialog(null, "Output was not found.", "Input Erorr",
                                          JOptionPane.ERROR_MESSAGE);
            return null;   
        }
        // Preparation
        DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        f.applyPattern("0.00E00");
        
//        DecimalFormat f = new DecimalFormat("0.00E00");
//        NumberFormat p = NumberFormat.getPercentInstance();
//        p.setMaximumFractionDigits(1);
//        p.setMinimumFractionDigits(1);
        String par = "", ser = "", rse = "", sd = "", cv = "", n, lb = "", ub = ""; 
        String NA = "   N/A";
        
        if(output.objective == null)
            output.objective = NA;
        
        String theta = ""; 
        String omega = "";
        String sigma = "";
        int k = 0;
        // Write theta
        if(output.theta != null)
        {
            theta = "THETA\n";
            for(int i = 0; i < output.theta.length; i++)
            {
                n = String.valueOf(i + 1);        
                par = output.theta[i] != null && !output.theta[i].equals("nan") && !output.theta[i].endsWith("inf")  ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.theta[i]))) : NA;
                ser = output.stdErrTheta != null && output.stdErrTheta[i] != null && !output.stdErrTheta[i].equals("nan") && !output.stdErrTheta[i].endsWith("inf") ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.stdErrTheta[i]))) : NA;
                rse = output.coefVariation != null && output.coefVariation[i] != null && !output.coefVariation[i].equals("nan") && !output.coefVariation[i].endsWith("inf") ? 
                      Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[i]))) : NA;
                lb = output.confInterval != null && output.confInterval[0][i] != null && !output.confInterval[0][i].equals("nan") && !output.confInterval[0][i].endsWith("inf") ? 
                     Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][i]))) : NA;
                ub = output.confInterval != null && output.confInterval[1][i] != null && !output.confInterval[1][i].equals("nan") && !output.confInterval[1][i].endsWith("inf") ? 
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
                omega += "OMEGA\n";
                if(output.omegaStruct[l].equals("block"))
                {
                    for(int j = 1; j < output.omega[l].length + 1; j++)
                    {
                        for(int i = j - 1; i < output.omega[l].length; i++)  
                        {
                            par = output.omega[l][i][j] != null && !output.omega[l][i][j].equals("nan") && !output.omega[l][i][j].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.omega[l][i][j]))) : NA;
                            ser = output.stdErrOmega != null && output.stdErrOmega[l][i][j] != null && !output.stdErrOmega[l][i][j].equals("nan") && !output.stdErrOmega[l][i][j].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.stdErrOmega[l][i][j]))) : NA;
                            rse = output.coefVariation != null && output.coefVariation[k] != null && !output.coefVariation[k].equals("nan") && !output.coefVariation[k].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                            lb = output.confInterval != null && output.confInterval[0][k] != null && !output.confInterval[0][k].equals("nan") && !output.confInterval[0][k].endsWith("inf") ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                            ub = output.confInterval != null && output.confInterval[1][k] != null && !output.confInterval[1][k].equals("nan") && !output.confInterval[1][k].endsWith("inf") ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k]))) : NA;
                            cv = !par.equals(NA) && Double.parseDouble(par) >= 0 ? 
                                 Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(par)))) : NA;
                            omega = omega + "  " + j + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                    getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub;
                            if(j == i + 1)
                                omega += getSpace(14 - ub.length()) + cv;                    
                            omega += "\n";
                            k++;
                        }
                    }
                }

                if(output.omegaStruct[l].equals("diagonal"))
                {
                    for(int i = 0; i < output.omega[l].length; i++)
                    {
                        par = output.omega[l][i][i + 1] != null && !output.omega[l][i][i + 1].equals("nan") && !output.omega[l][i][i + 1].endsWith("inf") ?
                              Utility.formatData(6, f.format(Double.parseDouble(output.omega[l][i][i + 1]))) : NA;
                        ser = output.stdErrOmega != null && output.stdErrOmega[l][i][i + 1] != null && !output.stdErrOmega[l][i][i + 1].equals("nan")  && !output.stdErrOmega[l][i][i + 1].endsWith("inf") ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.stdErrOmega[l][i][i + 1]))) : NA;
                        rse = output.coefVariation != null && output.coefVariation[k] != null && !output.coefVariation[k].equals("nan") && !output.coefVariation[k].endsWith("inf") ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                        lb = output.confInterval != null && output.confInterval[0][k] != null && !output.confInterval[0][k].equals("nan") && !output.confInterval[0][k].endsWith("inf") ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                        ub = output.confInterval != null && output.confInterval[1][k] != null && !output.confInterval[1][k].equals("nan") && !output.confInterval[1][k].endsWith("inf") ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k]))) : NA;
                        cv = !par.equals(NA) && Double.parseDouble(par) >= 0 ? 
                             Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(par)))) : NA;
                        omega = omega + "  " + (i + 1) + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub +
                                getSpace(14 - ub.length()) + cv + "\n";
                        k++;
                    }
                }
            }
        }
        
        // Write sigma
        if(output.sigma != null && output.sigmaStruct != null)
        {
            for(int l = 0; l < output.sigma.length; l++)
            {
                sigma += "SIGMA\n";                
                if(output.sigmaStruct[l].equals("block"))
                {            
                    for(int j = 1; j < output.sigma[l].length + 1; j++)
                    {
                        for(int i = j - 1; i < output.sigma[l].length; i++)  
                        {                        
                            par = output.sigma[l][i][j] != null && !output.sigma[l][i][j].equals("nan") && !output.sigma[l][i][j].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.sigma[l][i][j]))) : NA;
                            ser = output.stdErrSigma != null && output.stdErrSigma[l][i][j] != null && !output.stdErrSigma[l][i][j].equals("nan") && !output.stdErrSigma[l][i][j].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.stdErrSigma[l][i][j]))) : NA;   
                            rse = output.coefVariation != null && output.coefVariation[k] != null && !output.coefVariation[k].equals("nan") && !output.coefVariation[k].endsWith("inf") ? 
                                  Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                            lb = output.confInterval != null && output.confInterval[0][k] != null && !output.confInterval[0][k].equals("nan") && !output.confInterval[0][k].endsWith("inf") ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                            ub = output.confInterval != null && output.confInterval[1][k] != null && !output.confInterval[1][k].equals("nan") && !output.confInterval[1][k].endsWith("inf") ? 
                                 Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k]))) : NA;
                            sd = !par.equals(NA) && Double.parseDouble(par) >= 0 ? 
                                 Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(par)))) : NA;
                            sigma = sigma + "  " + j + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +               
                                    getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub;                   
                            if(j == i + 1)
                                sigma += getSpace(14 - ub.length()) + sd;
                            sigma += "\n";
                            k++;
                        }
                    }
                }
 
                if(output.sigmaStruct[l].equals("diagonal"))
                {
                    for(int i = 0; i < output.sigma[l].length; i++)
                    {
                        par = output.sigma[l][i][i + 1] != null && !output.sigma[l][i][i + 1].equals("nan") && !output.sigma[l][i][i + 1].endsWith("inf") ?
                              Utility.formatData(6, f.format(Double.parseDouble(output.sigma[l][i][i + 1]))) : NA;
                        ser = output.stdErrSigma != null && output.stdErrSigma[l][i][i + 1] != null && !output.stdErrSigma[l][i][i + 1].equals("nan") && !output.stdErrSigma[l][i][i + 1].endsWith("inf") ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.stdErrSigma[l][i][i + 1]))) : NA;
                        rse = output.coefVariation != null && output.coefVariation[k] != null && !output.coefVariation[k].equals("nan") && !output.coefVariation[k].endsWith("inf") ? 
                              Utility.formatData(6, f.format(Double.parseDouble(output.coefVariation[k]))) : NA;
                        lb = output.confInterval != null && output.confInterval[0][k] != null && !output.confInterval[0][k].equals("nan") && !output.confInterval[0][k].endsWith("inf") ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[0][k]))) : NA;
                        ub = output.confInterval != null && output.confInterval[1][k] != null && !output.confInterval[1][k].equals("nan") && !output.confInterval[1][k].endsWith("inf") ? 
                             Utility.formatData(6, f.format(Double.parseDouble(output.confInterval[1][k]))) : NA;
                        cv = !par.equals(NA) && Double.parseDouble(par) >= 0 ? 
                             Utility.formatData(6, f.format(Math.sqrt(Double.parseDouble(par)))) : NA;
                        sigma = sigma + "  " + (i + 1) + "," + (i + 1) + "     " + par + getSpace(12 - par.length()) + ser +                            
                                getSpace(13 - ser.length()) + rse + getSpace(14 - rse.length()) + lb + getSpace(12 - lb.length()) + ub +
                                getSpace(14 - ub.length()) + cv + "\n";
                        k++;
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
            computingTime = String.valueOf(elapsedTime) + " s";
        }
        
        String jobId = output.jobId != null ? output.jobId : NA;
        String jobAbstract = output.jobAbstract != null ? output.jobAbstract : NA;
        String submissionTime = output.submissionTime != null ? formatTime(output.submissionTime) : NA;
        String completionTime = output.completionTime != null ? formatTime(output.completionTime) : NA;        
        String analysis = output.analysis != null ? output.analysis : NA;
        String jobMethod = NA + " due to offline (Method Code: " + output.methodCode + ")";
        if(isOnline)
        {
            jobMethod = ((String[])methodTable.get(output.methodCode))[0];
        }           
        String modelName = output.modelName != null ? output.modelName : NA;
        String modelVersion = output.modelVersion != null ? output.modelVersion : NA;
        String modelAbstract = output.modelAbstract != null ? output.modelAbstract : NA;
        String modelVersionLog = output.modelVersionLog != null ? output.modelVersionLog : NA;
        String dataName = output.dataName != null ? output.dataName : NA;
        String dataVersion = output.dataVersion != null ? output.dataVersion : NA;
        String dataAbstract = output.dataAbstract != null ? output.dataAbstract : NA;
        String dataVersionLog = output.dataVersionLog != null ? output.dataVersionLog : NA;
        String objective = output.objective != null ? output.objective : NA;
        String likelihood = output.likelihood != null ? output.likelihood[0][1] : NA;
        String objStdErr = output.likelihood_std != null ? output.likelihood_std[0][1] : NA;
        String nEvaluation = output.nEvaluation != null ? output.nEvaluation : NA;
        String objectiveItem = null;
        String objStdErrItem = "";
        String nEvaluationItem = "";
        if(output.objective != null)
        {
            if(output.nEvaluation != null)
            {
                objectiveItem = "\n\nEstimate for Likelihood Function: " + likelihood;
                objStdErrItem = "\nStandard Error in Likelihood Function: " + objStdErr;
                nEvaluationItem = "\nNumber of Evaluations: " + nEvaluation;
            }
            else
            {
                objectiveItem = "\n\nMinimum Value of Objective Function: " + objective;
            }
        }
        
        String identifyStatus = output.identifyStatus != null ? output.identifyStatus : NA;
        String identifySolutions = output.identifySolutions != null ? output.identifySolutions : NA;
        if(identifySolutions.trim().equals("0") || identifySolutions.trim().equals("1"))
            identifySolutions = "See the identifiability trace for details.";
        String nonparamNumOfPoints = output.nonparamNumOfPoints != null ? output.nonparamNumOfPoints : NA;
        String nonparamSeed = output.nonparamSeed != null ? output.nonparamSeed : NA;
        String nonparamPointsPerDim = output.nonparamPointsPerDim != null ? output.nonparamPointsPerDim : NA;
        String nonparamInPoints = output.nonparamInPoints != null ? output.nonparamInPoints : NA;
        String nonparamOutPoints = output.nonparamOutPoints != null ? output.nonparamOutPoints : NA;
        String errorMessage = "";
        if(output.error != null)
        {
            for(int i = 0; i < output.error.length; i++)
            {
                errorMessage += "\n<Error Message " + (i + 1) + ">\n" + output.error[i][0];
                if(isDeveloper)
                    errorMessage += "\nThis message was issued from file: " + output.error[i][1] + 
                                      " at line: " + output.error[i][2];
                if(i < output.error.length - 1) errorMessage += "\n";
            }
        }
        else
            errorMessage = "\nNone";
        errorMessage = errorMessage.replaceAll("\n", "\n     ");
        String warningMessage = "";
        if(output.warning != null)
        {
            for(int i = 0; i < output.warning.length; i++)
            {
                warningMessage += "\n<Warning Message " + (i + 1) + ">\n" + output.warning[i][0];
                if(isDeveloper)
                    warningMessage += "\nThis message was issued from file: " + output.warning[i][1] + 
                                      " at line: " + output.warning[i][2];
                if(i < output.warning.length - 1) warningMessage += "\n";
            }
        }
        else
            warningMessage = "\nNone";       
        warningMessage = warningMessage.replaceAll("\n", "\n     ");
        
        // Write summary
        StringBuffer summary = new StringBuffer("Summary Report");
        summary.append("\n\nJob Identification number: ").append(jobId)
               .append("\nJob Description: ").append(jobAbstract) 
               .append("\n\nTime of Job Submission: ").append(submissionTime) 
               .append("\nTime of Job Completion: ").append(completionTime)
               .append("\nSPK Computing Time: ").append(computingTime)
               .append("\nComputation Mode: ").append(output.computationMode)
               .append("\n\nAnalysis Type: ").append(analysis)
               .append("\n\nAnalysis Method: ").append(jobMethod)                      
               .append("\n\nModel Name: ").append(modelName).append("   Model Version: ").append(modelVersion)
               .append("\nModel Description: ").append(modelAbstract)
               .append("\nModel Version Log: ").append(modelVersionLog)
               .append("\n\nDataset Name: ").append(dataName).append("   Dataset Version: ").append(dataVersion)
               .append("\nDataset Description: ").append(dataAbstract)
               .append("\nDataset Version Log: ").append(dataVersionLog)
               .append("\n\nError Messages: ").append(errorMessage)
               .append("\n\nWarning Messages: ").append(warningMessage);
        if(output.subProblem != null)
            summary.append("\n\nSub-problem Number: ").append(output.subProblem);
        if(output.simulationSeed != null)
            summary.append("\n\nSeed for Simulation: ").append(output.simulationSeed);        
        if(!output.methodCode.equals("id"))
            summary.append(objectiveItem).append(objStdErrItem).append(nEvaluationItem);
        else
            summary.append("\n\nIdentifiability Status: ").append(identifyStatus)
                   .append("\nIdentifiability Number of Solutions: ").append(identifySolutions);
        if(output.methodCode.equals("un"))
            summary.append("\n\nNonparametric Number of Initial Points: ").append(nonparamInPoints)
                   .append("\nNonparametric Number of Output Points: ").append(nonparamOutPoints)
                   .append("\nNonparametric Seed: ").append(nonparamSeed);
        else if(output.methodCode.equals("gn"))
            summary.append("\n\nNonparametric Number of Initial Points: ").append(nonparamInPoints)
                   .append("\nNonparametric Number of Output Points: ").append(nonparamOutPoints);
        if(output.theta != null)
        {
            summary.append("\n\nParameter Estimation Result: ");
            String variability = "*This column indicates RUV in standard deviation units.";
            if(output.analysis.equals("population"))
                variability = "*For OMEGA, this column indicates BSV in standard deviation units.\n" +
                              " For SIGMA, this column indicates RUV in standard deviation units.";
            summary.append("\n")
                   .append("=====================================================================================\n\n")
                   .append("Parameter  Estimate  Std. Error  Coef. of Var.  95% Confidence Interval  Variability*\n")
                   .append("                                                  L-bound     U-bound\n")
                   .append(theta).append("\n").append(omega).append("\n");
            if(output.analysis.equals("population"))             
                summary.append(sigma).append("\n");
            summary.append(variability).append("\n\n")
                   .append("=====================================================================================");                    
        }

        return summary.toString();
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
