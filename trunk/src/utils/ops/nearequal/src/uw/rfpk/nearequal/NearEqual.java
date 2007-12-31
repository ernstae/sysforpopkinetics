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
package uw.rfpk.nearequal;

import uw.rfpk.mda.nonmem.Output;
import uw.rfpk.mda.nonmem.XMLReader;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import javax.xml.parsers.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Attr;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

/**  This class defines static methods to check computational results in a new result.xml
 *   file against an old result.xml file in the sense of near-equal.  The MDA output 
 *   parser is used, and additional source.xml parsing is included in the class only for  
 *   likelihood evaluation jobs.  The following items are checked.<br>
 *<p>
 *   Objective:  It is a unit.<br>
 *   THETA:  Each element is a unit.<br>
 *   OMEGA:  The whole matrix of each bloak is a unit.<br> 
 *   SIGMA:  The whole matrix of each bloak is a unit.<br>
 *   Standard error:  The whole vector is a unit.<br>
 *   Covariance:  The whole matrix is a unit.<br>
 *   Inverse covariance:  The whole matrix is a unit.<br>
 *   Correlation:  The whole matrix is a unit.<br>
 *   Coefficient of variation:  The whole vector is a unit.<br>
 *   Confidence interval:  Each bound vector is a unit.<br>
 *   Presentation data:  Each element is a unit.
 *   Alpha:  Each element coresponding to THETA is a unit.  The group of elements corresponding to each OMEGA or SIGMA block is a unit.<br> 
 *   Likelihood:  Each element is a unit.<br>
 *   Likelihood standard error:  Each element is a unit.<br>
 *<p>
 *   A matrix is treated as a vector.  The vector consists of all the elements of the original matrix.<br>
 *   For matrix, relative error tolerance is increased by the precision degradation factor.<br>
 *   For inversed matrix, relative error tolerance is increased twice by the precision degradation factor.<br>
 *<p>
 *   Test passing conditions:<br>
 *   |X - Y| <= aErr,  or |X - Y| <= rErr * (|X| + |Y|)<br>
 *   where aErr is absolute error; rErr is relative error.<br>
 *   For matrix, relative error tolerance is increased by the precision degradation factor.<br>
 *   For inversed matrix, relative error tolerance is increased twice by the precision degradation factor.<br>
 *   For presentation data,  All weighted residuals are ignored.<br> 
 *   For non-weighted population level residuals, error tolerances are modified to aErr = rErr * (|DV| + |F|) and rErr = 0<br>
 *   For non-weighted individual level residuals, relative error tolerance is increased by the precision degradation factor.<br>
 *<p>
 *   Norm definitions:<br>
 *   1:  norm(X) = max|xi|<br>
 *   2:  norm(X) = sum|xi|<br>
 *   3;  norm(X) = sqrt(sum(xi*xi))<br>
 *<p>
 *   The user can choose which norm to use, and can choose passing the test if the test with any of the norms is used is passed.
 *<p>
 *   Neither nan, inf nor -inf is allowed except in the presentation data.  When they are allowed they must be the same in the old and the new results.
 *<p>
 *   This program reports in the standard out any diffrence found.  When vectors or matrixes are compared, the index of the maximum-difeerence element 
 *   is also reported.   The reported index starts from 1, and it corresponds to the original lower triangle matrix.
 *
 * @author Jiaji Du
 */
public class NearEqual {
    
    // Check old number u with new number v. nan, inf and -inf are all allowed,
    // but them must be the same in u and v.
    private static boolean checkNumber(double u, double v, double aTol, double rTol)
    {
        if(u != u)  // NaN
        {
            if(v == v) 
                return false;
        }
        else if(u == Double.POSITIVE_INFINITY)
        {
            if(v != Double.POSITIVE_INFINITY)
                return false;
        }
        else if(u == Double.NEGATIVE_INFINITY)
        {
            if(v != Double.NEGATIVE_INFINITY)
                return false;
        }
        else
        {
            if(v != v || v == Double.POSITIVE_INFINITY || v == Double.NEGATIVE_INFINITY)
                return false;
            else
            {
                double diff = Math.abs(u - v);
                if(diff > aTol && diff > rTol * (Math.abs(u) + Math.abs(v)))
                    return false;
            }
        }
        return true;
    }
    
    // Check old vector u with new vector v.  Either nan, inf or -inf is not allowed.
    private static boolean checkVector(String[] u, String[] v)
    {       
        for(int i = 0; i < v.length; i++)
            if(v.equals("nan") || v.equals("inf") || v.equals("-inf"))
                return false;
        
        switch(norm)
        {
            case 1:
                if(!norm1(u, v)) return false;
                break;
            case 2:
                if(!norm2(u, v)) return false;
                break;
            case 3:
                if(!norm3(u, v)) return false;
                break;
            case 4:
                if(!norm1(u, v) && !norm2(u, v) && !norm3(u, v)) return false;
                break;
            default:
                System.out.println("Norm is missing.");
                System.exit(1);
        }
        
        return true;
    }
    
    // norm(X) = max|xi| norm_inf
    private static boolean norm1(String[] u, String[] v)
    {
        double x = 0;
        double y = 0;
        double normX = 0;
        double normY = 0;
        double diff = 0;
        for(int i = 0; i < v.length; i++)
        {
            x = Double.parseDouble(u[i]);
            y = Double.parseDouble(v[i]);
            diff = Math.max(Math.abs(x - y), diff);
            normX = Math.max(Math.abs(x), normX);
            normY = Math.max(Math.abs(y), normY);
        }
        if(diff > aErr && diff > rErr * (normX + normY))
        {
            maxDiffIndex(u, v);
            return false;
        }
        return true; 
    }
    
    // norm(X) = sum|xi| norm_1
    private static boolean norm2(String[] u, String[] v)
    {
        double x = 0;
        double y = 0;
        double normX = 0;
        double normY = 0;
        double diff = 0;
        for(int i = 0; i < v.length; i++)
        {
            x = Double.parseDouble(u[i]);
            y = Double.parseDouble(v[i]);
            diff += Math.abs(x - y);
            normX += Math.abs(x);
            normY += Math.abs(y);
        }
        if(diff > aErr && diff > rErr * (normX + normY))
        {
            maxDiffIndex(u, v);
            return false;
        }
        return true; 
    }
    
    // norm(X) = sqrt(sum(xi*xi)) norm_2
    private static boolean norm3(String[] u, String[] v)
    {
        double x = 0;
        double y = 0;
        double normX = 0;
        double normY = 0;
        double diff = 0;
        for(int i = 0; i < v.length; i++)
        {
            x = Double.parseDouble(u[i]);
            y = Double.parseDouble(v[i]);
            diff += (x - y) * (x - y);
            normX += x * x;
            normY += y * y;
        }
        diff = Math.sqrt(diff);
        normX = Math.sqrt(normX);
        normY = Math.sqrt(normY);
        if(diff > aErr && diff > rErr * (normX + normY))
        {
            maxDiffIndex(u, v);
            return false;
        }
        return true; 
    }
    
    // Determine the index of the maximum difference. 
    private static void maxDiffIndex(String[] u, String[] v)
    {
        double[] diffs = new double[v.length];
        double x = 0;
        double y = 0;
        for(int i = 0; i < v.length; i++)
        {
            x = Double.parseDouble(u[i]);
            y = Double.parseDouble(v[i]);
            diffs[i] = Math.abs(x - y);
        }
        double diff = diffs[0];
        index = 1;
        for(int i = 1; i < v.length; i++)
        {
            if(diffs[i] > diff)
            {
                diff = diffs[i];
                index = i + 1;
            }
        }
        oldValue = u[index - 1];
        newValue = v[index - 1];
    }
    
    // Check matrix by converting lower triangle matrix into full matrix
    // contained in a row-major vector. then call checkVector method.
    // If checkVector method returns false, convert back the index.
    private static boolean checkMatrix(String[][] u, String[][] v)
    {
        double temp = rErr;
        rErr = rErr * pdfm;
        if(isInverseMatrix) rErr = rErr * pdfm;
        int l = v.length;
        String[] x = new String[l * l];
        String[] y = new String[l * l];
        int k = 0;
        for(int i = 0; i < l; i++)
            for(int j = 1; j <= l; j++)
            {
                if(j > i+ 1)
                {
                    x[k] = u[j - 1][i + 1];
                    y[k++] = v[j - 1][i + 1];
                }
                else
                {
                    x[k] = u[i][j];
                    y[k++] = v[i][j];
                }
            }
        if(!checkVector(x, y))
        {
            index--;
            int i = index / l;
            int j = index % l;
            if(i < j)
            {
                i = index % l;
                j = index / l;
            }
            index = (i + 1) * i / 2 + j + 1;
            rErr = temp;
            return false;
        }
        rErr = temp;
        return true;
    }
    
    // Compare old output with new output, and if difference is found put message in standard out.
    private static void compare(Output oldOutput, Output newOutput)
    {   
        // Check objective
        if(oldOutput.objective != null)
        {
            if(newOutput.objective != null)
            {
                if(!checkVector(new String[]{oldOutput.objective}, new String[]{newOutput.objective}))
                    msg.append("\nValue of objective (obj) is different." +
                               "\n    Old value: " + oldOutput.objective + "     New value: " + newOutput.objective);
            }
            else
                msg.append("\nObjective (obj) is missing.");
        }
        
        // Check theta
        if(oldOutput.theta != null)
        {
            if(newOutput.theta != null)
            {
                if(oldOutput.theta.length == newOutput.theta.length)
                {
                    for(int i = 0; i < oldOutput.theta.length; i++)
                    {
                        if(!checkVector(new String[]{oldOutput.theta[i]}, new String[]{newOutput.theta[i]}))
                            msg.append("\nValue of THETA " + (i + 1) + " is different." +
                                       "\n    Old value: " + oldOutput.theta[i] + "     New value: " + newOutput.theta[i]);
                    }
                }
                else
                    msg.append("\nLength of THETA is wrong." +
                               "\n    Old value: " + oldOutput.theta.length + "     New value: " + newOutput.theta.length);
            }
            else
                msg.append("\nTHETA is missing.");
        }
        
        // Check omega
        if(oldOutput.omega != null)
        {
            if(newOutput.omega != null)
            {
                if(oldOutput.omega.length == newOutput.omega.length)
                {
                    for(int i = 0; i < oldOutput.omega.length; i++)
                    {
                        if(oldOutput.omega[i].length == newOutput.omega[i].length)
                        {
                            if(!checkMatrix(oldOutput.omega[i], newOutput.omega[i]))
                                msg.append("\nValue of OMEGA block " + (i + 1) + " is different.  index = " + index + values());
                        }
                        else
                            msg.append("\nDimension of of OMEGA blocks " + (i + 1) + " is wrong." +
                                       "\n    Old value: " + oldOutput.omega[i].length + "     New value: " + newOutput.omega[i].length);
                    }
                }
                else
                    msg.append("\nTotal number of OMEGA blocks is wrong." +
                               "\n    Old value: " + oldOutput.omega.length + "     New value: " + newOutput.omega.length);
            }
            else
                msg.append("\nOMEGA is missing.");
        }
        
        // Check sigma
        if(oldOutput.sigma != null)
        {
            if(newOutput.sigma != null)
            {
                if(oldOutput.sigma.length == newOutput.sigma.length)
                {
                    for(int i = 0; i < oldOutput.sigma.length; i++)
                    {
                        if(oldOutput.sigma[i].length == newOutput.sigma[i].length)
                        {
                            if(!checkMatrix(oldOutput.sigma[i], newOutput.sigma[i]))
                                msg.append("\nValue of SIGMA block " + (i + 1) + " is different.  index = " + index + values());
                        }
                        else
                            msg.append("\nDimension of of SIGMA blocks " + (i + 1) + " is wrong." +
                                       "\n    Old value: " + oldOutput.sigma[i].length + "     New value: " + newOutput.sigma[i].length);
                    }
                }
                else
                    msg.append("\nTotal number of SIGMGA blocks is wrong." +
                               "\n    Old value: " + oldOutput.sigma.length + "     New value: " + newOutput.sigma.length);
            }
            else
                msg.append("\nSIGMA is missing.");
        }
        
        // Check standard error vector
        if(oldOutput.stdErrVector != null)
        {
            if(newOutput.stdErrVector != null)
            {
                if(oldOutput.stdErrVector.length == newOutput.stdErrVector.length)
                {
                    double temp = rErr;
                    rErr = rErr * pdfm;
                    if(!checkVector(oldOutput.stdErrVector, newOutput.stdErrVector))
                        msg.append("\nValue of standard error (stderror) is different.  index = " + index +
                                   "\n    Old value: " + oldOutput.stdErrVector[index -1] + "     New value: " + newOutput.stdErrVector[index -1]);
                    rErr = temp;
                }
                else
                    msg.append("\nLength of standard error (stderror) vector is wrong." +
                               "\n    Old value: " + oldOutput.stdErrVector.length + "     New value: " + newOutput.stdErrVector.length);
            }
            else
                msg.append("\nStandard error (stderror) vector is missing.");
        }
        
        // Check covariance
        if(oldOutput.covariance != null)
        {
            if(newOutput.covariance != null)
            {
                if(oldOutput.covariance.length == newOutput.covariance.length)
                {
                    if(!checkMatrix(oldOutput.covariance, newOutput.covariance))
                        msg.append("\nCovariance matrix is different.  index = " + index + values());
                }
                else
                    msg.append("\nDimension of covariance matrix is wrong." +
                               "\n    Old value: " + oldOutput.covariance.length + "     New value: " + newOutput.covariance.length);
            }
            else
                msg.append("\nCovariance matrix is missing.");            
        }
        
        // Check inverse covariance
        if(oldOutput.invCovariance != null)
        {
            if(newOutput.invCovariance != null)
            {
                if(oldOutput.invCovariance.length == newOutput.invCovariance.length)
                {
                    isInverseMatrix = true;
                    if(!checkMatrix(oldOutput.invCovariance, newOutput.invCovariance))
                        msg.append("\nInverse covariance matrix is different.  index = " + index + values());
                    isInverseMatrix = false;
                }
                else
                    msg.append("\nDimension of inverse covariance matrix is wrong." +
                               "\n    Old value: " + oldOutput.invCovariance.length + "     New value: " + newOutput.invCovariance.length);
            }
            else
                msg.append("\nInverse covariance matrix is missing.");            
        }
        
        // Check correlation
        if(oldOutput.correlation != null)
        {
            if(newOutput.correlation != null)
            {
                if(oldOutput.correlation.length == newOutput.correlation.length)
                {
                    if(!checkMatrix(oldOutput.correlation, newOutput.correlation))
                        msg.append("\nCorrelation matrix is different.  index = " + index + values());
                }
                else
                    msg.append("\nDimension of correlation matrix is wrong." +
                               "\n    Old value: " + oldOutput.correlation.length + "     New value: " + newOutput.correlation.length);
            }
            else
                msg.append("\nCorrelation matrix is missing."); 
        }
        
        // Check coefficient of variation
        if(oldOutput.coefVariation != null)
        {
            if(newOutput.coefVariation != null)
            {
                if(oldOutput.coefVariation.length == newOutput.coefVariation.length)
                {
                    double temp = rErr;
                    rErr = rErr * pdfm;
                    if(!checkVector(oldOutput.coefVariation, newOutput.coefVariation))
                        msg.append("\nCoefficient of variation is different.  index = " + index + values());
                    rErr = temp;
                }
                else
                    msg.append("\nLength of coefficient of variation is wrong." +
                               "\n    Old value: " + oldOutput.coefVariation.length + "     New value: " + newOutput.coefVariation.length);                
            }
            else
                msg.append("\nCoefficient of variation is missing.");
        }
        
        // Check confidence interval
        if(oldOutput.confInterval != null)
        {
            if(newOutput.confInterval != null)
            {
                if(oldOutput.confInterval[0].length == newOutput.confInterval[0].length)
                {
                    if(!checkVector(oldOutput.confInterval[0], newOutput.confInterval[0]))
                        msg.append("\nConfidence interval lower bound is different.  index = " + index + values());
                }
                else
                    msg.append("\nLength of confidence interval lower bound is wrong." +
                               "\n    Old value: " + oldOutput.confInterval[0].length + "     New value: " + newOutput.confInterval[0].length);
                if(oldOutput.confInterval[1].length == newOutput.confInterval[1].length)
                {
                    if(!checkVector(oldOutput.confInterval[1], newOutput.confInterval[1]))
                        msg.append("\nConfidence interval upper bound is different.  index = " + index + values());
                }
                else
                    msg.append("\nLength of confidence interval upper bound is wrong." +
                               "\n    Old value: " + oldOutput.confInterval[1].length + "     New value: " + newOutput.confInterval[1].length);
            }
            else
                msg.append("\nConfidence interval is missing.");
        }
        
        // Check presentation data
        if(oldOutput.dataAll != null)
        {
            if(newOutput.dataAll != null)
            {
                if(oldOutput.dataAll.length == newOutput.dataAll.length)
                {
                    int indexDV = oldOutput.dataItems.indexOf("DV");
                    int indexF = oldOutput.dataItems.indexOf("F");
                    for(int i = 0; i < oldOutput.dataAll.length; i++)
                    {
                        if(oldOutput.dataAll[i].length == newOutput.dataAll[i].length)
                        {
                            for(int j = 0; j < oldOutput.dataAll[i].length; j++)
                            {
                                double rTol = rErr;
                                double aTol = aErr;
                                String label = (String)oldOutput.dataItems.get(j);
                                if(label.endsWith(")") && !label.startsWith("THETA("))
                                    rTol = rErr * pdfm;
                                if(label.endsWith("RES") && !label.endsWith("WRES") && label.indexOf("WETARES(") == -1)
                                {
                                    double DV = oldOutput.dataAll[i][indexDV];
                                    double F = oldOutput.dataAll[i][indexF];
//                                    double s = DV - F == 0 ?  1 : oldOutput.dataAll[i][j] / (DV - F);
//                                    aTol = s * (rErr * (Math.abs(DV) + Math.abs(F)) + aErr);
                                    aTol = rErr * (Math.abs(DV) + Math.abs(F));
                                    rTol = 0;
                                }
                                if(!label.endsWith("WRES") && label.indexOf("WETARES(") == -1 &&
                                   !checkNumber(oldOutput.dataAll[i][j], newOutput.dataAll[i][j], aTol, rTol))
                                    msg.append("\nValue of presentation data at row " + (i + 1) + ", column " + (j + 1) + ", " + label +", is different." +
                                           "\n    Old value: " + oldOutput.dataAll[i][j] + "     New value: " + newOutput.dataAll[i][j]);
                            }
                        }
                        else
                            msg.append("\nNumber of columns at row  " + (i + 1) + " of presentation data is wrong." +
                               "\n    Old value: " + oldOutput.dataAll[i].length + "     New value: " + newOutput.dataAll[i].length);
                            
                    }
                }
                else
                    msg.append("\nNumber of rows of presentation data is wrong." +
                               "\n    Old value: " + oldOutput.dataAll.length + "     New value: " + newOutput.dataAll.length);
            }
            else
                msg.append("\nPresentation data is missing.");
        }
        
        // Check alpha
        if(oldOutput.alpha != null)
        {
            if(newOutput.alpha != null)
            {
                if(oldOutput.alpha.length == newOutput.alpha.length)
                {
                    for(int i = 0; i < 2; i++)
                    {
                        String which = i == 0? "center" : "step";
                        if(oldOutput.alpha[i].length == newOutput.alpha[i].length)
                        {
                            for(int j = 0; j < nTheta; j++)
                                if(!checkVector(new String[]{oldOutput.alpha[i][j]}, new String[]{newOutput.alpha[i][j]}))
                                    msg.append("\nValue of alpha " + which + " for THETA " + (j + 1) + " is different." + values());
                            int start = nTheta;
                            for(int j = 0; j < nOmega.length; j++)
                            {
                                String[] oldOmega = new String[nOmega[j]];
                                String[] newOmega = new String[nOmega[j]];
                                for(int k = 0; k < nOmega[j]; k++)
                                {
                                    oldOmega[k] = oldOutput.alpha[i][start + k];
                                    newOmega[k] = newOutput.alpha[i][start + k];
                                }
                                if(!checkVector(oldOmega, newOmega))
                                    msg.append("\nValue of alpha " + which + " for OMEGA block" + (j + 1) + " is different.  index = " + index + values());
                                start += nOmega[j];
                            }
                            for(int j = 0; j < nSigma.length; j++)
                            {
                                String[] oldSigma = new String[nSigma[j]];
                                String[] newSigma = new String[nSigma[j]];
                                for(int k = 0; k < nSigma[j]; k++)
                                {
                                    oldSigma[k] = oldOutput.alpha[i][start + k];
                                    newSigma[k] = newOutput.alpha[i][start + k];
                                }
                                if(!checkVector(oldSigma, newSigma))
                                    msg.append("\nValue of alpha " + which + " for SIGMA block" + (j + 1) + " is different.  index = " + index + values());
                                start += nSigma[j];
                            }
                        }
                        else
                            msg.append("\nNumber of alpha " + which + " elements is wrong." +
                                       "\n    Old value: " + oldOutput.alpha[i].length + "     New value: " + newOutput.alpha[i].length);
                    }
                }
                else
                    msg.append("\nNumber of alpha vectors is wrong." +
                               "\n    Old value: " + oldOutput.alpha.length + "     New value: " + newOutput.alpha.length);
            }
            else
                msg.append("\nAlpha is missing.");
        }
        
        // Check likelihood
        if(oldOutput.likelihood != null)
        {
            if(newOutput.likelihood != null)
            {
                if(oldOutput.likelihood.length == newOutput.likelihood.length && newOutput.likelihood[0].length == 3)
                {
                    for(int i = 0; i < newOutput.likelihood.length; i++)
                    {
                        for(int j = 0; j < 3; j++)
                        {
                            String which = "center";
                            if(j == 0) which = "lower";
                            if(j == 2) which = "upper";
                            if(!checkVector(new String[]{oldOutput.likelihood[i][j]}, new String[]{newOutput.likelihood[i][j]}))
                                msg.append("\nValue of likelihood " + which + " " + (i + 1) + " is different." + values());
                        }
                    }
                }
                else
                    msg.append("\nNumber of likelihood is wrong." +
                               "\n    Old value: " + oldOutput.likelihood.length + "     New value: " + newOutput.likelihood.length);
            }
            else
                msg.append("\nLikelihood is missing.");
        }
        
        // Check likelihood standard error
        if(oldOutput.likelihood_std != null)
        {
            if(newOutput.likelihood_std != null)
            {
                if(oldOutput.likelihood_std.length == newOutput.likelihood_std.length && newOutput.likelihood_std[0].length == 3)
                {
                    for(int i = 0; i < newOutput.likelihood_std.length; i++)
                    {
                        for(int j = 0; j < 3; j++)
                        {
                            String which = "center";
                            if(j == 0) which = "lower";
                            if(j == 2) which = "upper";
                            if(!checkVector(new String[]{oldOutput.likelihood_std[i][j]}, new String[]{newOutput.likelihood_std[i][j]}))
                                msg.append("\nValue of likelihood standard error " + which + " " + (i + 1) + " is different." + values());
                        }
                    }
                }
                else
                    msg.append("\nNumber of likelihood standard error is wrong." +
                               "\n    Old value: " + oldOutput.likelihood_std.length + "     New value: " + newOutput.likelihood_std.length);                
            }
            else
                msg.append("\nLikelihood standard error is missing.");            
        }
        
        if(msg.length() == 0)
           System.out.print("No difference founnd.");
        else
        {
            System.out.println(msg.toString());
            System.exit(1);
        }
    }
    
    /** main method taking six arguments.
     *  @param args String array containing the following in order.<br>
     *              path-name of the old result.xml file;<br>
     *              path-name of the new result.xml file;<br>
     *              path-name of the old source.xml file;<br>
     *              relative error limit;<br>
     *              absolute error limit;<br>
     *              norm type: 1 as max|xi|, 2 as sum|xi|, 3 as sqrt(sum(xi*xi)), 4 passng any of 1,2,3.
     *              optional precision degradation factor for comparing matrix, 10 as the default.
     */
    public static void main(String[] args)
    {
        String oldFile = args[0];
        String newFile = args[1];
        String srcFile = args[2];
        rErr = Double.parseDouble(args[3]);
        aErr = Double.parseDouble(args[4]);
        norm = Integer.parseInt(args[5]);
        pdfm = 10;
        if(args.length == 7) pdfm = Double.parseDouble(args[6]);
        
        Output oldOutput = new Output();
        Output newOutput = new Output();
        new XMLReader(openFile(oldFile), oldOutput, true);
        new XMLReader(openFile(newFile), newOutput, true);
        
        if(oldOutput.alpha != null && newOutput.alpha != null)
            parseSource(openFile(srcFile));
        
        compare(oldOutput, newOutput);
    }
    
    // Open a file by passing in the file path-name and return the text of the file.
    private static String openFile(String filename)
    {
        File file = new File(filename);
        String text = null;
        try
	{
            StringBuffer buffer = new StringBuffer();
            BufferedReader in = new BufferedReader(new FileReader(file));
            String line;
            while((line = in.readLine()) != null)
                buffer.append(line).append("\n");
            text = buffer.toString();
            in.close();
        }
        catch(IOException e)
	{
            System.out.println(e.getMessage());
            System.exit(1);
        }
        return text;
    }
    
    // Parse source.xml to get number of THETA, number of OMEGA in each block and number of 
    // SIGMA in each block.  It takes source as a String obkect and initialize static variables.
    private static void parseSource(String source)
    {
        Document docSource = null;
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            docSource = builder.parse(new InputSource(new ByteArrayInputStream(source.getBytes())));
        }
        catch(ParserConfigurationException e)
        {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        catch(SAXException e)
        {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        catch(IOException e)
	{
            System.out.println(e.getMessage());
            System.exit(1);
        }
        Element src = docSource.getDocumentElement();
        NodeList list = docSource.getElementsByTagName("theta");
        nTheta = Integer.parseInt(((Element)list.item(0)).getAttribute("length"));
        list = docSource.getElementsByTagName("omega");
        nOmega = new int[list.getLength()];
        for(int i = 0; i < list.getLength(); i++)
        {
            int dimension = Integer.parseInt(((Element)list.item(i)).getAttribute("dimension"));
            String struct = ((Element)list.item(i)).getAttribute("struct");
            if(struct.equals("block"))
                dimension = (dimension + 1) * dimension / 2;
            nOmega[i] = dimension;
        }
        list = docSource.getElementsByTagName("sigma");
        nSigma = new int[list.getLength()];
        for(int i = 0; i < list.getLength(); i++)
        {
            int dimension = Integer.parseInt(((Element)list.item(i)).getAttribute("dimension"));
            String struct = ((Element)list.item(i)).getAttribute("struct");
            if(struct.equals("block"))
                dimension = (dimension + 1) * dimension / 2;
            nSigma[i] = dimension;
        }
    }
    
    private static String values()
    {
        return "\n    Old value: " + oldValue + "     New value: " + newValue;
    }
    
    private static double rErr, aErr, pdfm;
    private static String oldValue, newValue;
    private static int nTheta, norm, index;
    private static int[] nOmega, nSigma;
    private static StringBuffer msg = new StringBuffer();
    private static boolean isInverseMatrix;
}
