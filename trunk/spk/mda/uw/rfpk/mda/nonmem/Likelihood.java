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

import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.display.Output;

/** This class handles likelihood evaluation method.
 * @author  Jiaji Du
 */
public class Likelihood {
    
    /** Creates a new instance of Likelihood.
     * @param spkInput SPK input file as a String object.
     * @param report SPK report file as a String object.
     * @param jobId job ID.
     * @param isLibrary true if the job belongs to the library, false otherwise.
     * @return a String object containing the modified SPK input file.  
     */
    public static String changeInput(String spkInput, String report, long jobId, boolean isLibrary) 
    {
        // Get the model from the input file
        int index1 = spkInput.indexOf("<spkdata");
        int index2 = spkInput.indexOf("<spkmodel");
        String source = spkInput.substring(0, index1 - 22);
        String data = spkInput.substring(index1 - 22, index2 - 22);
        String model = spkInput.substring(index2 - 22);

        // Modify source.
        source = replaceSourceParameters(source, report);
 
        // Remove attributes of <pop_analysis>.
        String front = source.substring(0, source.indexOf("<pop_analysis "));
        int popSizeIndex = source.indexOf("pop_size=", source.indexOf("<pop_analysis "));
        String popSize = source.substring(popSizeIndex, source.indexOf(" ", popSizeIndex));
        String back = source.substring(source.indexOf(">", source.indexOf("<pop_analysis ")) + 1);
        source = front + "<pop_analysis is_estimation=\"no\" " + popSize + ">" + back;
        JOptionPane.showMessageDialog(null, "Initial values of the parameters have been replaced" +
                                      "\nby the estimated values obtained from the parent job." +         
                                      "\nAttributes of the <pop_analysis> have been changed.");
        
        // Remove simulation in the source.
        if(source.indexOf("<simulation ") != -1)
        {
            // Remove <simulation> element from the source.
            front = source.substring(0, source.indexOf("<simulation ") - 1);
            back = source.substring(source.indexOf(">", source.indexOf("<simulation ")) + 2);
            source = front + back;
            data = Utility.replaceDataDVbySimDV(report, source, data);
            JOptionPane.showMessageDialog(null, "The <simulation> element of the source has been removed." +
                                         "\nThe DV values of the dataset have been replaced by the" +
                                         "\nsimulated DV values obtained from the parent job.");            
        }
        return source + data + model;
    }
                
    // Replace initial values of parameters in the souce by those in the report 
    private static String replaceSourceParameters(String source, String report)
    {
        // Replace theta values of souce by those of report 
        int beginIndex = source.indexOf("<in>", source.indexOf("<theta ")) + 5;
        int endIndex = source.indexOf("</in>", beginIndex);
        String front = source.substring(0, beginIndex);
        String back = source.substring(endIndex);
        beginIndex = report.indexOf("<value>", report.indexOf("<theta_out "));
        endIndex = report.indexOf("</theta_out>", beginIndex);
        source = front + report.substring(beginIndex, endIndex) + "               " + back;
                       
        // Replace omega and sigma of source by those of report
        beginIndex = source.indexOf("<omega ");
        endIndex = source.lastIndexOf("</sigma>") + 8;
        front = source.substring(0, beginIndex);
        back = source.substring(endIndex);
        report = report.replaceAll("omega_out", "omega");
        report = report.replaceAll("sigma_out", "sigma");
        beginIndex = report.indexOf("<omega ");
        endIndex = report.lastIndexOf("</omega>") + 8;
        String[] blocks = report.substring(beginIndex, endIndex).split("<omega ");
        String omega = "";
        for(int i = 1; i < blocks.length; i++)
            omega += "<omega " + blocks[i].replaceFirst("<value>", "<in>\n<value>");
        omega = omega.replaceAll("</omega>", "</in>\n</omega>");
        beginIndex = report.indexOf("<sigma ");
        endIndex = report.lastIndexOf("</sigma>") + 8;
        blocks = report.substring(beginIndex, endIndex).split("<sigma ");
        String sigma = "";
        for(int i = 1; i < blocks.length; i++)
            sigma += "<sigma " + blocks[i].replaceFirst("<value>", "<in>\n<value>");
        sigma = sigma.replaceAll("</sigma>", "</in>\n</sigma>");
        return front + omega + "\n" + sigma + "            " + back;
    }
    
    /** Insert <monte_carlo> element for using likelihood evaluation methods.
     * @param source the source XML without <monte_carlo> element.
     * @param jobMethodCode the job method code.
     * @return the source XML with <monte_carlo> element inserted.
     */
    public static String insertLeElement(String source, String jobMethodCode)
    {
        // Find number of random effects
        int nEta = 0;
        String text = new String(source);
        while(text.indexOf("<omega ") != -1)
        {
            int beginIndex = text.indexOf("dimension=", text.indexOf("<omega ")) + 11;
            int endIndex = text.indexOf("\"", beginIndex);
            nEta += Integer.parseInt(text.substring(beginIndex, endIndex));
            text = text.substring(endIndex);
        }
        String nEvaluation = "";
        if(jobMethodCode.equals("gr"))
        {
            for(int i = 0; i < nEta; i++)
            {
                String nGrid = JOptionPane.showInputDialog(null, "Enter number of grid points for random effect " + (i + 1), "10");
                if(nGrid == null || !Utility.isPosIntNumber(nGrid.trim()))
                {
                    JOptionPane.showMessageDialog(null, "The number of grid points is missing",  
                                                  "Input Error",
                                                  JOptionPane.ERROR_MESSAGE);
                    return null;
                }
                else
                    nEvaluation += "\n            <value>" + nGrid + "</value>";
            }
        }
        else if(jobMethodCode.equals("ml") || jobMethodCode.equals("mi"))
        {
            nEvaluation = JOptionPane.showInputDialog(null, "Enter number of individual objective evaluations", "1000");
            if(nEvaluation == null || !Utility.isPosIntNumber(nEvaluation.trim()))
            {
                JOptionPane.showMessageDialog(null, "The number of individual objective evaluations is missing",  
                                              "Input Error",            
                                              JOptionPane.ERROR_MESSAGE);                 
                return null;
            }
            else
                nEvaluation = "\n            <value>" + nEvaluation + "</value>";                
        }
        else if(jobMethodCode.equals("an"))
            nEvaluation = "\n            <value>1</value>";
            
        String integrationMethod = "plain";
        if(jobMethodCode.equals("gr"))
            integrationMethod = "grid";
        else if(jobMethodCode.equals("mi"))
            integrationMethod = "miser";
        else if(jobMethodCode.equals("an"))
            integrationMethod = "analytic";

        // Modify the source
        source = source.replaceFirst("</nonmem>", "   <monte_carlo method=\"" + integrationMethod + 
                                     "\">\n         <number_eval>" + nEvaluation + 
                                     "\n         </number_eval>\n      </monte_carlo>\n   </nonmem>");        
        return source;
    }
}
