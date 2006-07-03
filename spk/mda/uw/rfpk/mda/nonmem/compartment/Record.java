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
package uw.rfpk.mda.nonmem.compartment;

import java.util.*;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.Utility;

/** This class generates MODEL, PK, DES and ERROR codes.
 *
 * @author  Jiaji Du
 */
public class Record {
    
    /** Creates a new instance of Record 
     * @param models all models.
     * @param subjectModel subject and model correspondance.
     */
    public Record(Vector models, Properties subjectModel)
    {
        this.models = models;
        this.subjectModel = subjectModel;
    }
    
    /** Generate MODEL code.
     * @return MODEL code.
     */
    protected String getModel()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        int nCompartments = 0;
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment compartment = (Element.Compartment)element;
                sb.append("COMP=(" + compartment.name);
                for(int j = 0; j < compartment.attributes.size(); j++)               
                    sb.append(" " + (String)compartment.attributes.get(j));               
                if(compartment.nInputs == 0) sb.append(" NODOSE");
                sb.append(")");
                if(i < size - 1)
                    sb.append("\n");
                nCompartments++;
            }
        }
        if(nCompartments == 0)
        {
            modelText = "";
            return "";
        }
        sb.insert(0, "NCOMPARTMENTS=" + nCompartments + " NEQUILIBRIUM=0 NPARAMETERS=0\n");
        modelText = sb.toString();
        return modelText;
    }
    
    /** Generate PK code.
     * @return PK code.
     */    
    protected String getPK()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment comp = (Element.Compartment)element;
                if(!comp.equations.equals(""))
                    sb.append("\n" + comp.equations);
                if(comp.force != null)
                    sb.append("\n" + comp.force);
                Enumeration keys = comp.parameters.keys();
                String param;
                while(keys.hasMoreElements())
                {
                    param = (String)keys.nextElement();
                    String value = comp.parameters.getProperty(param);
                    sb.append("\n" + param + "=" + comp.parameters.getProperty(param).substring(1));
                }
            }
            else
            {
                Element.Delay delay = (Element.Delay)element;
                if(delay.compartments.size() > 0)
                {
                    if(delay.delayTime != null)
                        sb.append("\nTLAG" + delay.number + "=" + delay.delayTime);
                    else
                        sb.append("\nTLAG" + delay.number + "=" + delay.delayModel);
                    sb.append("\nK" + delay.number + "=" + delay.nDelayComps + "/TLAG" + delay.number);
                }
            }
        }
        List keySet = new Vector(Model.variables.keySet());
        Iterator keyIter = keySet.iterator();
        String key, value;
        while(keyIter.hasNext())
        {
            key = (String)keyIter.next();
            value = Model.variables.getProperty(key);
            if(value != null)
                sb.append("\n" + key + "=" + value);
        }
        size = Model.fluxes.size();
        for(int i = 0; i < size; i++)
        {
            Element.Flux flux = (Element.Flux)Model.fluxes.get(i);
            if(flux.element1 instanceof Element.Compartment)
            {
                sb.append("\n" + flux.name + "=");           
                if(flux.flowRate != null)
                    sb.append(flux.flowRate);
                else if(flux.equations != null)
                    sb.append(flux.equations);
                else
                    sb.append(flux.mixedEffect);
            }
        }
        pkText = sb.toString().trim();
        return pkText;
    }
    
    /** Generate DES code.
     * @return DES code.
     */
    protected String getDes()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        int pIndex = size;
        
        // Find delay number versus last compartment number for the delay
        int compNumber = size;
        Properties delayNumber = new Properties();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Delay)
            {
                Element.Delay delay = (Element.Delay)element;
                compNumber += delay.nDelayComps - 1;
                delayNumber.setProperty(String.valueOf(delay.number), String.valueOf(compNumber));
            }
        }
        String[] eqns = new String[compNumber];
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment compartment = (Element.Compartment)element;
                String eqn = "\nDADT(" + compartment.number + ")=";
                for(int j = 0; j < Model.fluxes.size(); j++)
                {
                    Element.Flux flux = (Element.Flux)Model.fluxes.get(j);
                    if(flux.element1 == element)
                    {
                        if(((Element.Compartment)flux.element1).force != null)
                            eqn += "-" + flux.name + "*FF" + compartment.number;
                        else
                            eqn += "-" + flux.name + "*A(" + compartment.number + ")";
                    }
                    if(flux.element2 == element)
                    {
                        if(flux.element1 instanceof Element.Compartment)
                        {
                            if(((Element.Compartment)flux.element1).force != null)
                                eqn += "+" + flux.name + "*FF" + flux.element1.number;
                            else
                                eqn += "+" + flux.name + "*A(" + flux.element1.number + ")";
                        }
                        else if(flux.element1 instanceof Element.Delay)
                        {
                            Element.Delay delay = (Element.Delay)flux.element1;
                            int index = delay.compartments.indexOf(element);
                            String fraction = (String)delay.fractions.get(index);
                            
                            eqn += "+K" + delay.number + "*" + fraction + "*A(" + 
                                   delayNumber.getProperty(String.valueOf(delay.number)) + ")";
                        }
                    }
                }
                if(!eqn.endsWith("="))
//                    sb.append(eqn);
                    eqns[compartment.number - 1] = eqn;
            }
            else if(element instanceof Element.Delay)
            {
                Element.Delay delay = (Element.Delay)element;    
                String eqn = "\nDADT(" + delay.number + ")=";
                for(int j = 0; j < Model.fluxes.size(); j++)
                {
                    Element.Flux flux = (Element.Flux)Model.fluxes.get(j);
                    if(flux.element2 == element)
                    {
                        if(flux.element1 instanceof Element.Compartment && 
                           ((Element.Compartment)flux.element1).force != null)
                            eqn += "+" + flux.name + "*FF" + flux.element1.number;
                        else
                            eqn += "+" + flux.name + "*A(" + flux.element1.number + ")";
                    }
                }
                String k = "K" + delay.number;
                eqn += "-" + k + "*A(" + (size + 1) + ")   ; " + delay.name;
                if(!eqn.endsWith("="))
                {
//                    sb.append(eqn);
                    eqns[delay.number - 1] = eqn;
                    int nIndex = pIndex + delay.nDelayComps;
                    for(int j = pIndex + 1; j < nIndex; j++)
                    {
                        if(j > pIndex + 1) pIndex = j - 1;
                        eqn = "\nDADT(" + j + ")=" + k + "*(A(" + pIndex + ")-A(" + j + "))   ; " + delay.name;
//                        sb.append(eqn);
                        eqns[j - 1] = eqn;
                    }                   
                    pIndex = nIndex;
                }
            }
        }
        for(int i = 0; i < eqns.length; i++)
            sb.append(eqns[i]);
        desText = sb.toString().trim();
        if(desText.equals("null"))
            return "";
        return desText;       
    }
    
    /** Generate ERROR code.
     * @return ERROR code.
     */
    protected String getError()
    {
        StringBuffer sb = new StringBuffer();
        for(int i = 0; i < models.size(); i++)
        {
            Vector samples = ((Model)models.get(i)).samples;           
            for(int j = 0; j < samples.size(); j++)
            {
                Element.Sample sample = (Element.Sample)samples.get(j);
                String key = sample.name;
                if(sample.compartments.size() == 1)
                {
                    if(samples.size() == 1)
                    {
                        sb.append(sample.errorModel);
                    }
                    else
                    {
                        Element.Compartment cmp = (Element.Compartment)sample.compartments.get(0);
                        sb.append("IF(CMP .EQ. " + String.valueOf(cmp.number) + ") THEN\n" +
                                  sample.errorModel + "\nEND IF");
                    }
                }
            }
        }
        return sb.toString().trim().replaceAll("END IFIF", "ELSE IF");
    }

    
    protected boolean setModel(MDAObject object)
    {
        if(modelText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Model specification is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        String[] modelLines = modelText.split("\n");
        String[] ns = modelLines[0].split(" ");
        String n1 = ns[0].split("=")[1];
        String n2 = ns[1].split("=")[1];
        String n3 = ns[2].split("=")[1];
        int size = Integer.parseInt(n1);
        String[][] compartments = new String[size + 1][];
        compartments[0] = new String[3]; 
        compartments[0][0] = n1;
        compartments[0][1] = n2;
        compartments[0][2] = n3;
        for(int i = 1; i <= size; i++)
        {
            String compartment = modelLines[i];
            int end = compartment.length() -1;
            if(compartment.indexOf(" ") != -1)
                end = compartment.indexOf(" ");
            String name = compartment.substring(6, end);              
            compartments[i] = new String[8];
            if(name.startsWith("\"") || name.startsWith("'"))
            {
                name = name.substring(1, name.length() - 1);                    
            }
            compartments[i][0] = name;
            if(compartment.indexOf(" INITIALOFF") != -1)
                compartments[i][1] = "yes";
            else
                compartments[i][1] = "no";                
            if(compartment.indexOf(" NOOFF") != -1)
                compartments[i][2] = "yes";
            else
                compartments[i][2] = "no";
            if(compartment.indexOf(" NODOSE") != -1)
                compartments[i][3] = "yes";
            else
                compartments[i][3] = "no";
            if(compartment.indexOf(" EQUILIBRIUM") != -1)
                compartments[i][4] = "yes";
            else
                compartments[i][4] = "no";
            if(compartment.indexOf(" EXCLUDE") != -1)
                compartments[i][5] = "yes";
            else
                compartments[i][5] = "no";
            if(compartment.indexOf(" DEFOBSERVATION") != -1)
                compartments[i][6] = "yes";
            else
                compartments[i][6] = "no";
            if(compartment.indexOf(" DEFDOSE") != -1)
                compartments[i][7] = "yes";
            else
                compartments[i][7] = "no";
        }
        object.getSource().model = compartments;
        object.getRecords().setProperty("Model", "$MODEL " + modelText);
        return true;
    }
    
    protected boolean setPK(MDAObject object, MDAIterator iterator)
    {
        if(pkText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Model parameters are not defined.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        object.getRecords().setProperty("PK", "$PK " + "\n" + pkText);
        object.getSource().pk = "\n" + pkText + "\n";
        // Eliminate comments
        String code = Utility.eliminateComments(pkText); 
        // Find number of THETAs and number of ETAS
        int nTheta = Utility.find(code, "THETA");
        if(!iterator.getIsInd() && !iterator.getIsTwoStage())
        {
            if(iterator.getNTheta() == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of fixed effect parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            int nEta = Utility.find(code.replaceAll("THETA", ""), "ETA");
            if(nEta == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of random effect parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            iterator.setNTheta(nTheta);
            iterator.setNEta(nEta);
        }
        else
        {
            if(iterator.getNTheta() == 0)
                JOptionPane.showMessageDialog(null, "The number of random effect parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        // Check NONMEM compatibility
        Vector names = Utility.checkMathFunction(code, "Model Parameters");
        // Check parenthesis mismatch
        Vector lines = Utility.checkParenthesis(code, "Model Parameters");
        // Check expression left hand side
        Vector errors = Utility.checkLeftExpression(code, "Model Parameters");
        return true;
    }
        
    protected boolean setDes(MDAObject object)
    {
        if(desText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Diifferential equation structure is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        String record = "$DES " + "\n" + desText;
        object.getRecords().setProperty("Des", record);
        object.getSource().des = record.substring(5) + "\n";
        // Eliminate comments
        String code = Utility.eliminateComments(record); 
        // Check NONMEM compatibility
        Vector names = Utility.checkMathFunction(code, "Differential Equation Structure");
        // Check parenthesis mismatch
        Vector lines = Utility.checkParenthesis(code, "Differential Equation Structure");
        // Check expression left hand side
        Vector errors = Utility.checkLeftExpression(code, "Differential Equation Structure");
        return true;
    }
    
    protected boolean setError(MDAObject object)
    {
        if(errorText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Residual unknwn variability model is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        String record = "$ERROR " + "\n" + errorText;
        object.getRecords().setProperty("Error", record);
        object.getSource().error = record.substring(7) + "\n";
                
        // Eliminate comments
        String code = Utility.eliminateComments(record); 
        // Check NONMEM compatibility
        Vector names = Utility.checkMathFunction(code, "Residual Unknown Variability Model");
        // Check parenthesis mismatch
        Vector lines = Utility.checkParenthesis(code, "Residual Unknown Variability Model");
        // Check expression left hand side
        Vector errors = Utility.checkLeftExpression(code, "Residual Unknown Variability Model");
        return true;
    }
    
    private Vector models;
    private Properties subjectModel;
    private String modelText, pkText, desText, errorText;
}
