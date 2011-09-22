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
import java.awt.Color;
import javax.swing.JTextArea;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.Utility;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;

/** This class generates MODEL, PK, DES and ERROR codes.
 *
 * @author  Jiaji Du
 */
public class Record {
    
    /** Creates a new instance of Record.
     * @param tool the DesignTool object.
     */
    public Record(DesignTool tool)
    {
        this.tool = tool;
    }
    
    /** Generate MODEL code.
     * @return MODEL code.
     */
    protected String getModel()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        int nCompartments = 0;
        ArrayList<Element.Delay> delays = new ArrayList<Element.Delay>();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment compartment = (Element.Compartment)element;
                String name = compartment.name;
                if(compartment.name.indexOf(" ") != -1)
                    name = "\"" + name + "\"";
                sb.append("COMP=(" + name);
                for(int j = 0; j < compartment.attributes.size(); j++)               
                    sb.append(" " + (String)compartment.attributes.get(j));
                sb.append(")   ;" + compartment.xCenter + "," + compartment.yCenter + "\n");
                nCompartments++;
            }
            else
            {
                Element.Delay delay = (Element.Delay)element;
                delays.add(delay);                
                sb.append("COMP=(" + delay.name + ")   ;" + delay.xCenter + "," + delay.yCenter + "\n");
                nCompartments += delay.nDelayComps;
            }
        }
        for(int i = 0; i < delays.size(); i++)
        {
            Element.Delay delay = (Element.Delay)delays.get(i);
            for(int j = 1; j < delay.nDelayComps; j++)
                sb.append("COMP=(" + delay.name + ")\n");
        }
        if(nCompartments == 0)
        {
            modelText = "";
            return "";
        }
        String numbers = "NCOMPARTMENTS=" + nCompartments;
        int nParam = Utility.find(pkText, "P");
        if(nParam != 0) numbers += " NPARAMETERS=" + nParam;
        sb.insert(0, numbers + "\n");
        modelText = sb.toString().trim();
        return modelText;
    }
    
    /** Generate PK code.
     * @return PK code.
     */    
    protected String getPK()
    {
        StringBuffer sb = new StringBuffer();
        for(Parameter parameter : Model.parameterList)
        {
            int indexIF = parameter.value.indexOf("IF(");
            if(indexIF == -1)
                sb.append(parameter.value.replaceAll("\n", ";\n"));
            else
            {
                sb.append(parameter.value.substring(0, indexIF).replaceAll("\n", ";\n"));
                sb.append(parameter.value.substring(indexIF));
            }
            sb.append("\n"); 
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
                    if(flux.element2 == element && flux.element1 != null)
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
                            String k = delay.nDelayComps + "/TLAG" + delay.number;
                            eqn += "+" + k + "*" + fraction + "*A(" + 
                                   delayNumber.getProperty(String.valueOf(delay.number)) + ")";
                        }
                    }
                }
                eqn += " " + compartment.timeRate;
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
                    if(flux.element2 == element && flux.element1 != null)
                    {
                        if(flux.element1 instanceof Element.Compartment && 
                           ((Element.Compartment)flux.element1).force != null)
                            eqn += "+" + flux.name + "*FF" + flux.element1.number;
                        else
                            eqn += "+" + flux.name + "*A(" + flux.element1.number + ")";
                    }
                }
                String k = delay.nDelayComps + "/TLAG" + delay.number;
                eqn += "-" + k + "*A(" + delay.number + ")   ;" + delay.name;
                if(!eqn.endsWith("="))
                {
//                    sb.append(eqn);
                    eqns[delay.number - 1] = eqn;
                    int nIndex = pIndex + delay.nDelayComps;
                    int l;
                    for(int j = pIndex + 1; j < nIndex; j++)
                    {
                        if(j == pIndex + 1) l = delay.number;
                        else l = j - 1;                       
                        eqn = "\nDADT(" + j + ")=" + k + "*(A(" + l + ")-A(" + j + "))   ;" + delay.name;
//                        sb.append(eqn);
                        eqns[j - 1] = eqn;
                    }                   
                    pIndex = nIndex - 1;
                }
            }
        }
        for(int i = 0; i < eqns.length; i++)
            if(eqns[i] != null)
                sb.append(eqns[i]);
        desText = sb.toString().trim();
        if(!Model.desEqns.equals(""))
            desText = Model.desEqns + "\n" + desText;
        return desText;       
    }
    
    /** Generate ERROR code.
     * @return ERROR code.
     */
    protected String getError()
    {
        StringBuffer sb = new StringBuffer();
        Vector models = tool.models;
        String space = "";
        for(int i = 0; i < models.size(); i++)
        {
            Vector samples = ((Model)models.get(i)).samples;
            if(models.size() > 1 && samples.size() > 0)
            {
                ArrayList list = getKeyIntervals(tool.subjectModel, String.valueOf(((Model)models.get(i)).id));
                String condition = "IF(";
                String[] interval;
                space = "  ";
                for(int j = 0; j < list.size(); j++)
                {
                    interval = (String[])list.get(j);
                    if(interval[0].equals(interval[1]))
                        condition += "ID .EQ. " + interval[0];
                    else
                        if(list.size() == 1) condition += "ID .GE. " + interval[0] + " .AND. ID .LE. " + interval[1];
                        else condition += "(ID .GE. " + interval[0] + " .AND. ID .LE. " + interval[1] + ")";
                    if(j == list.size() - 1)
                        condition += ") THEN";
                    else
                        condition += " .OR. ";
                }
                sb.append(condition + "\n");
            }                       
            for(int j = 0; j < samples.size(); j++)
            {
                Element.Sample sample = (Element.Sample)samples.get(j);
                String str = space;
                // Assume each sample is taken from only one compartment.
                if(samples.size() == 1)
                    str += sample.errorModel;
                else
                {
                    Element.Compartment comp = (Element.Compartment)sample.compartments.get(0);
                    str += "IF(CMT .EQ. " + comp.number + ") THEN\n  " + space + sample.errorModel + 
                           "\n" + space + "ENDIF";
                }
                sb.append(str);
            }
            if(models.size() > 1 && samples.size() > 0) sb.append("\nENDIF");
        }
        errorText = sb.toString().replaceAll("ENDIF  IF", "ENDIF\n  IF").replaceAll("ENDIFIF", "ENDIF\nIF");
//        errorText = sb.toString().replaceAll("END IF  IF", "ELSE IF").replaceAll("END IFIF", "ELSE IF");
        sb = new StringBuffer();
        for(int i = 0; i < models.size(); i++)
        {
            Model model = (Model)models.get(i);
            Vector samples = model.samples;
            Vector inputs = model.inputs;
            if(inputs.size() > 0 || samples.size() > 0)
            {
                sb.append("\n;" + model.name);
                for(int j = 0; j < inputs.size(); j++)
                {
                    Element.Input input = (Element.Input)inputs.get(j);
                    sb.append(";" + input.name + ":" + input.xCenter + "," + input.yCenter + 
                              "(" + ((Element)input.compartments.get(0)).number + ")");
                }
                for(int j = 0; j < samples.size(); j++)
                {
                    Element sample = (Element)samples.get(j);
                    sb.append(";" + sample.name + ":" + sample.xCenter + "," + sample.yCenter);
                }
            }
        }
        errorText += sb.toString();
        if(Model.errorEqns.trim().length() != 0)
            errorText = Model.errorEqns.trim().replaceAll("\n", ";\n") + ";\n" + errorText;
        return errorText;
    }

    /** Set model specification to MDAObject object.
     * @param object MDAObject object.
     * @return true if successful, false otherwise.
     */
    protected boolean setModel(MDAObject object)
    {
        // Check completeness
        if(modelText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Model specification is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        for(int i = 0; i < tool.models.size(); i++)
        {
            Model model = (Model)tool.models.get(i);
            if(model.inputs.size() == 0)
            {
                JOptionPane.showMessageDialog(null, "The model for group " + model.name + " has not a dose.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if(model.samples.size() == 0)
            {
                JOptionPane.showMessageDialog(null, "The model for group " + model.name + " has no observation.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        int nDefDose = 0;
        int nDefObservation = 0;
        for(Element element : Model.elements)
        {
            if(element instanceof Element.Compartment)
            {
                if(((Element.Compartment)element).attributes.indexOf("DEFDOSE") != -1)
                    nDefDose++;
                if(((Element.Compartment)element).attributes.indexOf("DEFOBSERVATION") != -1)
                    nDefObservation++;    
            }
        }
        if(nDefDose == 0)
        {
            JOptionPane.showMessageDialog(null, "DEFDOSE is missing.", "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        if(nDefObservation == 0)
        {
            JOptionPane.showMessageDialog(null, "DEFOBSERVATION is missing.", "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        if(nDefDose > 1)
        {
            JOptionPane.showMessageDialog(null, "Multiple DEFDOSEs were found.", "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        if(nDefObservation > 1)
        {
            JOptionPane.showMessageDialog(null, "Multiple DEFOBSERVATIONs were found.", "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        // Set source
        String[] modelLines = modelText.split("\n");
        String[] ns = modelLines[0].split(" ");
        String n1 = null;
        String n2 = "0";
        String n3 = null;
        for(int i = 0; i < ns.length; i++)
        {
            if(ns[i].startsWith("NCOMPARTMENTS"))
                n1 = ns[i].substring(14);
            if(ns[i].startsWith("NEQUILIBRIUM"))
                n2 = ns[i].substring(13);
            if(ns[i].startsWith("NPARAMETERS"))
                n3 = ns[i].substring(12);
        }
        int size = Integer.parseInt(n1);         
        String[][] compartments = new String[size + 1][];
        compartments[0] = new String[3];
        compartments[0][0] = n1;
        compartments[0][1] = n2;
        compartments[0][2] = n3;
        for(int i = 1; i <= size; i++)
        {
            String name = "";
            int end = -1;
            String compartment = modelLines[i].substring(6);
            if(compartment.startsWith("\""))
            {
                end = compartment.indexOf("\"", 1);
                name = compartment.substring(1, end++);
            }
            else if(compartment.startsWith("'"))
            {
                end = compartment.indexOf("'", 1);
                name = compartment.substring(1, end++);
            }
            else
            {
                end = compartment.indexOf(" ");
                if(end == -1) end = compartment.indexOf(")");
                name = compartment.substring(0, end);
            }
            compartment = compartment.substring(end);          
            compartments[i] = new String[8];                
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
        
        // Set source and record
        object.getSource().model = compartments;
        object.getRecords().setProperty("Model", "$MODEL " + modelText);
        
        return true;
    }
     
    /** Set model parameters definition to MDAObject object.
     * @param object MDAObject object.
     * @param iterator MDAIterator object.
     * @return true if successful, false otherwise.
     */   
    protected boolean setPK(MDAObject object, MDAIterator iterator)
    {
        // Check completeness
        if(pkText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Model parameters are not defined.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        String[] rows = pkText.split("\n");
        String[] sizes;
        for(int i = 0; i < rows.length; i++)
        {
            if(rows[i].startsWith("IF(") || rows[i].equals("ELSE") || rows[i].equals("ENDIF"))
                continue;
            sizes = rows[i].trim().split("=");
            if(sizes.length != 2)
            {
                JOptionPane.showMessageDialog(null, "Value of " + sizes[0] + " is missing.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        
        // Eliminate comments
        String code = Utility.eliminateComments(pkText);        
       
        // Find number of THETAs and number of ETAS
        int nTheta = Utility.find(code, "THETA");
        int nEta = Utility.find(code.replaceAll("THETA", ""), "ETA");
        if(iterator.analysis.equals("population"))
        {
            if(nTheta == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of fixed effect parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
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
            if(nTheta == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of random effect parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            if(nEta != 0)
            {
                JOptionPane.showMessageDialog(null, "ETA is not a valid model parameter for an individual model.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            iterator.setNTheta(nTheta);
        }
        
        // Set source and record
        object.getRecords().setProperty("PK", "$PK \n" + pkText);
        object.getSource().pk = "\n" + pkText + "\n";
        object.getSource().nTheta = String.valueOf(iterator.getNTheta());   
        
        return true;
    }

    /** Set differential equation structure to MDAObject object.
     * @param object MDAObject object.
     * @return true if successful, false otherwise.
     */
    protected boolean setDes(MDAObject object)
    {
        // Check completeness
        if(desText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Diifferential equation structure is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        String[] rows = desText.substring(desText.indexOf("DADT")).split("\n");
        String[] sizes;
        for(int i = 0; i < rows.length; i++)
        {
            if(rows[i].indexOf("=") == -1)
            {
                JOptionPane.showMessageDialog(null, "Compartment " + (i + 1) + " is isolated.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        
        // Set source and record
        object.getRecords().setProperty("Des", "$DES \n" + desText);
        object.getSource().des = "\n" + Utility.eliminateComments(desText) + "\n";
        
        return true;
    }
    
    /** Set residual unknown variability model to MDAObject object.
     * @param object MDAObject object.
     * @param iterator MDAIterator object.
     * @return true if successful, false otherwise.
     */    
    protected boolean setError(MDAObject object, MDAIterator iterator)
    {
        // Check completeness
        if(errorText.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Residual unknwn variability model is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        for(int i = 0; i < tool.models.size(); i++)
        {
            Model model = (Model)tool.models.get(i);
            for(int j = 0; j < model.samples.size(); j++)
            {
                Element.Sample sample = (Element.Sample)model.samples.get(j);
                if(sample.errorModel.trim().length() == 0)
                {
                    JOptionPane.showMessageDialog(null, "The RUV for " + sample.name + " is missing.",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }
                     
        // Eliminate comments
        String code = Utility.eliminateComments(errorText).trim();

        // Find number of EPSs for population analysis or Etas for individual analysis
        int nEta = 0;
        int nEps = 0;
        if(iterator.analysis.equals("population"))
        {
            String pkCode = Utility.eliminateComments(pkText);
            if(pkCode != null)
                nEta = Utility.find(pkCode + code, "ETA");
            else
                nEta = Utility.find(code, "ETA");
            nEps = Utility.find(code, "EPS");
            if(nEps == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of residual unkown variability parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        else
        {
            nEta = Utility.find(code, "ETA");
            if(nEta == 0)
            {
                JOptionPane.showMessageDialog(null, "The number of residual unkown variability parameters is 0.\n",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        }
        iterator.setNEta(nEta);
        if(iterator.analysis.equals("population"))
            iterator.setNEps(nEps);
        
        // Set source and record
        object.getRecords().setProperty("Error", "$ERROR \n" + errorText);
        object.getSource().error = "\n" + code + "\n";
        object.getSource().nEta = String.valueOf(iterator.getNEta());

        return true;
    }
    
    /** Check the code and highlight the found parts.
     * @param step Step title.
     * @param textArea TextArea.
     * @param highlighterNumber Highlighter number, either 1, 2, or 3.
     */
    protected void checkCode(String step, JTextArea textArea, int highlighterNumber)
    {
        // Get text
        String code = textArea.getText();
        
        // Check NONMEM compatibility
        Vector names = Utility.checkMathFunction(code, step);
        // Check parenthesis mismatch
        Vector lines = Utility.checkParenthesis(code, step);
        // Check expression left hand side
        Vector errors = Utility.checkLeftExpression(code, step);
        
        // Highlight the incompatible function names and mismatched parenthesis lines
        if(names.size() > 0 || lines.size() > 0 || errors.size() > 0)
        {
            DefaultHighlighter highlighter;
            boolean isHighlighted;
            if(highlighterNumber == 1)
            {
                highlighter = highlighter1;
                isHighlighted = isHighlighted1;
            }
            else if(highlighterNumber == 2)
            {
                highlighter = highlighter2;
                isHighlighted = isHighlighted2;
            }
            else if(highlighterNumber == 3)
            {
                highlighter = highlighter3;
                isHighlighted = isHighlighted3;
            }
            else
                return;
            if(isHighlighted)
            {                
                 highlighter.removeAllHighlights();
                 isHighlighted = false;
            }
            textArea.setHighlighter(highlighter);
            javax.swing.text.Element paragraph = textArea.getDocument().getDefaultRootElement();          
            String[] text = textArea.getText().split("\n");
            try
            {
                for(int i = 0; i < text.length; i++)
                {
                    for(int j = 0; j < names.size(); j++)
                    {
                        int comment = text[i].indexOf(";");
                        if(comment != 0)
                        {
                            String line = text[i];
                            if(comment > 0)
                                line = text[i].substring(0, comment);
                            int pos = 0;
                            int offset = paragraph.getElement(i).getStartOffset();
                            String name = (String)names.get(j);
                            while ((pos = text[i].indexOf(name, pos)) >= 0) 
                            {                
                                highlighter.addHighlight(pos + offset, pos + offset + name.length(), highlight_painter1);
                                pos += name.length();
                                isHighlighted = true;
                            }
                        }
                    }
                }
                for(int i = 0; i < lines.size(); i++)
                {
                    int n = ((Integer)lines.get(i)).intValue(); 
                    highlighter.addHighlight(paragraph.getElement(n).getStartOffset(),
                                             paragraph.getElement(n).getEndOffset() - 1,
                                             highlight_painter2); 
                    isHighlighted = true;                    
                }
                for(int i = 0; i < errors.size(); i++)
                {
                    int n = ((Integer)errors.get(i)).intValue(); 
                    highlighter.addHighlight(paragraph.getElement(n).getStartOffset(),
                                             paragraph.getElement(n).getEndOffset() - 1,
                                             highlight_painter2); 
                    isHighlighted = true;                    
                }
            }
            catch(BadLocationException e) 
            {
                JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);
            }                    
        }
    }
    
    private ArrayList getKeyIntervals(Properties property, String value)
    {
        String key;
        ArrayList<String> ids = new ArrayList<String>();
        Enumeration keys = property.keys();
        while(keys.hasMoreElements())
        {
            key = (String)keys.nextElement();
            if(property.getProperty(key).equals(value)) ids.add(key);
        }
        if(ids.size() == 0) return null;
        Collections.sort(ids, new Comparer());
        String ID = (String)ids.get(0);
        ArrayList<String[]> list = new ArrayList<String[]>();
        String[] interval = new String[2];
        interval[0] = ID;
        int id = Integer.parseInt(ID);
        int next;
        for(int i = 1; i < ids.size(); i++)
        {
            next = Integer.parseInt((String)ids.get(i));
            if(next != id + 1)
            {
                interval[1] = String.valueOf(id);
                list.add(interval);
                interval = new String[2];
                interval[0] = String.valueOf(next);
            }
            id = next;
        }
        interval[1] = String.valueOf(id);
        list.add(interval);
        return list;
    }
    
    private class Comparer implements Comparator<String>
    {
         public int compare(String str1, String str2)
         {
             int i1 = Integer.parseInt(str1);
             int i2 = Integer.parseInt(str2);
             return i1 - i2;
         }
    } 

    /** Clear all records */
    protected void clearAll()
    {
        modelText = pkText = desText = errorText = "";
    }
    
    private DesignTool tool;
    private String modelText = "";
    private String pkText = "";
    private String desText = "";
    private String errorText = "";
    private boolean isHighlighted1 = false;
    private boolean isHighlighted2 = false;
    private boolean isHighlighted3 = false;
    private DefaultHighlighter highlighter1 = new DefaultHighlighter();
    private DefaultHighlighter highlighter2 = new DefaultHighlighter();
    private DefaultHighlighter highlighter3 = new DefaultHighlighter();
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter1 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,255,0));
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter2 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,0,0));
}
