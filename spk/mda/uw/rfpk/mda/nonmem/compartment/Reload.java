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
import java.util.regex.*;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.Utility;

/** This class reloads a model into the model graphical editor.
 *
 * @author  Jiaji Du
 */
public class Reload {
    
    /** Creates a new instance of Record
     * @param tool the DesignTool object.
     */
    public Reload(DesignTool tool)
    {
        this.tool = tool;
        showSteps();
//        testSteps();
    }
    
    private void testSteps()
    {
        testParseDes();
        testParseModel();
        testParsePK();
        testParseError();
        addModels();
    }

    private void showSteps()
    {
        Model.elements.clear();
        Model.fluxes.clear();
        Model.equations = "";
        Model.errorEqns = "";
        Model.variables.clear();
        String text = null;
        if(tool.iterator.getAdvan() == 6 && tool.iterator.initAdvan.contains("model"))
        {
            tool.clickClearButton();
            setAdvan();
            return;
        }
        if(tool.iterator.getIsReload() && isGraphicalModel())
        {
            text = tool.iterator.getReload().getProperty("DES");
            if(text != null)
            {
                parseDes(text.trim().substring(5));
                text = tool.iterator.getReload().getProperty("MODEL");
                parseModel(text.trim().substring(6));
                text = tool.iterator.getReload().getProperty("PK");
                parsePK(text.trim().substring(4));
                text = tool.iterator.getReload().getProperty("ERROR");
                parseError(text.trim().substring(7));
                addModels();
                return;
            }
        }
        text = tool.object.getRecords().getProperty("Des");
        if(!text.equals(""))
        {
            parseDes(text.substring(5));
            text = tool.object.getRecords().getProperty("Model");
            parseModel(text.substring(6));
            text = tool.object.getRecords().getProperty("PK");
            parsePK(text.substring(5));
            text = tool.object.getRecords().getProperty("Error");
            parseError(text.substring(7));
            addModels();
        }
    }

    private void parseModel(String modelText)
    {
        Model model = tool.diagram.model;
        String[] modelLines = modelText.split("\n");
        for(int i = 1; i < modelLines.length; i++)
        {
            if(modelLines[i].indexOf(";") == -1) break;
            String name = "";
            Vector<String> attributes = new Vector<String>();
            int end = -1;
            String comp = modelLines[i].substring(6, modelLines[i].indexOf(")") + 1);
            if(comp.startsWith("\""))
            {
                end = comp.indexOf("\"", 1);
                name = comp.substring(1, end++);
            }
            else if(comp.startsWith("'"))
            {
                end = comp.indexOf("'", 1);
                name = comp.substring(1, end++);
            }
            else
            {
                end = comp.indexOf(" ");
                if(end == -1) end = comp.indexOf(")");
                name = comp.substring(0, end);
            }
            comp = comp.substring(end);                      
            if(comp.indexOf(" INITIALOFF") != -1)
                attributes.add("INITIALOFF");               
            if(comp.indexOf(" NOOFF") != -1)
                attributes.add("NOOFF");       
            if(comp.indexOf(" NODOSE") != -1)
                attributes.add("NODOSE");
            if(comp.indexOf(" EQUILIBRIUM") != -1)
                attributes.add("EQUILIBRIUM");
            if(comp.indexOf(" EXCLUDE") != -1)
                attributes.add("EXCLUDE");
            if(comp.indexOf(" DEFOBSERVATION") != -1)
            {
                attributes.add("DEFOBSERVATION");
                defObsComp = i;
            }
            if(comp.indexOf(" DEFDOSE") != -1)
            {
                attributes.add("DEFDOSE");
                defDoseComp = i;
            }
            String coordinate = modelLines[i].substring(modelLines[i].indexOf(";") + 1);
            int x = Integer.parseInt(coordinate.split(",")[0]);
            int y = Integer.parseInt(coordinate.split(",")[1]);
            if(!isDelay[i - 1])
            {
                Element.Compartment compartment = new Element.Compartment(x, y, model);
                Model.elements.add(compartment);
                compartment.name = name;
                compartment.attributes = attributes;
            }
            else
            {
                Element.Delay delay = new Element.Delay(x, y, model);
                Model.elements.add(delay);
                delay.name = name;          
            }
        }
        if(delayList == null) return;
        Element.Flux flux;
        Element.Delay delay;
        int index;
        for(int i = 0; i < delayList.size(); i++)
        {
             String[] info = (String[])delayList.get(i);
             delay = (Element.Delay)Model.elements.get(Integer.parseInt(info[0]) - 1);
             delay.nDelayComps = Integer.parseInt(info[1]);
             index = Integer.parseInt(info[2]) - 1;
             flux = new Element.Flux(delay, (Element)Model.elements.get(index), model);
             delay.fractions.setElementAt(info[3], delay.fractions.size() - 1); 
             Model.fluxes.add(flux);
        }
    }

    private void parsePK(String pkText)
    {
        Model model = tool.diagram.model;    
        for(int i = 0; i < Model.elements.size(); i++)
            if((Element)Model.elements.get(i) instanceof Element.Compartment)
                ((Element.Compartment)Model.elements.get(i)).parameters.clear();
        String[] pkLines = pkText.split("\n");        
        for(int i = 0; i < pkLines.length; i++)
        {
            String[] sides = pkLines[i].split("=");
            if(sides[0].matches("TLAG\\d+"))
            {
//                System.out.println("TLAG:  " + pkLines[i]);                
                ((Element.Delay)Model.elements.get(Integer.parseInt(sides[0].substring(4)) - 1))
                .delayTime = sides[1];
            }
            else if(sides[0].matches("[S|F|R|D]\\d+"))
            {
//                System.out.println("Comp param:  " + pkLines[i]);
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(1)) - 1))
                .parameters.setProperty(sides[0], sides[1]);
            }
            else if(sides[0].matches("ALAG\\d+"))
            {
//                System.out.println("Comp param:  " + pkLines[i]);
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(4)) - 1))
                .parameters.setProperty(sides[0], sides[1]);
            }
            else if(sides[0].matches("FF\\d+"))
            {
//                System.out.println("Force:  " + pkLines[i]);
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(2)) - 1))
                .force = sides[1];
            }
            else if(sides[0].matches("K\\d+") || sides[0].matches("K\\d+T\\d+"))
            {
//                System.out.println("Fluxes:  " + pkLines[i]);
                int element1, element2;
                String left = sides[0];
                if(sides[0].indexOf("T") == -1)
                {
                    element1 = Integer.parseInt(left.substring(1, 2));
                    element2 = Integer.parseInt(left.substring(2));
                }
                else
                {
                    element1 = Integer.parseInt(left.substring(1, left.indexOf("T")));
                    element2 = Integer.parseInt(left.substring(left.indexOf("T") + 1));
                }
                Element.Flux flux;
                if(element2 == 0)
                    flux = new Element.Flux((Element)Model.elements.get(element1 - 1), null, model);
//                else if(element1 == 0)         // Remove the feature of constant input.
//                    flux = new Element.Flux(null, (Element)Model.elements.get(element2 - 1), model);
                else
                    flux = new Element.Flux((Element)Model.elements.get(element1 - 1), 
                                            (Element)Model.elements.get(element2 - 1), model);
                flux.flowRate = sides[1];
                Model.fluxes.add(flux);
            }
            else if(Pattern.compile("\\bTHETA\\(\\d+\\)", Pattern.UNIX_LINES).matcher(sides[1]).find())
            {
//                System.out.println("Variables:  " + pkLines[i]);
                Model.variables.setProperty(sides[0], sides[1]);
            }
            else
            {
//                System.out.println("Equations:  " + pkLines[i]);
                if(!Model.equations.equals(""))
                    Model.equations += " ";
                Model.equations += pkLines[i];
            }
        }
    }

    private void parseError(String errorText)
    {
        subjectModel = new Properties();
        compErrorModels = new Vector<String[]>();
        int index1 = errorText.indexOf("IF(ID ");
        if(index1 != -1)
        {
            if(index1 != 0) Model.errorEqns = errorText.substring(0, index1).trim();
            int index2 = errorText.indexOf("\nENDIF\nIF(ID ");
//            int index2 = errorText.indexOf("\nELSE IF(ID ");
            int index3;  
            int i = 1;
            String code;
            while(index1 != -1 && index2 != -1)
            {
                code = errorText.substring(index1, index2);
            
                // Get subjectModel from IF(ID)
                String[] idCode = code.substring(0, code.indexOf(" THEN\n") - 1).split(" OR ");
                for(int j = 0; j < idCode.length; j++)
                {
                    if(idCode[j].indexOf(" .EQ. ") != -1)
                        subjectModel.setProperty(idCode[j].split(" .EQ. ")[1], String.valueOf(i));
                    else
                    {
                        idCode[j] = idCode[j].substring(1, idCode[j].length() - 1);
                        int ge = Integer.parseInt(idCode[j].split(" AND")[0].split(" .GE. ")[1]);
                        int le = Integer.parseInt(idCode[j].split(" AND")[1].split(" .LE. ")[1]);
                        for(int k = ge; k <= le; k++)
                            subjectModel.setProperty(String.valueOf(k), String.valueOf(i));
                    }
                }
            
                // Get compErrorModels from IF(CMP)
                parseCmp(code);
            
                index1 = index2;
                index2 = errorText.indexOf("\nENIF\nIF(ID ", index1 + 5);
//                index2 = errorText.indexOf("\nELSE IF(ID ", index1 + 5);
                if(index2 == -1) index2 = errorText.indexOf("\nENDIF", index1 + 5);
                i++;
            }
            tool.subjectModel = subjectModel;
        }
        else
        {
            int indexY = errorText.indexOf("Y=");
            if(indexY == -1) return;
            if(indexY != 0)
            {
                Model.errorEqns = errorText.substring(0, indexY).trim();
                errorText = errorText.substring(indexY);
            }
            parseCmp(errorText);
        }
        
        // Get model, input and sample information
        String[] lines = errorText.substring(errorText.indexOf(";")).split("\n");
        modelNames = new String[lines.length];
        inputInfo = new String[lines.length][];
        sampleInfo = new String[lines.length][];       
        for(int i = 0; i < lines.length; i++)
        {
            String[] tokens = lines[i].substring(1).split(";");
            modelNames[i] = tokens[0];
            int n = 0;
            for(int j = 1; j < tokens.length; j++)
                if(tokens[j].endsWith(")"))
                    n++;
            inputInfo[i] = new String[n];
            sampleInfo[i] = new String[tokens.length - 1 - n];
            for(int j = 0; j < n; j++)
                inputInfo[i][j] = tokens[j + 1];
            for(int j = n; j < tokens.length - 1; j++)
                sampleInfo[i][j - n] = tokens[j + 1];
        }
    }
    
    private void parseCmp(String code)
    {
        String n, model;
        String[] comps;
        int index3 = code.indexOf("IF(CMP ");           
        if(index3 != -1)
        {
            code = code.substring(index3);
            if(code.indexOf("\n;") != -1)
            code = code.substring(0, code.indexOf("\n;"));
            String[] lines = code.split("\n");
            comps = new String[lines.length/3];
            int k = 0;
            for(int j = 0; j < lines.length - 1; j++)
            {
                String line = lines[j];
                int i1 = lines[j].indexOf(".EQ.") + 5;
                int i2 = lines[j].indexOf(")");
                n = lines[j].substring(lines[j].indexOf(".EQ.") + 5, lines[j].indexOf(")"));
                model = lines[++j].trim();
                comps[k++] = n + ":" + model;
                j++;
            }
        }
        else
        {
            model = code.substring(code.indexOf("Y="));
            if(model.indexOf("\n") != -1)
                model = model.substring(0, model.indexOf("\n"));
            comps = new String[]{defObsComp + ":" + model};
        }
        compErrorModels.add(comps);
    }

    private void parseDes(String desText)
    {
        String[] desLines = desText.split("\n");
        String regExp = "[+](\\d)+/TLAG(\\d+)[*](\\S+)[*]A[(]\\d+[)]";
        Pattern pattern = Pattern.compile(regExp, Pattern.UNIX_LINES);
        isDelay = new boolean[desLines.length];
        for(int i = 0; i < isDelay.length; i++)
            isDelay[i] = false;
        delayList = new Vector<String[]>();
        for(int i = 0; i < desLines.length; i++)
        {           
            Matcher matcher = pattern.matcher(desLines[i]);
            while(matcher.find())
            {
                delayList.add(new String[]{matcher.group(2), matcher.group(1),
                                           String.valueOf(i + 1), matcher.group(3)});
                isDelay[Integer.parseInt(matcher.group(2)) - 1] = true;
            }
        }
    }
    
    private void addModels()
    {
        Model model = tool.diagram.model;
        Vector<Element> inputElements;
        for(int i = 0; i < compErrorModels.size(); i++)
        {
            if(i != 0) tool.clickAddButton();
            String[] comps = (String[])compErrorModels.get(i);
            for(int j = 0; j < comps.length; j++)
            {
                int index = Integer.parseInt(comps[j].split(":")[0]) - 1;
                Element element = (Element)Model.elements.get(index);
                inputElements = new Vector<Element>();
                inputElements.add(element);
                int x = Integer.parseInt(sampleInfo[i][j].split(":")[1].split(",")[0]);
                int y = Integer.parseInt(sampleInfo[i][j].split(":")[1].split(",")[1]);
                Element.Sample sample = new Element.Sample(inputElements, x, y, model);
                sample.name = sampleInfo[i][j].split(":")[0];
                model.samples.add(sample);
                sample.errorModel = comps[j].split(":")[1];
            }
            for(int j = 0; j < inputInfo[i].length; j++)
            {
                String coordinate = inputInfo[i][j].split(":")[1].split("[(]")[0];
                int index = Integer.parseInt(inputInfo[i][j].split("[(]")[1].split("[)]")[0]) - 1;
                Element element = (Element)Model.elements.get(index);
                inputElements = new Vector<Element>();
                inputElements.add(element);
                int x = Integer.parseInt(coordinate.split(",")[0]);
                int y = Integer.parseInt(coordinate.split(",")[1]);
                Element.Input input = new Element.Input(inputElements, x, y, model);
                input.name = inputInfo[i][j].split(":")[0];;
                model.inputs.add(input);
            }
            tool.saveModel();
            ((Model)tool.models.get(tool.models.size() - 1)).name = modelNames[i];
        }
        tool.diagram.repaint();
        tool.setRecords();
    }

    /** Main method.
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        Reload reload = new Reload(new DesignTool());
        System.exit(0);
    }
    
    // Test parseError
    private void testParseError()
    {
        String errorText = "Error Equation = 1\n" +
                           "Error Equation = 2\n" +
                           "IF(ID .EQ. 1 OR (ID .GE. 3 AND .LE. 10)) THEN\n" +
                           "  IF(CMP .EQ. 1) THEN\n" +
                           "    Y=F+EPS(1)\n" +
                           "  ELSE IF(CMP .EQ. 2) THEN\n" +
                           "    Y=F+F*EPS(2)\n" +
                           "  END IF\n" +
                           "ELSE IF(ID .EQ. 2) THEN\n" +
                           "  Y=F+EPS(3)\n" +
                           "END IF\n" +
                           ";Group-1;Dose1:92,253(1);Obs1:147,129;Obs2:300,130\n" +
                           ";Group-2;Dose2:74,202(2);Obs3:128,111";
/*
        errorText = "IF(CMP .EQ. 1) THEN\n" +
                    "  Y=F+EPS(1)\n" +
                    "ELSE IF(CMP .EQ. 2) THEN\n" +
                    "  Y=F+F*EPS(2)\n" +
                    "END IF\n" +
                    ";Model1";

        errorText = "Y=F+EPS(1)\n" +
                    ";Model1";
*/
        parseError(errorText);
        Enumeration keys = subjectModel.keys();
        while(keys.hasMoreElements())
        {
            String key = (String)keys.nextElement();
            System.out.println(key + ":  " + subjectModel.getProperty(key));
        }
        for(int i = 0; i < compErrorModels.size(); i++)
        {
            String[] comps = (String[])compErrorModels.get(i);
            for(int j = 0; j < comps.length; j++) 
                System.out.println(comps[j]);
        }
        for(int i = 0; i < modelNames.length; i++)
            System.out.println(modelNames[i]);
        for(int i = 0; i < inputInfo.length; i++)
            for(int j = 0; j < inputInfo[i].length; j++)
                System.out.println(inputInfo[i][j]);
        for(int i = 0; i < sampleInfo.length; i++)
            for(int j = 0; j < sampleInfo[i].length; j++)
                System.out.println(sampleInfo[i][j]);
    }
    
    private void testParseModel()
    {
        String modelText = "NCOMPARTMENTS=6\n" +
                           "COMP=(A1 DEFDOSE)   ;123,152\n" +
                           "COMP=(A2 DEFOBSERVATION)   ;247,166\n" +
                           "COMP=(D3)   ;105,254\n" +
                           "COMP=(A4)   ;244,258\n" +
                           "COMP=(D3)\n" +
                           "COMP=(D3)";
        parseModel(modelText);
    }

    private void testParsePK()
    {
        String pkText = "TLAG3=100\nS1=1\nF2=2\nALAG3=THETA(1)\nFF1=DV\nCL=THETA(2)\n" +
                        "K12=THETA(3)+ETA(3)\nK12T3=THETA(4)+ETA(4)\nK12T34=THETA(5)+ETA(5)\n" +
                        "Equation1=A\nEquation2=B";
        pkText = "V=THETA(5)+ETA(5)\n" +
                 "CL=THETA(6)+THETA(6)*ETA(6)\n" +
                 "S1=1\n" +
                 "F2=THETA(4)+ETA(4)\n" +
                 "TLAG3=123\n" +
                 "FF2=force\n" +
                 "R4=CL/V\n" +
                 "K12=THETA(2)+THETA(2)*ETA(2)\n" +
                 "K13=THETA(1)+ETA(1)\n" +
                 "K23=THETA(3)+EXP(ETA(3))";
        parsePK(pkText);
    }
    
    private void testParseDes()
    {
        String desText = "DADT(1)=-K12*A(1)-K13*A(1)\n" +
                         "DADT(2)=+K12*A(1)+3/TLAG3*1/2*A(6)-K23*FF2\n" +
                         "DADT(3)=+K13*A(1)+K23*FF2-3/TLAG3*A(3)   ;D3\n" +
                         "DADT(4)=+3/TLAG3*1/2*A(6)\n" +
                         "DADT(5)=3/TLAG3*(A(3)-A(5))   ;D3\n" +
                         "DADT(6)=3/TLAG3*(A(5)-A(6))   ;D3";
        parseDes(desText);
        for(int i = 0; i < delayList.size(); i++)
        {
            String[] info = (String[])delayList.get(i);
            System.out.println(info[0] + " " + info[1] + " " + info[2] + " " + info[3]);
        }
    }

    private void setAdvan()
    {
        Model model = tool.diagram.model;
        int adn = tool.iterator.adn;
        int trn = tool.iterator.trn;
        Element.Input input = null;
        Element.Sample sample = null;
        switch(adn)
        {
            case 1:
                Element.Compartment comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "CENTRAL";
                comp1.attributes.add("DEFDOSE");
                comp1.attributes.add("DEFOBSERVATION");
                comp1.attributes.add("NOOFF");
                Model.elements.add(comp1);
                Element.Flux flux10 = new Element.Flux(comp1, null, model);
                Model.fluxes.add(flux10);
                Vector<Element.Compartment> comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {                
                    case 1:
                        break;
                    case 2:
                        flux10.flowRate = "CL/V";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V", "");
                }
                break;
            case 2:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "DEPOT";
                comp1.attributes.add("INITIALOFF");
                comp1.attributes.add("DEFDOSE");
                Model.elements.add(comp1);
                Element.Compartment comp2 = new Element.Compartment(250, 100, model);
                comp2.name = "CENTRAL";
                comp2.attributes.add("DEFOBSERVATION");
                comp2.attributes.add("NOOFF");
                Model.elements.add(comp2);
                Element.Flux flux12 = new Element.Flux(comp1, comp2, model);
                Model.fluxes.add(flux12);
                Element.Flux flux20 = new Element.Flux(comp2, null, model);
                Model.fluxes.add(flux20);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);     
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp2);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {
                    case 1:
                        break;
                    case 2:
                        flux20.flowRate = "CL/V";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V", "");
                }
                break;
            case 3:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "CENTRAL";
                comp1.attributes.add("DEFOBSERVATION");
                comp1.attributes.add("DEFDOSE");
                comp1.attributes.add("NOOFF");
                Model.elements.add(comp1);
                comp2 = new Element.Compartment(250, 100, model);
                comp2.name = "PERIPH";
                comp2.attributes.add("INITIALOFF");
                Model.elements.add(comp2);
                Element.Flux flux21 = new Element.Flux(comp2, comp1, model);
                Model.fluxes.add(flux21);
                flux10 = new Element.Flux(comp1, null, model);
                Model.fluxes.add(flux10);
                flux12 = new Element.Flux(comp1, comp2, model);
                Model.fluxes.add(flux12);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {                
                    case 1:
                        break;
                    case 3:
                        flux10.flowRate = "CL/V";
                        flux12.flowRate = "Q/V";
                        flux21.flowRate = "Q/(VSS-V)";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V", "");
                        Model.variables.setProperty("Q", "");
                        Model.variables.setProperty("VSS", "");                        
                        break;
                    case 4:
                        flux10.flowRate = "CL/V1";
                        flux12.flowRate = "Q/V1";
                        flux21.flowRate = "Q/V2";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V1", "");
                        Model.variables.setProperty("Q", "");
                        Model.variables.setProperty("V2", "");
                        break;
                    case 5:
                        flux21.flowRate = "(AOB*BETA+ALPHA)/(AOB+1)";
                        flux10.flowRate = "ALPHA*BETA/K21";
                        flux12.flowRate = "ALPHA+BETA-K21-K10";                        
                        Model.variables.setProperty("AOB", "");
                        Model.variables.setProperty("ALPHA", "");
                        Model.variables.setProperty("BETA", "");    
                }
                break;
            case 4:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "DEPOT";
                comp1.attributes.add("INITIALOFF");
                comp1.attributes.add("DEFDOSE");
                Model.elements.add(comp1);
                comp2 = new Element.Compartment(250, 100, model);
                comp2.name = "CENTRAL";
                comp2.attributes.add("DEFOBSERVATION");
                comp2.attributes.add("NOOFF");
                Model.elements.add(comp2);
                Element.Compartment comp3 = new Element.Compartment(400, 100, model);
                comp3.name = "PERIPH";
                comp3.attributes.add("NOOFF");
                Model.elements.add(comp3);
                Element.Flux flux32 = new Element.Flux(comp3, comp2, model);
                Model.fluxes.add(flux32);
                flux20 = new Element.Flux(comp2, null, model);
                Model.fluxes.add(flux20);
                Element.Flux flux23 = new Element.Flux(comp2, comp3, model);
                Model.fluxes.add(flux23);
                flux12 = new Element.Flux(comp1, comp2, model);
                Model.fluxes.add(flux12);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);     
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp2);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {
                    case 1:
                        break;
                    case 3:
                        flux20.flowRate = "CL/V";
                        flux23.flowRate = "Q/V";
                        flux32.flowRate = "Q/(VSS-V)";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V", "");
                        Model.variables.setProperty("Q", "");
                        Model.variables.setProperty("VSS", "");
                        break;
                    case 4:
                        flux12.flowRate = "CL/V2";
                        flux23.flowRate = "Q/V2";
                        flux32.flowRate = "Q/V3";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V2", "");
                        Model.variables.setProperty("Q", "");
                        Model.variables.setProperty("V3", "");
                        break;
                    case 5:
                        flux32.flowRate = "(AOB*BETA+ALPHA)/(AOB+1)";
                        flux20.flowRate = "ALPHA*BETA/K32";
                        flux23.flowRate = "ALPHA+BETA-K32-K20";                        
                        Model.variables.setProperty("AOB", "");
                        Model.variables.setProperty("ALPHA", "");
                        Model.variables.setProperty("BETA", ""); 
                }
                break;
            case 10:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "CENTRAL";
                comp1.attributes.add("DEFDOSE");
                comp1.attributes.add("DEFOBSERVATION");
                comp1.attributes.add("NOOFF");
                Model.elements.add(comp1);
                flux10 = new Element.Flux(comp1, null, model);
                Model.fluxes.add(flux10);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {
                    case 1:
                        flux10.flowRate = "VM/(KM+A(1))";
                        Model.variables.setProperty("VM", "");
                        Model.variables.setProperty("KM", "");
                }
                break;
            case 11:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "CENTRAL";
                comp1.attributes.add("DEFOBSERVATION");
                comp1.attributes.add("DEFDOSE");
                comp1.attributes.add("NOOFF");
                Model.elements.add(comp1);
                comp2 = new Element.Compartment(250, 100, model);
                comp2.name = "PERIPH1";
                comp2.attributes.add("NOOFF");
                Model.elements.add(comp2);
                comp3 = new Element.Compartment(250, 250, model);
                comp3.name = "PERIPH2";
                comp3.attributes.add("NOOFF");
                Model.elements.add(comp3);
                flux10 = new Element.Flux(comp1, null, model);
                Model.fluxes.add(flux10);
                flux21 = new Element.Flux(comp2, comp1, model);
                Model.fluxes.add(flux21);
                Element.Flux flux31 = new Element.Flux(comp3, comp1, model);
                Model.fluxes.add(flux31);
                flux12 = new Element.Flux(comp1, comp2, model);
                Model.fluxes.add(flux12);
                Element.Flux flux13 = new Element.Flux(comp1, comp3, model);
                Model.fluxes.add(flux13);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {
                    case 1:
                        break;
                    case 4:
                        flux10.flowRate = "CL/V1";
                        flux12.flowRate = "Q2/V1";
                        flux21.flowRate = "Q2/V2";
                        flux13.flowRate = "Q3/V1";
                        flux31.flowRate = "Q3/V2";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V1", "");
                        Model.variables.setProperty("Q2", "");
                        Model.variables.setProperty("V2", "");
                        Model.variables.setProperty("Q3", "");
                        Model.variables.setProperty("V3", "");
                        break;
                    case 6:
                        flux10.flowRate = "ALPHA*BETA*GAMMA/(K21*K31)";
                        flux13.flowRate = "(P+K31*K31-K31*S-K10*K21)/(K21-K31)";
                        flux12.flowRate = "S-K10-K13-K21-K31";
                        Model.variables.setProperty("ALPHA", "");
                        Model.variables.setProperty("BETA", "");
                        Model.variables.setProperty("GAMMA", "");
                        Model.equations = "S=ALPHA+BETA+GAMMA\nP=ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA";
                }
                break;
            case 12:
                comp1 = new Element.Compartment(100, 100, model);
                comp1.name = "DEPOT";
                comp1.attributes.add("INITIALOFF");
                comp1.attributes.add("DEFDOSE");
                Model.elements.add(comp1);
                comp2 = new Element.Compartment(250, 100, model);
                comp2.name = "CENTRAL";
                comp2.attributes.add("DEFOBSERVATION");
                comp2.attributes.add("NOOFF");
                Model.elements.add(comp2);
                comp3 = new Element.Compartment(400, 100, model);
                comp3.name = "PERIPH1";
                comp3.attributes.add("NOOFF");
                Model.elements.add(comp3);
                Element.Compartment comp4 = new Element.Compartment(400, 250, model);
                comp4.name = "PERIPH2";
                comp4.attributes.add("NOOFF");
                Model.elements.add(comp4);
                flux12 = new Element.Flux(comp1, comp2, model);
                Model.fluxes.add(flux12);
                flux32 = new Element.Flux(comp3, comp2, model);
                Model.fluxes.add(flux32);
                Element.Flux flux42 = new Element.Flux(comp4, comp2, model);
                Model.fluxes.add(flux42);
                flux20 = new Element.Flux(comp2, null, model);
                Model.fluxes.add(flux20);
                Element.Flux flux24 = new Element.Flux(comp2, comp4, model);
                Model.fluxes.add(flux24);
                flux23 = new Element.Flux(comp2, comp3, model);
                Model.fluxes.add(flux23);
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp1);
                input = new Element.Input(comps, 0, 0, model);     
                comps = new Vector<Element.Compartment>(1);
                comps.add(comp2);
                sample = new Element.Sample(comps, 0, 0, model);
                switch(trn)
                {
                    case 1:
                        break;
                    case 4:
                        flux20.flowRate = "CL/V2";
                        flux23.flowRate = "Q3/V2";
                        flux32.flowRate = "Q3/V3";
                        flux24.flowRate = "Q4/V2";
                        flux42.flowRate = "Q4/V4";
                        Model.variables.setProperty("CL", "");
                        Model.variables.setProperty("V2", "");
                        Model.variables.setProperty("Q3", "");
                        Model.variables.setProperty("V3", "");
                        Model.variables.setProperty("Q4", "");
                        Model.variables.setProperty("V4", "");
                        break;
                    case 6:
                        flux20.flowRate = "ALPHA*BETA*GAMMA/(K32*K42)";
                        flux24.flowRate = "(P+K42*K42-K42*S-K20*K32)/(K32-K42)";
                        flux23.flowRate = "S-K20-K24-K32-K42";
                        Model.variables.setProperty("ALPHA", "");
                        Model.variables.setProperty("BETA", "");
                        Model.variables.setProperty("GAMMA", "");
                        Model.equations = "S=ALPHA+BETA+GAMMA\nP=ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA";
                }
        }
        if(!tool.iterator.getIsInd() && !tool.iterator.getIsTwoStage() && !tool.iterator.isNonparam)
            sample.errorModel = "Y=F+EPS(1)";
        else
            sample.errorModel = "Y=F+ETA(1)";
        model.inputs.add(input);
        model.samples.add(sample);
        tool.saveModel();
        tool.diagram.repaint();
    }
    
    // Find if reload graphical model
    private boolean isGraphicalModel()
    {
        if(tool.iterator.getReload() != null && tool.iterator.getReload().getProperty("MODEL") != null)
        {
            String text = tool.iterator.getReload().getProperty("MODEL").substring(6);
            return text.split("\n")[1].matches("COMP=[(].+[)]\\s+;\\d+,\\d+");
        }
        return false;
    }
    
    private DesignTool tool;          // DesignTool object for connection
    private int defDoseComp = 0;      // default dose number
    private int defObsComp = 0;       // default observation number
    private Vector<String[]> delayList;         // String[]s: [delay number,nDelayComps,to-comp number,to-comp fraction]s
    private boolean[] isDelay;        // Is the element a delay
    private Properties subjectModel;  // key: subject ID, value: model/group id 
    private Vector<String[]> compErrorModels;   // String[]s: [[comp number:error model]s for each model/group]s
    private String[] modelNames;      // model/group names
    private String[][] inputInfo;     // [[dose name:x,y(comp number)]s for each model/group]s
    private String[][] sampleInfo;    // [[observation name:x,y]s for each model/group]s
}
