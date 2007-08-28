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
        Model.errorEqns = "";
        Model.parameterList.clear();
        String text = null;
        if(tool.iterator.getAdvan() == 6 && tool.iterator.initAdvan.contains("model"))
        {
            tool.clear();
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
        int j = 0;
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
                compartment.timeRate = timeRates.get(j++);
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
        String[] pkLines = pkText.replaceAll(" THEN\n", " THEN").split("\n");
        String helperEqn = "";
        String condition = "";
        boolean isIF = false;
        for(int i = 0; i < pkLines.length; i++)
        {
            if(pkLines[i].endsWith(";"))
            {
                helperEqn += pkLines[i].replace(';', '\n');
                continue;
            }
            if(pkLines[i].startsWith("IF(") || pkLines[i].equals("ELSE"))
            {
                isIF = true;
                condition += pkLines[i].replaceAll(" THEN", " THEN\n") + "\n";
                continue;
            }
            if(pkLines[i].equals("ENDIF")) continue;
            String[] sides = pkLines[i].split("=");
            if(sides[0].matches("TLAG\\d+"))
            {
                System.out.println("TLAG:  " + helperEqn + pkLines[i]);
                Element.Delay delay = (Element.Delay)Model.elements.get(Integer.parseInt(sides[0].substring(4)) - 1);
                Model.parameterList.remove(delay.delayTime);
                if(!isIF)
                    delay.delayTime = new Parameter(sides[0], helperEqn + pkLines[i]);
                else
                    delay.delayTime = new Parameter(sides[0], helperEqn + condition + pkLines[i] + "\nENDIF");
                Model.parameterList.add(delay.delayTime);
                tool.updateParameterList(delay.delayTime, false);
            }
            else if(sides[0].matches("[S|F|R|D]\\d+"))
            {
                System.out.println("Comp param:  " + helperEqn + pkLines[i]);
                Parameter parameter;
                if(!isIF)
                    parameter = new Parameter(sides[0], helperEqn + pkLines[i]);
                else
                    parameter = new Parameter(sides[0], helperEqn + condition + pkLines[i] + "\nENDIF");
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(1)) - 1))
                .parameters.put(sides[0], parameter);
                Model.parameterList.add(parameter);
                tool.updateParameterList(parameter, false);
            }
            else if(sides[0].matches("ALAG\\d+"))
            {
                System.out.println("Comp param:  " + helperEqn + pkLines[i]);
                Parameter parameter;
                if(!isIF)
                    parameter = new Parameter(sides[0], helperEqn + pkLines[i]);
                else
                    parameter = new Parameter(sides[0], helperEqn + condition + pkLines[i] + "\nENDIF");
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(4)) - 1))
                .parameters.put(sides[0], parameter);
                Model.parameterList.add(parameter);
                tool.updateParameterList(parameter, false);
            }
            else if(sides[0].matches("FF\\d+"))
            {
                System.out.println("Force:  " + pkLines[i]);
                Parameter parameter = new Parameter(sides[0], pkLines[i]);
                ((Element.Compartment)Model.elements.get(Integer.parseInt(sides[0].substring(2)) - 1))
                .force = parameter;
                Model.parameterList.add(parameter);
                tool.updateParameterList(parameter, false);
            }
            else if(sides[0].matches("K\\d+") || sides[0].matches("K\\d+T\\d+"))
            {
                System.out.println("Fluxes:  " + helperEqn + pkLines[i]);
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
                if(!isIF)
                    flux.flowRate.value = helperEqn + pkLines[i];
                else
                    flux.flowRate.value = helperEqn + condition + pkLines[i] + "\nENDIF";
                tool.updateParameterList(flux.flowRate, false);
                Model.fluxes.add(flux);
            }
//            else if(Pattern.compile("\\bTHETA\\(\\d+\\)", Pattern.UNIX_LINES).matcher(sides[1]).find())
            else
            {
                System.out.println("Variables:  " + helperEqn + pkLines[i]);
                Variable variable;
                if(!isIF)
                    variable = new Variable(sides[0], helperEqn + pkLines[i]);
                else
                    variable = new Variable(sides[0], helperEqn + condition + pkLines[i] + "\nENDIF");
                variable.refCount = 0;
                Model.parameterList.add(variable);
                tool.updateParameterList(variable, false);
            }
/*            else
            {
                System.out.println("Equations:  " + pkLines[i]);
                Parameter parameter = new Parameter(sides[0], pkLines[i]);
                Model.equationList.add(parameter);
                Model.parameterList.add(parameter);
                tool.updateParameterList(parameter, false);
            }*/
            helperEqn = "";
            condition = "";
            isIF = false;
        }
    }

    private void parseError(String errorText)
    {
        // Find error equations
        String[] rows = errorText.split("\n");
        Model.errorEqns = "";
        errorText = "";
        for(String row : rows)
        {
            if(row.endsWith(";"))
                Model.errorEqns += row + "\n";
            else
                errorText += row + "\n";
        }
        Model.errorEqns = Model.errorEqns.replaceAll(";\n", "\n").trim();
        subjectModel = new Properties();
        compErrorModels = new Vector<String[]>();
        int indexY = errorText.indexOf("Y=");
        if(indexY == -1) return;
        int index1 = errorText.indexOf("IF(ID ");
        if(index1 != -1)
        {
            int index2 = errorText.indexOf("\nENDIF\nIF(ID ");
            int index3;  
            int i = 1;
            String code;
            while(index1 != -1 && index2 != -1)
            {
                code = errorText.substring(index1, index2);
            
                // Get subjectModel from IF(ID)
                String[] idCode = code.substring(0, code.indexOf(" THEN\n") - 1).split(" .OR. ");
                for(int j = 0; j < idCode.length; j++)
                {
                    if(idCode[j].indexOf(" .EQ. ") != -1)
                        subjectModel.setProperty(idCode[j].split(" .EQ. ")[1], String.valueOf(i));
                    else
                    {
                        idCode[j] = idCode[j].substring(1, idCode[j].length() - 1);
                        int ge = Integer.parseInt(idCode[j].split(" .AND. ")[0].split(" .GE. ")[1]);
                        int le = Integer.parseInt(idCode[j].split(" .AND. ")[1].split(" .LE. ")[1]);
                        for(int k = ge; k <= le; k++)
                            subjectModel.setProperty(String.valueOf(k), String.valueOf(i));
                    }
                }
            
                // Get compErrorModels from IF(CMT)
                parseCmp(code);
            
                index1 = index2;
                index2 = errorText.indexOf("\nENDIF\nIF(ID ", index1 + 5);
                if(index2 == -1) index2 = errorText.indexOf("\nENDIF", index1 + 5);
                i++;
            }
            tool.subjectModel = subjectModel;
        }
        else
        {
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
        int index3 = code.indexOf("IF(CMT ");           
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
        // Find equations
        int indexDE = desText.indexOf("DADT");
        Model.desEqns = desText.substring(0, indexDE).trim();
        desText = desText.substring(indexDE);
        
        // Find apended time rate of concentration
        String[] desLines = desText.trim().split("\n");
        timeRates = new Vector<String>();
        for(int i = 0; i < desLines.length; i++)
        {
            if(desLines[i].indexOf(";") == -1)
            {
                int indexSpace = desLines[i].indexOf(" ");
                if(indexSpace != -1)
                {
                    timeRates.add(desLines[i].substring(indexSpace).trim());
                    desLines[i] = desLines[i].substring(0, indexSpace);
                }
                else
                {
                    timeRates.add("");
                }
            }
        }
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
                String fraction = matcher.group(3);
                if(fraction.indexOf("*A(") != -1)
                    fraction = fraction.substring(0, fraction.indexOf("*A("));
                delayList.add(new String[]{matcher.group(2), matcher.group(1),
                                           String.valueOf(i + 1), fraction});
                isDelay[Integer.parseInt(matcher.group(2)) - 1] = true;
            }
        }
        for(String[] info : delayList)
            System.out.println(info[0] + " " + info[1] + " " + info[2] + " " + info[3]);
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
                           "  IF(CMT .EQ. 1) THEN\n" +
                           "    Y=F+EPS(1)\n" +
                           "  ELSE IF(CMT .EQ. 2) THEN\n" +
                           "    Y=F+F*EPS(2)\n" +
                           "  END IF\n" +
                           "ELSE IF(ID .EQ. 2) THEN\n" +
                           "  Y=F+EPS(3)\n" +
                           "END IF\n" +
                           ";Group-1;Dose1:92,253(1);Obs1:147,129;Obs2:300,130\n" +
                           ";Group-2;Dose2:74,202(2);Obs3:128,111";
/*
        errorText = "IF(CMT .EQ. 1) THEN\n" +
                    "  Y=F+EPS(1)\n" +
                    "ELSE IF(CMT .EQ. 2) THEN\n" +
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
        Model.elements.clear();
        Model.fluxes.clear();
        Model.parameterList.clear();
        Model.errorEqns = "";
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
                        flux10.flowRate.value = "K10=CL/V";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v = new Variable("V", "V=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v);
                        Model.parameterList.add(flux10.flowRate);
                        flux10.flowRate.dependVariables.add(cl);
                        flux10.flowRate.dependVariables.add(v);
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
                        flux20.flowRate.value = "K20=CL/V";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v = new Variable("V", "V=");
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v);
                        Model.parameterList.add(flux20.flowRate);
                        flux20.flowRate.dependVariables.add(cl);
                        flux20.flowRate.dependVariables.add(v);
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
                        flux10.flowRate.value = "K10=CL/V";
                        flux12.flowRate.value = "K12=Q/V";
                        flux21.flowRate.value = "K21=Q/(VSS-V)";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v = new Variable("V", "V=");
                        Variable q = new Variable("Q", "Q=");
                        Variable vss = new Variable("VSS", "VSS=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.remove(flux12.flowRate);
                        Model.parameterList.remove(flux21.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v);
                        Model.parameterList.add(q);
                        Model.parameterList.add(vss);
                        Model.parameterList.add(flux10.flowRate);
                        Model.parameterList.add(flux12.flowRate);
                        Model.parameterList.add(flux21.flowRate);
                        flux10.flowRate.dependVariables.add(cl);
                        flux10.flowRate.dependVariables.add(v);
                        flux12.flowRate.dependVariables.add(q);
                        flux12.flowRate.dependVariables.add(v);
                        flux21.flowRate.dependVariables.add(q);
                        flux21.flowRate.dependVariables.add(v);
                        flux21.flowRate.dependVariables.add(vss);
                        v.refCount = 3;
                        q.refCount = 2;
                        break;
                    case 4:
                        flux10.flowRate.value = "K10=CL/V1";
                        flux12.flowRate.value = "K12=Q/V1";
                        flux21.flowRate.value = "K21=Q/V2";
                        cl = new Variable("CL", "CL=");
                        Variable v1 = new Variable("V1", "V1=");
                        q = new Variable("Q", "Q=");
                        Variable v2 = new Variable("V2", "V2=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.remove(flux12.flowRate);
                        Model.parameterList.remove(flux21.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v1);
                        Model.parameterList.add(q);
                        Model.parameterList.add(v2);
                        Model.parameterList.add(flux10.flowRate);
                        Model.parameterList.add(flux12.flowRate);
                        Model.parameterList.add(flux21.flowRate);
                        flux10.flowRate.dependVariables.add(cl);
                        flux10.flowRate.dependVariables.add(v1);
                        flux12.flowRate.dependVariables.add(q);
                        flux12.flowRate.dependVariables.add(v1);
                        flux21.flowRate.dependVariables.add(q);
                        flux21.flowRate.dependVariables.add(v2);
                        v1.refCount = q.refCount = 2;
                        break;
                    case 5:
                        flux21.flowRate.value = "K21=(AOB*BETA+ALPHA)/(AOB+1)";
                        flux10.flowRate.value = "K10=ALPHA*BETA/K21";
                        flux12.flowRate.value = "K12=ALPHA+BETA-K21-K10";
                        Variable aob = new Variable("AOB", "AOB=");
                        Variable alpha = new Variable("ALPHA", "ALPHA=");
                        Variable beta = new Variable("BETA", "BETA=");
                        Model.parameterList.remove(flux21.flowRate);
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.remove(flux12.flowRate);
                        Model.parameterList.add(aob);
                        Model.parameterList.add(alpha);
                        Model.parameterList.add(beta);
                        Model.parameterList.add(flux21.flowRate);
                        Model.parameterList.add(flux10.flowRate);
                        Model.parameterList.add(flux12.flowRate);
                        flux21.flowRate.dependVariables.add(aob);
                        flux21.flowRate.dependVariables.add(alpha);
                        flux21.flowRate.dependVariables.add(beta);
                        flux10.flowRate.dependVariables.add(alpha);
                        flux10.flowRate.dependVariables.add(beta);
                        flux12.flowRate.dependVariables.add(alpha);
                        flux12.flowRate.dependVariables.add(beta);
                        alpha.refCount = beta.refCount = 3;
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
                        flux20.flowRate.value = "K20=CL/V";
                        flux23.flowRate.value = "K23=Q/V";
                        flux32.flowRate.value = "K32=Q/(VSS-V)";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v = new Variable("V", "V=");
                        Variable q = new Variable("Q", "Q=");
                        Variable vss = new Variable("VSS", "VSS=");
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        Model.parameterList.remove(flux32.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v);
                        Model.parameterList.add(q);
                        Model.parameterList.add(vss);
                        Model.parameterList.add(flux20.flowRate);
                        Model.parameterList.add(flux23.flowRate);
                        Model.parameterList.add(flux32.flowRate);
                        flux20.flowRate.dependVariables.add(cl);
                        flux20.flowRate.dependVariables.add(v);
                        flux23.flowRate.dependVariables.add(q);
                        flux23.flowRate.dependVariables.add(v);
                        flux32.flowRate.dependVariables.add(v);
                        flux32.flowRate.dependVariables.add(q);
                        flux32.flowRate.dependVariables.add(vss);
                        v.refCount = 3;
                        q.refCount = 2;
                        break;
                    case 4:
                        flux20.flowRate.value = "K20=CL/V2";
                        flux23.flowRate.value = "K23=Q/V2";
                        flux32.flowRate.value = "K32=Q/V3";
                        cl = new Variable("CL", "CL=");
                        Variable v2 = new Variable("V2", "V2=");
                        q = new Variable("Q", "Q=");
                        Variable v3 = new Variable("V3", "V3=");
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        Model.parameterList.remove(flux32.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v2);
                        Model.parameterList.add(q);
                        Model.parameterList.add(v3);
                        Model.parameterList.add(flux20.flowRate);
                        Model.parameterList.add(flux23.flowRate);
                        Model.parameterList.add(flux32.flowRate);
                        flux20.flowRate.dependVariables.add(cl);
                        flux20.flowRate.dependVariables.add(v2);
                        flux23.flowRate.dependVariables.add(q);
                        flux23.flowRate.dependVariables.add(v2);
                        flux32.flowRate.dependVariables.add(q);
                        flux32.flowRate.dependVariables.add(v3);
                        v2.refCount = q.refCount = 2;
                        break;
                    case 5:
                        flux32.flowRate.value = "K32=(AOB*BETA+ALPHA)/(AOB+1)";
                        flux20.flowRate.value = "K20=ALPHA*BETA/K32";
                        flux23.flowRate.value = "K23=ALPHA+BETA-K32-K20";
                        Variable aob = new Variable("AOB", "AOB=");
                        Variable alpha = new Variable("ALPHA", "ALPHA=");
                        Variable beta = new Variable("BETA", "BETA=");
                        Model.parameterList.remove(flux32.flowRate);
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        Model.parameterList.add(aob);
                        Model.parameterList.add(alpha);
                        Model.parameterList.add(beta);
                        Model.parameterList.remove(flux32.flowRate);
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        flux32.flowRate.dependVariables.add(aob);
                        flux32.flowRate.dependVariables.add(alpha);
                        flux32.flowRate.dependVariables.add(beta);
                        flux20.flowRate.dependVariables.add(alpha);
                        flux20.flowRate.dependVariables.add(beta);
                        flux23.flowRate.dependVariables.add(alpha);
                        flux23.flowRate.dependVariables.add(beta);
                        alpha.refCount = beta.refCount = 3;
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
                        flux10.flowRate.value = "K10=VM/(KM+A(1))";
                        Variable vm = new Variable("VM", "VM=");
                        Variable km = new Variable("KM", "KM=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.add(vm);
                        Model.parameterList.add(km);
                        Model.parameterList.add(flux10.flowRate);
                        flux10.flowRate.dependVariables.add(vm);
                        flux10.flowRate.dependVariables.add(km);
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
                flux21 = new Element.Flux(comp2, comp1, model);
                Model.fluxes.add(flux21);
                Element.Flux flux31 = new Element.Flux(comp3, comp1, model);
                Model.fluxes.add(flux31);
                flux10 = new Element.Flux(comp1, null, model);
                Model.fluxes.add(flux10);
                Element.Flux flux13 = new Element.Flux(comp1, comp3, model);
                Model.fluxes.add(flux13);
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
                    case 4:
                        flux10.flowRate.value = "K10=CL/V1";
                        flux12.flowRate.value = "K12=Q2/V1";
                        flux21.flowRate.value = "K21=Q2/V2";
                        flux13.flowRate.value = "K13=Q3/V1";
                        flux31.flowRate.value = "K31=Q3/V3";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v1 = new Variable("V1", "V1=");
                        Variable q2 = new Variable("Q2", "Q2=");
                        Variable v2 = new Variable("V2", "V2=");
                        Variable q3 = new Variable("Q3", "Q3=");
                        Variable v3 = new Variable("V3", "V3=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.remove(flux12.flowRate);
                        Model.parameterList.remove(flux21.flowRate);
                        Model.parameterList.remove(flux13.flowRate);
                        Model.parameterList.remove(flux31.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v1);
                        Model.parameterList.add(q2);
                        Model.parameterList.add(v2);
                        Model.parameterList.add(q3);
                        Model.parameterList.add(v3);
                        Model.parameterList.add(flux10.flowRate);
                        Model.parameterList.add(flux12.flowRate);
                        Model.parameterList.add(flux21.flowRate);
                        Model.parameterList.add(flux13.flowRate);
                        Model.parameterList.add(flux31.flowRate);
                        flux10.flowRate.dependVariables.add(cl);
                        flux10.flowRate.dependVariables.add(v1);
                        flux12.flowRate.dependVariables.add(q2);
                        flux12.flowRate.dependVariables.add(v1);
                        flux21.flowRate.dependVariables.add(q2);
                        flux21.flowRate.dependVariables.add(v2);
                        flux13.flowRate.dependVariables.add(q3);
                        flux13.flowRate.dependVariables.add(v1);
                        flux31.flowRate.dependVariables.add(q3);
                        flux31.flowRate.dependVariables.add(v3);
                        v1.refCount = 3;
                        q2.refCount = q3.refCount = 2;
                        break;
                    case 6:
                        flux10.flowRate.value = "K10=ALPHA*BETA*GAMMA/(K21*K31)";
                        flux13.flowRate.value = "P=ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA\nS=ALPHA+BETA+GAMMA\nK13=(P+K31*K31-K31*S-K10*K21)/(K21-K31)";
                        flux12.flowRate.value = "S=ALPHA+BETA+GAMMA\nK12=S-K10-K13-K21-K31";
                        Variable alpha = new Variable("ALPHA", "ALPHA=");
                        Variable beta = new Variable("BETA", "BETA=");
                        Variable gamma = new Variable("GAMMA", "GAMMA=");
                        Model.parameterList.remove(flux10.flowRate);
                        Model.parameterList.remove(flux13.flowRate);
                        Model.parameterList.remove(flux12.flowRate);
                        Model.parameterList.add(alpha);
                        Model.parameterList.add(beta);
                        Model.parameterList.add(gamma);
                        Model.parameterList.add(flux10.flowRate);
                        Model.parameterList.add(flux13.flowRate);
                        Model.parameterList.add(flux12.flowRate);
                        flux10.flowRate.dependVariables.add(alpha);
                        flux10.flowRate.dependVariables.add(beta);
                        flux10.flowRate.dependVariables.add(gamma);
                        flux13.flowRate.dependVariables.add(alpha);
                        flux13.flowRate.dependVariables.add(beta);
                        flux13.flowRate.dependVariables.add(gamma);
                        flux12.flowRate.dependVariables.add(alpha);
                        flux12.flowRate.dependVariables.add(beta);
                        flux12.flowRate.dependVariables.add(gamma);
                        alpha.refCount = beta.refCount = gamma.refCount = 3;
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
                        flux20.flowRate.value = "K20=CL/V2";
                        flux23.flowRate.value = "K23=Q3/V2";
                        flux32.flowRate.value = "K32=Q3/V3";
                        flux24.flowRate.value = "K24=Q4/V2";
                        flux42.flowRate.value = "K42=Q4/V4";
                        Variable cl = new Variable("CL", "CL=");
                        Variable v2 = new Variable("V2", "V2=");
                        Variable q3 = new Variable("Q3", "Q3=");
                        Variable v3 = new Variable("V3", "V3=");
                        Variable q4 = new Variable("Q4", "Q4=");
                        Variable v4 = new Variable("V4", "V4=");
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        Model.parameterList.remove(flux32.flowRate);
                        Model.parameterList.remove(flux24.flowRate);
                        Model.parameterList.remove(flux42.flowRate);
                        Model.parameterList.add(cl);
                        Model.parameterList.add(v2);
                        Model.parameterList.add(q3);
                        Model.parameterList.add(v3);
                        Model.parameterList.add(q4);
                        Model.parameterList.add(v4);
                        Model.parameterList.add(flux20.flowRate);
                        Model.parameterList.add(flux23.flowRate);
                        Model.parameterList.add(flux32.flowRate);
                        Model.parameterList.add(flux24.flowRate);
                        Model.parameterList.add(flux42.flowRate);
                        flux20.flowRate.dependVariables.add(cl);
                        flux20.flowRate.dependVariables.add(v2);
                        flux23.flowRate.dependVariables.add(q3);
                        flux23.flowRate.dependVariables.add(v2);
                        flux32.flowRate.dependVariables.add(q3);
                        flux32.flowRate.dependVariables.add(v3);
                        flux24.flowRate.dependVariables.add(q4);
                        flux24.flowRate.dependVariables.add(v2);
                        flux42.flowRate.dependVariables.add(q4);
                        flux42.flowRate.dependVariables.add(v4);
                        v2.refCount = 3;
                        q3.refCount = q4.refCount = 2;
                        break;
                    case 6:
                        flux20.flowRate.value = "K20=ALPHA*BETA*GAMMA/(K32*K42)";
                        flux24.flowRate.value = "P=ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA\nS=ALPHA+BETA+GAMMA\nK24=(P+K42*K42-K42*S-K20*K32)/(K32-K42)";
                        flux23.flowRate.value = "S=ALPHA+BETA+GAMMA\nK23=S-K20-K24-K32-K42";
                        Variable alpha = new Variable("ALPHA", "ALPHA=");
                        Variable beta = new Variable("BETA", "BETA=");
                        Variable gamma = new Variable("GAMMA", "GAMMA=");
                        Model.parameterList.remove(flux20.flowRate);
                        Model.parameterList.remove(flux24.flowRate);
                        Model.parameterList.remove(flux23.flowRate);
                        Model.parameterList.add(alpha);
                        Model.parameterList.add(beta);
                        Model.parameterList.add(gamma);
                        Model.parameterList.add(flux20.flowRate);
                        Model.parameterList.add(flux24.flowRate);
                        Model.parameterList.add(flux23.flowRate);
                        flux20.flowRate.dependVariables.add(alpha);
                        flux20.flowRate.dependVariables.add(beta);
                        flux20.flowRate.dependVariables.add(gamma);
                        flux24.flowRate.dependVariables.add(alpha);
                        flux24.flowRate.dependVariables.add(beta);
                        flux24.flowRate.dependVariables.add(gamma);
                        flux23.flowRate.dependVariables.add(alpha);
                        flux23.flowRate.dependVariables.add(beta);
                        flux23.flowRate.dependVariables.add(gamma);
                        alpha.refCount = beta.refCount = gamma.refCount = 3;
                }
        }
        if(tool.iterator.analysis.equals("population"))
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
    private Vector<String> timeRates; // timeRate of each compartment in order
}
