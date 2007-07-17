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

import java.util.Observable;
import java.util.Vector;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Properties;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Dimension;

/** This class defines model.
 *
 * @author  Jiaji Du
 */
 public class Model implements Cloneable
{    
    /** Creates a new instance of Model. 
     * @param tool the DesignTool object.
     */
    public Model(DesignTool tool) 
    {
        this.models = tool.models;
        this.tool = tool;
    }
    
    /** Clone a model.
     * @return a cloned model object.
     */
    public Object clone()
    {
        Model model;
        try
        {
            model = (Model)super.clone();
        }
        catch(CloneNotSupportedException e)
        {
            return null;
        }

        model.inputs = new Vector<Element.Input>();
        model.samples = new Vector<Element.Sample>();
        if(isCopyToDiagram)
        {
            for(int i = 0; i < elements.size(); i++)
                ((Element)model.elements.get(i)).model = model;
            for(int i = 0; i < fluxes.size(); i++)
                ((Element)model.fluxes.get(i)).model = model;
        }

        isClone = true;
        for(int i = 0; i < inputs.size(); i++)
        {
            Element.Input oldInput = (Element.Input)inputs.get(i);
            Vector<Element.Compartment> compartments = new Vector<Element.Compartment>();
            for(int j = 0; j < oldInput.compartments.size(); j++)
            {
                Element.Compartment compartment = (Element.Compartment)oldInput.compartments.get(j);
                int index = elements.indexOf(compartment);
                compartments.add((Element.Compartment)model.elements.get(index));
            }
            Element.Input newInput = new Element.Input(compartments, oldInput.xCenter, oldInput.yCenter, model);
            newInput.number = oldInput.number;
            newInput.name = oldInput.name;
            newInput.xCenter = oldInput.xCenter;
            newInput.yCenter = oldInput.yCenter;
            model.inputs.add(newInput);
        }
        for(int i = 0; i < samples.size(); i++)
        {
            Element.Sample oldSample = (Element.Sample)samples.get(i);
            Vector<Element.Compartment> compartments = new Vector<Element.Compartment>();
            for(int j = 0; j < oldSample.compartments.size(); j++)
            {
                Element.Compartment compartment = (Element.Compartment)oldSample.compartments.get(j);
                int index = elements.indexOf(compartment);
                compartments.add((Element.Compartment)model.elements.get(index));
            }
            Element.Sample newSample = new Element.Sample(compartments, oldSample.xCenter, oldSample.yCenter, model);
            newSample.number = oldSample.number;
            newSample.name = oldSample.name;
            newSample.xCenter = oldSample.xCenter;
            newSample.yCenter = oldSample.yCenter;
            newSample.errorModel = oldSample.errorModel;
            model.samples.add(newSample);
        }
        isClone = false;
        selectedElement = null;
        return model;
    }
    
    /** Draw model.
     * @param gc2D Graphics2D.
     * @param x left margin.
     * @param y top margin.
     */
    protected void draw(Graphics2D gc2D, int x, int y)
    {
        for(int i = 0; i < elements.size(); i++)
            ((Element)elements.get(i)).draw(gc2D, x, y);
        for(int i = 0; i < fluxes.size(); i++)
            ((Element)fluxes.get(i)).draw(gc2D, x, y);
        for(int i = 0; i < inputs.size(); i++)
            ((Element)inputs.get(i)).draw(gc2D, x, y);
        for(int i = 0; i < samples.size(); i++)
            ((Element)samples.get(i)).draw(gc2D, x, y);        
    }
    
    /** Remove all elements of the model. */
    protected void clear()
    {
        elements.removeAllElements();
        fluxes.removeAllElements();
        inputs.removeAllElements();
        samples.removeAllElements();
        isSelected = false;
        selectedElement = null;
    }
    
    /** Remove an element.
     * @param element element to remove.
     */
    protected void remove(Element element)
    {        
        if(element instanceof Element.Compartment)
        {
            for(int i = 0; i < fluxes.size(); i++)
            {
                Element.Flux flux = (Element.Flux)fluxes.get(i);
                if(flux.element1 == element && flux.element2 != element)
                {
                    fluxes.remove(flux);
                    parameterList.remove(flux.flowRate);
                    tool.updateParameterList(flux.flowRate, true);
                    i--;
                }
                if(flux.element1 != element && flux.element2 == element)
                {
                    if(flux.element1 instanceof Element.Delay)
                    {
                        Element.Delay delay = ((Element.Delay)flux.element1);
                        int index = delay.compartments.indexOf(flux.element2);
                        delay.compartments.remove(index);
                        delay.fractions.remove(index);
                    }
                    fluxes.remove(flux);
                    parameterList.remove(flux.flowRate);
                    tool.updateParameterList(flux.flowRate, true);
                    i--;
                }
            }
            for(int i = 0; i < inputs.size(); i++)
            {
                Element.Input input = (Element.Input)inputs.get(i);
                for(int j = 0; j < input.compartments.size(); j++)
                    if((Element.Compartment)input.compartments.get(j) == element)
                        inputs.remove(i--);
            }
            for(int i = 0; i < samples.size(); i++)
            {
                Element.Sample sample = (Element.Sample)samples.get(i);
                for(int j = 0; j < sample.compartments.size(); j++)
                    if((Element.Compartment)sample.compartments.get(j) == element)
                        samples.remove(i--);
            }
            for(int j = 0; j < models.size(); j++)
            {
                Model m = (Model)models.get(j);
                Vector inp = (Vector)m.inputs;
                for(int i = 0; i < inp.size(); i++)
                {
                    Element.Input input = (Element.Input)inp.get(i);
                    for(int k = 0; k < input.compartments.size(); k++)
                        if((Element.Compartment)input.compartments.get(k) == element)
                            inp.remove(i--);
                }
                Vector sam = (Vector)m.samples;
                for(int i = 0; i < sam.size(); i++)
                {
                    Element.Sample sample = (Element.Sample)sam.get(i);
                    for(int k = 0; k < sample.compartments.size(); k++)
                        if((Element.Compartment)sample.compartments.get(k) == element)
                            sam.remove(i--); 
                }
            }
            elements.remove(element);
            Element.Compartment compartment = (Element.Compartment)element;
            Iterator iterator = compartment.parameters.keySet().iterator();
            while(iterator.hasNext())
            {
                Parameter parameter = (Parameter)compartment.parameters.get(iterator.next());
                parameterList.remove(parameter);
                tool.updateParameterList(parameter, true);
            }
            if(compartment.force != null)
            {
                parameterList.remove(compartment.force);
                tool.updateParameterList(compartment.force, true);
            }
            renumberElements(element);
        }
        if(element instanceof Element.Delay)
        {
            for(int i = 0; i < fluxes.size(); i++)
            {
                Element.Flux flux = (Element.Flux)fluxes.get(i);
                if(flux.element1 == element || flux.element2 == element)
                {
                    fluxes.remove(i--);
                    parameterList.remove(flux.flowRate);
                    tool.updateParameterList(flux.flowRate, true);
                }
            }
            elements.remove(element);
            Parameter parameter = ((Element.Delay)element).delayTime;
            parameterList.remove(parameter);
            tool.updateParameterList(parameter, true);
            renumberElements(element);
        }
        if(element instanceof Element.Flux)
        {
            fluxes.remove(element);
            Element.Flux flux = (Element.Flux)element;
            parameterList.remove(flux.flowRate);
            tool.updateParameterList(flux.flowRate, true);
            if(flux.element1 instanceof Element.Delay)
            {
                Element.Delay delay = (Element.Delay)flux.element1;
                int index = delay.compartments.indexOf(flux.element2);
                delay.compartments.remove(index);
                delay.fractions.remove(index);
                parameterList.remove(delay.delayTime);
                tool.updateParameterList(delay.delayTime, true);
            }
        }
        if(element instanceof Element.Input)
        {
            inputs.remove(element);
            Element.Compartment comp = (Element.Compartment)((Element.Input)element).compartments.get(0);
            boolean isDose = false;
            for(int i = 0; i < models.size(); i++)
            {
                if((Model)models.get(i) == tool.selectedModel) continue;
                Vector inpts = ((Model)models.get(i)).inputs;
                for(int j = 0; j < inpts.size(); j++)
                {
                    Vector compartments = ((Element.Input)inpts.get(j)).compartments;
                    if((Element.Compartment)compartments.get(0) == comp)
                    {
                        isDose = true;
                        break;
                    }
                }
            }
            if(!isDose) comp.attributes.remove("DEFDOSE");
        }
        if(element instanceof Element.Sample)
        {
            samples.remove(element);
            Element.Compartment comp = (Element.Compartment)((Element.Sample)element).compartments.get(0);
            boolean isObsv = false;
            for(int i = 0; i < models.size(); i++)
            {
                if((Model)models.get(i) == tool.selectedModel) continue;
                Vector smpls = ((Model)models.get(i)).samples;               
                for(int j = 0; j < smpls.size(); j++)
                {
                    Vector compartments = ((Element.Sample)smpls.get(j)).compartments;
                    if((Element.Compartment)compartments.get(0) == comp)
                    {
                        isObsv = true;
                        break;
                    }
                }
            }
            if(!isObsv) comp.attributes.remove("DEFOBSERVATION");
        }
        element = null;
    }
    
    private void renumberElements(Element element)
    {
        for(int i = element.number - 1; i < elements.size(); i++)
        {
            Element elem = ((Element)elements.get(i));
            elem.number--;
            if(elem instanceof Element.Compartment)
            {
                Element.Compartment comp = (Element.Compartment)elem;
                if(comp.force != null)
                {
                    comp.force.name = "FF" + comp.number;
                    comp.force.value = "FF" + comp.number + comp.force.value.substring(comp.force.value.indexOf("="));
                    tool.updateParameterList(comp.force, false);
                }
                Iterator iterator = comp.parameters.keySet().iterator();
                while(iterator.hasNext())
                {
                    Parameter parameter = (Parameter)comp.parameters.get(iterator.next());
                    if(parameter.name.startsWith("S")) parameter.name = "S" + comp.number;
                    if(parameter.name.startsWith("F")) parameter.name = "F" + comp.number;
                    if(parameter.name.startsWith("R")) parameter.name = "R" + comp.number;
                    if(parameter.name.startsWith("D")) parameter.name = "D" + comp.number;
                    if(parameter.name.startsWith("ALAG")) parameter.name = "ALAG" + comp.number;
                    if(parameter.value.indexOf("IF(") == -1)
                    {
                        parameter.value = "\n" + parameter.value;
                        parameter.value = parameter.value.substring(0, parameter.value.lastIndexOf("\n") + 1) +
                                          parameter.name + parameter.value.substring(parameter.value.lastIndexOf("="));
                        parameter.value = parameter.value.trim();
                    }
                    else
                    {
                        int indexIF = parameter.value.indexOf("IF(");
                        String[] lines = parameter.value.substring(indexIF).split("\n");
                        lines[1] = parameter.name + lines[1].substring(lines[1].indexOf("="));
                        lines[3] = parameter.name + lines[3].substring(lines[1].indexOf("="));
                        parameter.value = parameter.value.substring(0, indexIF) + lines[0] + lines[1] + lines[2] + lines[3] + lines[4];
                    }       
                    tool.updateParameterList(parameter, false);
                }
            }
            else if(elem instanceof Element.Delay)
            {
                Element.Delay delay = (Element.Delay)elem;
                delay.delayTime.name = "TLAG" + delay.number;
                if(delay.delayTime.value.indexOf("IF(") == -1)
                {
                    delay.delayTime.value = "\n" + delay.delayTime.value;
                    delay.delayTime.value = delay.delayTime.value.substring(0, delay.delayTime.value.lastIndexOf("\n") + 1) +
                                            delay.delayTime.name + delay.delayTime.value.substring(delay.delayTime.value.lastIndexOf("="));
                    delay.delayTime.value = delay.delayTime.value.trim();
                }
                else
                {
                    int indexIF = delay.delayTime.value.indexOf("IF(");
                    String[] lines = delay.delayTime.value.substring(indexIF).split("\n");
                    lines[1] = delay.delayTime.name + lines[1].substring(lines[1].indexOf("="));
                    lines[3] = delay.delayTime.name + lines[3].substring(lines[1].indexOf("="));
                    delay.delayTime.value = delay.delayTime.value.substring(0, indexIF) + lines[0] + lines[1] + lines[2] + lines[3] + lines[4];
                }
                tool.updateParameterList(delay.delayTime, false);
            }
            for(Element.Flux flux: fluxes)
            {
                if(flux.element1 == element || flux.element2 == element)
                {
                    if(flux.element1 != null && flux.element2 != null)
                    {
                        if(flux.element1 instanceof Element.Compartment)
                        {
                            if(flux.element1.number < 10 && flux.element2.number < 10)
                                flux.name = "K" + flux.element1.number + flux.element2.number;
                            else
                                flux.name = "K" + flux.element1.number + "T" + flux.element2.number;
                        }
                        if(flux.element1 instanceof Element.Delay)
                        {
                            if(flux.element1.number < 10 && flux.element2.number < 10)
                                flux.name = "D" + flux.element1.number + flux.element2.number;
                            else
                                flux.name = "D" + flux.element1.number + "T" + flux.element2.number;
                        }
                    }
                    else if(flux.element2 == null)
                    {
                        if(flux.element1 instanceof Element.Compartment)
                        {
                            if(flux.element1.number < 10)
                                flux.name = "K" + flux.element1.number + "0";
                            else
                                flux.name = "K" + flux.element1.number + "T0";
                        }
                        if(flux.element1 instanceof Element.Delay)
                        {
                            if(flux.element1.number < 10)
                                flux.name = "D" + flux.element1.number + "0";
                            else
                                flux.name = "D" + flux.element1.number + "T0";                   
                        }
                    }
                    flux.flowRate.name = flux.name;
                    if(!flux.name.startsWith("D"))
                    {
                        if(flux.flowRate.value.indexOf("IF(") == -1)
                        {
                            flux.flowRate.value = "\n" + flux.flowRate.value;
                            flux.flowRate.value = flux.flowRate.value.substring(0, flux.flowRate.value.lastIndexOf("\n") + 1) +
                                                  flux.name + flux.flowRate.value.substring(flux.flowRate.value.lastIndexOf("="));
                            flux.flowRate.value = flux.flowRate.value.trim();
                        }
                        else
                        {
                            int indexIF = flux.flowRate.value.indexOf("IF(");
                            String[] lines = flux.flowRate.value.substring(indexIF).split("\n");
                            lines[1] = flux.flowRate.name + lines[1].substring(lines[1].indexOf("="));
                            lines[3] = flux.flowRate.name + lines[3].substring(lines[1].indexOf("="));
                            flux.flowRate.value = flux.flowRate.value.substring(0, indexIF) + lines[0] + lines[1] + lines[2] + lines[3] + lines[4];
                        }
                        tool.updateParameterList(flux.flowRate, false);
                    }
                }
            }
        }
    }
    
    /** Model ID */
    protected int id;
    /** Model name */
    protected String name;
    /** All elements */
    protected static Vector<Element> elements = new Vector<Element>();
    /** All fluxes */
    protected static Vector<Element.Flux> fluxes = new Vector<Element.Flux>();
    /** All inputs */
    protected Vector<Element.Input> inputs = new Vector<Element.Input>();
    /** All samples */
    protected Vector<Element.Sample> samples = new Vector<Element.Sample>();
    /** Is the model selected */
    protected boolean isSelected = false;
    /** Selected element */
    protected Element selectedElement = null;
    /** Number of inputs */
    protected int nInput = 0;
    /** Number of samples */
    protected int nSample = 0;
    /** Is clone */
    protected static boolean isClone;
    /** Is copy to the diagram */
    protected boolean isCopyToDiagram;
    /** User defined parameter of all groups */
    protected static ArrayList<Parameter> parameterList = new ArrayList<Parameter>();
    /** Des block equations of all groups */
    protected static String desEqns = "";
    /** Error block equations of all groups */
    protected static String errorEqns = "";
    private Vector<Model> models;
    private DesignTool tool;
}
