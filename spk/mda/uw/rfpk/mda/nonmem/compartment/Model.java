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
     * @param models all the models.
     */
    public Model(Vector models) 
    {
        this.models = models;
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

        model.inputs = new Vector();
        model.samples = new Vector();
        model.appliedSubjects = new Vector();
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
            Vector compartments = new Vector();
            for(int j = 0; j < oldInput.compartments.size(); j++)
            {
                Element.Compartment compartment = (Element.Compartment)oldInput.compartments.get(j);
                int index = elements.indexOf(compartment);
                compartments.add((Element.Compartment)model.elements.get(index));
            }
            Element.Input newInput = new Element.Input(compartments, oldInput.xCenter, oldInput.yCenter, model);
            newInput.number = oldInput.number;
            newInput.name = oldInput.name;
            model.inputs.add(newInput);
        }
        for(int i = 0; i < samples.size(); i++)
        {
            Element.Sample oldSample = (Element.Sample)samples.get(i);
            Vector compartments = new Vector();
            for(int j = 0; j < oldSample.compartments.size(); j++)
            {
                Element.Compartment compartment = (Element.Compartment)oldSample.compartments.get(j);
                int index = elements.indexOf(compartment);
                compartments.add((Element.Compartment)model.elements.get(index));
            }
            Element.Sample newSample = new Element.Sample(compartments, oldSample.xCenter, oldSample.yCenter, model);
            newSample.number = oldSample.number;
            newSample.name = oldSample.name;
            newSample.errorModel = oldSample.errorModel;
            newSample.associatedDataName = oldSample.associatedDataName;
            model.samples.add(newSample);
        }
        isClone = false;
        selectedElement = null;
        return model;
    }
    
    /** Draw model.
     * @param gc2D Graphics2D.
     */
    protected void draw(Graphics2D gc2D)
    {
        for(int i = 0; i < elements.size(); i++)
            ((Element)elements.get(i)).draw(gc2D);
        for(int i = 0; i < fluxes.size(); i++)
            ((Element)fluxes.get(i)).draw(gc2D);
        for(int i = 0; i < inputs.size(); i++)
            ((Element)inputs.get(i)).draw(gc2D);
        for(int i = 0; i < samples.size(); i++)
            ((Element)samples.get(i)).draw(gc2D);        
    }
    
    /** Remove all elements of the model. */
    protected void clear()
    {
        elements.removeAllElements();
        fluxes.removeAllElements();
        inputs.removeAllElements();
        samples.removeAllElements();
        appliedSubjects.removeAllElements();
        nElement = 0;
        nSample = 0;
        isSelected = false;
        selectedElement = null;
        variables.clear();
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
                    variables.remove(String.valueOf(((Element.Flux)fluxes.remove(i--)).number));
                if(flux.element1 != element && flux.element2 == element)
                {
                    if(flux.element1 instanceof Element.Delay)
                    {
                        Element.Delay delay = ((Element.Delay)flux.element1);
                        int index = delay.compartments.indexOf(flux.element2);
                        delay.compartments.remove(index);
                        delay.fractions.remove(index);
                    }
                    variables.remove(String.valueOf(((Element.Flux)fluxes.remove(i--)).number));
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
            
            // Renumber compartments and delays
            for(int i = element.number - 1; i < elements.size(); i++)
                ((Element)elements.get(i)).number--;
        }
        if(element instanceof Element.Delay)
        {
            for(int i = 0; i < fluxes.size(); i++)
            {
                Element.Flux flux = (Element.Flux)fluxes.get(i);
                if(flux.element1 == element || flux.element2 == element)
                    fluxes.remove(i--);
            }
            elements.remove(element);
        }
        if(element instanceof Element.Flux)
        {
            fluxes.remove(element);
            variables.remove(String.valueOf(((Element.Flux)element).number));
            Element.Flux flux = (Element.Flux)element;
            if(flux.element1 instanceof Element.Delay)
            {
                Element.Delay delay = (Element.Delay)flux.element1;
                int index = delay.compartments.indexOf(flux.element2);
                delay.compartments.remove(index);
                delay.fractions.remove(index);
            }
        }
        if(element instanceof Element.Input)
        {
            Vector comps = ((Element.Input)element).compartments;
            for(int i = 0; i < comps.size(); i++)
                ((Element.Compartment)comps.get(i)).nInputs--;
            inputs.remove(element);
        }
        if(element instanceof Element.Sample)
        {         
            samples.remove(element);
        }
        element = null;
    }
    
    /** Model ID */
    protected int id;
    /** Model name */
    protected String name;
    /** All elements */
    protected static Vector elements = new Vector();
    /** All fluxes */
    protected static Vector fluxes = new Vector();
    /** All inputs */
    protected Vector inputs = new Vector();
    /** All samples */
    protected Vector samples = new Vector();
    /** Is the model selected */
    protected boolean isSelected = false;
    /** Selected element */
    protected Element selectedElement = null;
    /** Number of elements */
    protected static int nElement = 0;
    /** Number of inputs */
    protected int nInput = 0;
    /** Number of samples */
    protected int nSample = 0;
    /** Subjects that the model applies to */
    protected Vector appliedSubjects = new Vector();
    /** Is clone */
    protected static boolean isClone;
    /** Is copy to the diagram */
    protected boolean isCopyToDiagram;
    /** Mixed effects variables of the model */
    protected static Properties variables = new Properties();
    private Vector models;
}