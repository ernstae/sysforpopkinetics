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
package uw.rfpk.mda.saamii.compartment;

import java.util.Observable;
import java.util.Vector;
import java.util.Properties;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Dimension;

/**
 *
 * @author  jiaji Du
 */
public class Model implements Cloneable
{    
    /** Creates a new instance of Model */
    public Model(Vector models) 
    {
        this.models = models;
    }
    
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
//        model.elements = new Vector();
//        model.fluxes = new Vector();
        model.inputs = new Vector();
        model.samples = new Vector();
//        model.deletedElements = new Vector();
//        model.deletedSamples = new Vector();
        model.appliedSubjects = new Vector();
       
        if(isCopyToDiagram)
        {
            for(int i = 0; i < elements.size(); i++)
                ((Element)model.elements.get(i)).model = model;
            for(int i = 0; i < fluxes.size(); i++)
                ((Element)model.fluxes.get(i)).model = model;
        }
/* 
        for(int i = 0; i < elements.size(); i++)
        {
            Element tempElement = (Element)elements.get(i);                
            if(tempElement instanceof Element.Compartment)
            {
                Element.Compartment oldElement = (Element.Compartment)tempElement;
                Element.Compartment newElement = new Element.Compartment(oldElement.name, oldElement.xCenter, 
                                                                         oldElement.yCenter, model);
                newElement.number = oldElement.number;
                model.nElement--;
                model.elements.add(newElement);
            }
            if(tempElement instanceof Element.Delay)
            {
                Element.Delay oldElement = (Element.Delay)tempElement;
                Element.Delay newElement = new Element.Delay(oldElement.time, oldElement.xCenter,
                                                             oldElement.yCenter, model);
                newElement.number = oldElement.number;
                model.elements.add(newElement);
            }
        }
        for(int i = 0; i < fluxes.size(); i++)
        {
            Element.Flux oldFlux = (Element.Flux)fluxes.get(i);                
            int index1 = elements.indexOf(oldFlux.element1);
            int index2 = elements.indexOf(oldFlux.element2);
            if(oldFlux.element2 != null)                    
                model.fluxes.add(new Element.Flux((Element)model.elements.get(index1), 
                                                  (Element)model.elements.get(index2), model));
            else
                model.fluxes.add(new Element.Flux((Element)model.elements.get(index1), 
                                                  null, model));
        }
*/        
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
            model.samples.add(newSample);
        }
        isClone = false;
/*
        for(int i = 0; i < deletedElements.size(); i++)
            model.deletedElements.add(new Integer(((Integer)deletedElements.get(i)).intValue()));
        for(int i = 0; i < deletedSamples.size(); i++)
            model.deletedSamples.add(new Integer(((Integer)deletedSamples.get(i)).intValue()));
*/
        selectedElement = null;
        return model;
    }
            
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
    
    protected void clear()
    {
        elements.removeAllElements();
        fluxes.removeAllElements();
        inputs.removeAllElements();
        samples.removeAllElements();
        nElement = 0;
        nSample = 0;
        isSelected = false;
        selectedElement = null;
    }
    
    protected void remove(Element element)
    {        
        if(element instanceof Element.Compartment)
        {
            for(int i = 0; i < fluxes.size(); i++)
            {
                Element.Flux flux = (Element.Flux)fluxes.get(i);
                if(flux.element1 == element && flux.element2 != element)
                    fluxes.remove(i--);
                if(flux.element1 != element && flux.element2 == element)
                {
                    if(flux.element1 instanceof Element.Delay)
                        ((Element.Delay)flux.element1).compartments.remove(flux.element2);
                    fluxes.remove(i--);
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
        }
        if(element instanceof Element.Input)
        {
            inputs.remove(element);
        }
        if(element instanceof Element.Sample)
        {         
            samples.remove(element);
        }
        element = null;
    }
    
    protected int id;
    protected String name;     
    protected static Vector elements = new Vector();
    protected static Vector fluxes = new Vector();
    protected Vector inputs = new Vector();
    protected Vector samples = new Vector();
    protected boolean isSelected = false;
    protected Element selectedElement = null;
    protected static int nElement = 0;
    protected int nInput = 0;
    protected int nSample = 0;
    protected Vector appliedSubjects = new Vector();
    private Vector models;
    protected static boolean isClone;
    protected boolean isCopyToDiagram;
    protected static Properties variables = new Properties();
}
