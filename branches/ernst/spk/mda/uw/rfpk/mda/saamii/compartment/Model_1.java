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
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Dimension;

/**
 *
 * @author  jiaji Du
 */
public class Model_1 implements Cloneable
{    
    /** Creates a new instance of Model */
    public Model_1(Vector models) 
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
            int index = elements.indexOf(oldInput.compartment);
            Element.Input newInput = new Element.Input((Element.Compartment)model.elements.get(index), model);
            newInput.number = oldInput.number;
            newInput.name = oldInput.name;
            newInput.xCenter = oldInput.xCenter;
            newInput.yCenter = oldInput.yCenter;
            model.inputs.add(newInput);
        }
        for(int i = 0; i < samples.size(); i++)
        {
            Element.Sample oldSample = (Element.Sample)samples.get(i);                
            int index = elements.indexOf(oldSample.compartment);
            Element.Sample newSample = new Element.Sample((Element.Compartment)model.elements.get(index), model);
            newSample.number = oldSample.number;
            newSample.name = oldSample.name;
            newSample.xCenter = oldSample.xCenter;
            newSample.yCenter = oldSample.yCenter;
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
        deletedElements.removeAllElements();
        deletedInputs.removeAllElements();
        deletedSamples.removeAllElements();
    }
    
    protected void remove(Element element)
    {        
        if(element instanceof Element.Compartment || element instanceof Element.Delay)
        {
            for(int i = 0; i < fluxes.size(); i++)
            {
                Element.Flux flux = (Element.Flux)fluxes.get(i);
                if(flux.element1 == element || flux.element2 == element)
                {
                    fluxes.remove(flux);
                    i--;
                }
            }
            for(int i = 0; i < inputs.size(); i++)
            {
                if(((Element.Input)inputs.get(i)).compartment == element)
                    inputs.remove(i--);
            }
            for(int i = 0; i < samples.size(); i++)
            {
                if(((Element.Sample)samples.get(i)).compartment == element)
                    samples.remove(i--);
            }
            for(int j = 0; j < models.size(); j++)
            {
                Model m = (Model)models.get(j);
                Vector inp = (Vector)m.inputs;
                for(int i = 0; i < inp.size(); i++)
                {
                    Element.Input input = (Element.Input)inp.get(i);
                    if(input.compartment == element)
                    {
                        m.deletedInputs.add(input);
                        inp.remove(input);
                        i--;
                    }
                }
                Vector sam = (Vector)m.samples;
                for(int i = 0; i < sam.size(); i++)
                {
                    Element.Sample sample = (Element.Sample)sam.get(i);
                    if(sample.compartment == element)
                    {
                        m.deletedSamples.add(sample);
                        sam.remove(sample);
                        i--;   
                    }
                }
            }
            elements.remove(element);
            deletedElements.add(new Integer(element.number));
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
            deletedSamples.add(new Integer(element.number));
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
    protected static Vector deletedElements = new Vector();
    protected Vector deletedInputs = new Vector();
    protected Vector deletedSamples = new Vector();
    protected Vector appliedSubjects = new Vector();
    private Vector models;
    protected static boolean isClone;
    protected boolean isCopyToDiagram;
}
