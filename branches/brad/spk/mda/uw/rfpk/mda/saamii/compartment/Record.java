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

import java.util.*;

/**
 *
 * @author  jiaji Du
 */
public class Record {
    
    /** Creates a new instance of Record */
    public Record(Vector models, Properties subjectModel)
    {
        this.models = models;
        this.subjectModel = subjectModel;
    }
    
    protected String getModel()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment compartment = (Element.Compartment)element;
                sb.append("COMP=(");
                sb.append(compartment.name);
                if(compartment.force != null)
                {
                    sb.append(",");
                    sb.append(compartment.force);
                }
                sb.append(")");
                if(i < size - 1)
                    sb.append("\n");
            }
        }
        return sb.toString();
    }
    
    protected String getPK()
    {
        StringBuffer sb = new StringBuffer();
        List keySet = new Vector(Model.variables.keySet());
        Iterator keyIter = keySet.iterator();
        String key, value;
        while(keyIter.hasNext())
        {
            key = (String)keyIter.next();
            value = Model.variables.getProperty(key);
            sb.append("\n");
            sb.append(key + " = " + value);
        }
        int size = Model.fluxes.size();
        for(int i = 0; i < size; i++)
        {
            Element.Flux flux = (Element.Flux)Model.fluxes.get(i);
            if(flux.equations != null)
            {
                if(sb.length() != 0) sb.append("\n");
                sb.append(flux.equations);
            }
        }
        size = Model.elements.size();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment &&
               ((Element.Compartment)element).equations != null)
            {
                if(sb.length() != 0) sb.append("\n");
                sb.append(((Element.Compartment)element).equations);
            }
        }

        return sb.toString();
    }
    
    protected String getDes()
    {
        StringBuffer sb = new StringBuffer();
        int size = Model.elements.size();
        for(int i = 0; i < size; i++)
        {
            Element element = (Element)Model.elements.get(i);
            if(element instanceof Element.Compartment)
            {
                Element.Compartment compartment = (Element.Compartment)element;
                sb.append("DADT(" + compartment.number + ")=");
                for(int j = 0; j < Model.fluxes.size(); j++)
                {
                    Element.Flux flux = (Element.Flux)Model.fluxes.get(j);
                    if(flux.element1 == element)
                        sb.append("-" + flux.name + "*A(" + compartment.number + ")");
                    if(flux.element2 == element)
                        sb.append("+" + flux.name + "*A(" + flux.element1.number + ")");
                }
                for(int k = 0; k < models.size(); k++)
                {
                    Model model = (Model)models.get(k);
                    for(int j = 0; j < model.inputs.size(); j++)
                    {
                        Element.Input input = (Element.Input)model.inputs.get(j);
                        for(int l = 0; l < input.compartments.size(); l++)
                        {
                            if(input.compartments.get(l) == element)
                            {
                                sb.append("+" + input.name);
                                break;
                            }
                        }
                    }
                }
                if(i < size - 1)
                    sb.append("\n");
            }
        }
        return sb.toString();
    }
    
    private Vector models;
    private Properties subjectModel;
}
