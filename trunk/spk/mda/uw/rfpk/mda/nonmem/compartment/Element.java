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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Polygon;
import java.awt.BasicStroke;
import java.util.Vector;
import java.util.HashMap;
import java.util.Properties;

/** This class defines elements of graphical representation of a model.
 *
 * @author  Jiaji Du
 */
public abstract class Element implements Cloneable
{
    /** Draw the element.
     * @param gc2D Graphics2D
     * @param x0 left margin
     * @param y0 top margin
     */
    public abstract void draw(Graphics2D gc2D, int x0, int y0);
    
    /** Detect if (x, y) is within the element boundary.
     * @param x x-coordinate of the point.
     * @param y y-coordinate of the point.
     * @return true if the point is within the element boundary, false otherwise.
     */
    public abstract boolean contains(int x, int y);
    
    /** Draw the element's boundary.
     * @param gc2D Graphics2D  
     */
    public abstract void drawBoundary(Graphics2D gc2D);
    
    /** Draw element name.
     * @param gc2D Graphics2D
     * @param x0 left margin
     * @param y0 top margin
     */
    public abstract void drawName(Graphics2D gc2D, int x0, int y0);
    
    /** Clone element.
     * @return Object.
     */
    public Object clone()
    {
        try
        {
            return super.clone();
        }
        catch(CloneNotSupportedException e)
        {
            return null;
        }
    }
    
    private static int newNumber(Vector list)
    {
        int j = 1;
        int number = 0;
        boolean done = false;
        boolean exist = false;
        while(!done)
        {
            for(int i = 0; i < list.size(); i++)
            {
                Element element = (Element)list.get(i);
                if(element.number == j)
                {
                    exist = true;
                    break;
                }
                exist = false;
            }
            if(!exist)
            {
                number = j;
                done = true;
            }
            else
                j++;
        }
        return number;
    }

    /** This class defines compartment. */
    protected static class Compartment extends Element
    {
        /** Create a new instance of compartment.
         * @param x x-coordinate of the compartment center.
         * @param y y-coordinate of the compartment center.
         * @param model the model that this compartment belongs to.
         */
        public Compartment(int x, int y, Model model)
        {
            super.xCenter = x;
            super.yCenter = y;
            super.model = model;
            number = newNumber(Model.elements);
            name = "A" + number;
        }
        
        /** Draw compartment.
         * @param gc2D Graphics2D
         * @param x0 left margin
         * @param y0 top margin      
         */     
        public void draw(Graphics2D gc2D, int x0, int y0)
        {        
            gc2D.setColor(Color.black);
            gc2D.drawOval(xCenter + x0 - 25, yCenter + y0 -25, 50, 50);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillOval(xCenter + x0 - 24, yCenter + y0 -24, 48, 48);
            }
            drawName(gc2D, x0, y0);
        }
        
        /** Detect if (x,y) is within the element boundary.
         * @param x x-coordinate of the point.
         * @param y y-coordinate of the point.
         * @return true if the point is within the element boundary, false otherwise.
         */
        public boolean contains(int x, int y) 
        {
            return (x - xCenter) * (x - xCenter) + (y - yCenter) * (y - yCenter) <= 625;
        }
        
        /** Draw boundary of compartment.
         * @param gc2D Graphics2D.
         */        
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.black);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawOval(xCenter - 25, yCenter -25, 50, 50);
            gc2D.setStroke(new BasicStroke());
        }
        
        /** Draw name of compartment.
         * @param gc2D Graphics2D
         * @param x0 left margin
         * @param y0 top margin
         */
        public void drawName(Graphics2D gc2D, int x0, int y0) 
        {
            gc2D.setColor(Color.black);
            Graphics gc = (Graphics)gc2D;
            if(DesignTool.showName)
            {
                int labelWidth = gc.getFontMetrics().stringWidth(name)/2;
                gc2D.drawString(name, xCenter + x0 - labelWidth, yCenter + y0 + 5);
            }
            else
            {
                String label = String.valueOf(number);
                int labelWidth = gc.getFontMetrics().stringWidth(label)/2;
                gc2D.drawString(label, xCenter + x0 - labelWidth, yCenter + y0 + 5);
            }   
        }

        /** Attributes */
        protected Vector<String> attributes = new Vector<String>();
        /** Compartment force */
        protected Parameter force = null;
        /** Parameters */
        protected HashMap<String, Parameter> parameters = new HashMap<String, Parameter>();
        /** Time rate of concentration */
        protected String timeRate = "";
    }
    
    /** This class defines delay. */
    protected static class Delay extends Element
    {
        /** Create a new instance of delay.
         * @param x x-coordinate of the delay center.
         * @param y y-coordinate of the delay center.
         * @param model the model that this delay belongs to.
         */
        public Delay(int x, int y, Model model)
        {
            super.xCenter = x;
            super.yCenter = y;
            super.model = model;
            number = newNumber(Model.elements);
            name = "D" + number;
            delayTime = new Parameter(name, "TLAG" + number + "=");
//            delayTime= "TLAG" + number + "=";
            Model.parameterList.add(delayTime);
        }
        
        /** Draw delay.
         * @param gc2D Graphics2D
         * @param x0 left margin
         * @param y0 top margin
         */        
        public void draw(Graphics2D gc2D, int x0, int y0) 
        {            
            gc2D.translate(xCenter + x0, yCenter + y0);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillRect(-22, -16, 44, 32);
            }
            gc2D.setColor(Color.black);
            gc2D.drawRect(-23, -17, 46, 34);
            gc2D.drawLine(-23, 0, -16, 0);
            gc2D.drawOval(-16, -4, 8, 8); 
            gc2D.drawLine(-8, 0, -4, 0);
            gc2D.drawOval(-4, 0 - 4, 8, 8); 
            gc2D.drawLine(+4, 0, 8, 0);
            gc2D.drawOval(8, 0 - 4, 8, 8); 
            gc2D.drawLine(16, 0, 23, 0);
            gc2D.translate(-xCenter - x0, -yCenter - y0);
            drawName(gc2D, x0, y0);
        }
        
        /** Detect if (x,y) is within the element boundary.
         * @param x x-coordinate of the point.
         * @param y y-coordinate of the point.
         * @return true if the point is within the element boundary, false otherwise. 
         */
        public boolean contains(int x, int y) 
        {
            return Math.abs(x - xCenter) <= 23 &&  Math.abs(y - yCenter) <= 17;
        }
        
        /** Draw boundary of delay.
         * @param gc2D Graphics2D.
         */
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.black);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawRect(xCenter - 23, yCenter - 17, 46, 34);
            gc2D.setStroke(new BasicStroke());
        }        
        
        /** Draw name of delay.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void drawName(Graphics2D gc2D, int x0, int y0)
        {
            gc2D.setColor(Color.black);
            Graphics gc = (Graphics)gc2D;
            if(DesignTool.showName)
            {
                int labelWidth = gc.getFontMetrics().stringWidth(name)/2;
                gc2D.drawString(name, xCenter + x0 - labelWidth, yCenter + y0 - 7);
            }
            else
            {
                String label = String.valueOf(number);
                int labelWidth = gc.getFontMetrics().stringWidth(label)/2;
                gc2D.drawString(label, xCenter + x0 - labelWidth, yCenter + y0 - 7);
            }
        }
       
        /** Delay time. */
        protected Parameter delayTime;
//        protected String delayTime;
        /** Number of compartments to simulate the delay. */
        protected int nDelayComps = 2;
        /** Ending compartments from the delay */
        protected Vector<Element> compartments = new Vector<Element>();
        /** Delay fractions */
        protected Vector<String> fractions = new Vector<String>();
    }
    
    /** This class defines flux. */
    protected static class Flux extends Element
    {
        /** Create a new instance of flux.
         * @param element1 starting element.
         * @param element2 ending element.
         * @param model the model that this flux belongs to.
         */        
        public Flux(Element element1, Element element2, Model model)
        {
            super.model = model;
            this.element1 = element1;
            this.element2 = element2;
            number = newNumber(Model.fluxes);
            if(element1 != null && element2 != null)
            {
                if(element1 instanceof Element.Compartment)
                {
                    if(element1.number < 10 && element2.number < 10)
                        name = "K" + element1.number + element2.number;
                    else
                        name = "K" + element1.number + "T" + element2.number;
                }
                if(element1 instanceof Element.Delay)
                {
                    if(element1.number < 10 && element2.number < 10)
                        name = "D" + element1.number + element2.number;
                    else
                        name = "D" + element1.number + "T" + element2.number;
                    ((Element.Delay)element1).compartments.add(element2);
                    ((Element.Delay)element1).fractions.add("?");
                }
            }
            else if(element2 == null)
            {
                if(element1 instanceof Element.Compartment)
                {
                    if(element1.number < 10)
                        name = "K" + element1.number + "0";
                    else
                        name = "K" + element1.number + "T0";
                }
                if(element1 instanceof Element.Delay)
                {
                    if(element1.number < 10)
                        name = "D" + element1.number + "0";
                    else
                        name = "D" + element1.number + "T0";
                }
            }
            else
            {
                name = "U(" + element2.number + ")";
//                if(element2.number < 10)
//                    name = "K" + "0" + element2.number;
//                else
//                    name = "K" + "0T" + element2.number;
            }
            flowRate = new Parameter(name, name + "=");
            if(name.startsWith("K"))
                Model.parameterList.add(flowRate);
//                Model.fluxList.add(name);
        }
        
        /** Draw flux.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void draw(Graphics2D gc2D, int x0, int y0) 
        {
            int x1 = element1 != null? element1.xCenter + x0 : 0;
            int y1 = element1 != null? element1.yCenter + y0 : 0;
            int x2 = element2 != null? element2.xCenter + x0 : 0;
            int y2 = element2 != null? element2.yCenter + y0 : 0;
            if(element1 == null)
            {
                xStart = x2 - 87;
                xEnd = x2 - 18;
                yStart = y2 - 87;
                yEnd = y2 - 18;
                if(this == model.selectedElement)
                {
                    gc2D.setColor(Color.yellow);
                    gc2D.fillPolygon(new int[]{x2 - 73, x2 - 67, x2 - 15, x2 - 21}, 
                                     new int[]{y2 - 67, y2 - 73, y2 - 21, y2 - 15}, 4);
                }
                gc2D.setColor(Color.black);
                drawArrow(gc2D, x2 - 70, y2 - 70, 0, 0, 71, 0.7854);
                drawName(gc2D, x0, y0);               
                sin = 0.7071;
                cos = 0.7071;
                return;
            }
            if(element2 == null)
            {
                xStart = xEnd = x1;
                if(element1 instanceof Element.Compartment) yStart = y1 + 25;
                if(element1 instanceof Element.Delay) yStart = y1 + 16;
                yEnd = yStart + 100;
                if(this == model.selectedElement)
                {
                    gc2D.setColor(Color.yellow);
                    if(element1 instanceof Element.Compartment)
                        gc2D.fillRect(x1 - 4, y1 + 26, 8, 74);
                    if(element1 instanceof Element.Delay)
                        gc2D.fillRect(x1 - 4, y1 + 18, 8, 74);
                }
                gc2D.setColor(Color.black);
                if(element1 instanceof Element.Compartment)
                    drawArrow(gc2D, x1, y1, 0, 25, 100, 1.5708);
                if(element1 instanceof Element.Delay)
                    drawArrow(gc2D, x1, y1, 0, 18, 100, 1.5708);
                drawName(gc2D, x0, y0);               
                sin = 1;
                cos = 0;
                return;
            }
            double angle = Math.atan2(y2 - y1, x2 - x1);
            double distance = Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
            cos = Math.cos(angle);
            sin = Math.sin(angle);
            tan = sin / cos;
            int start = 0; 
            int end = 0;
            if(element1 instanceof Compartment)
                start = 24;
            if(element2 instanceof Compartment)
                end = 24;
            if(element1 instanceof Delay)
            {
                if(angle > -0.3815 && angle <= 1.05)
                    start = (int)((23 - shift * sin) / cos) + 1;
                if(angle > 1.05 && angle <= 2.7601)
                    start = (int)((17 + shift * cos) / sin) + 1;
                if(angle > -2.0916 && angle <= -0.3815)
                    start = (int)((-17 + shift * cos)/ sin) + 1;
                if((angle > 2.7601 && angle <= 3.1416) || (angle > -3.1416 && angle <= -2.0916))
                    start = (int)((-23 - shift * sin) / cos) + 1;
            }
            if(element2 instanceof Delay)
            {
                if(angle > -1.05 && angle <= 0.3815)
                    end = (int)((23 + shift * sin) / cos) + 1;
                if(angle > -2.7601 && angle <= -1.05)
                    end = (int)((-17 - shift * cos) / sin) + 1;
                if(angle > 0.3815 && angle <= 2.0916)
                    end = (int)((17 - shift * cos)/ sin) + 1;
                if((angle > 2.0916 && angle <= 3.1416) || (angle > -3.1416 && angle <= -2.7601))
                    end = (int)((-23 + shift * sin) / cos) + 1;
            }
           
            xStart = (int)(x1 + start * cos + shift * sin);
            yStart = (int)(y1 + start * sin - shift * cos);
            xEnd = (int)(x1 + (distance - end) * cos + shift * sin);
            yEnd = (int)(y1 + (distance - end) * sin - shift * cos);
            
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                Polygon area = new Polygon(new int[]{xStart - (int)(4 * sin), xStart + (int)(4 * sin), 
                                                     xEnd + (int)(4 * sin), xEnd - (int)(4 * sin)},
                                           new int[]{yStart + (int)(4 * cos), yStart - (int)(4 * cos), 
                                                     yEnd - (int)(4 * cos), yEnd + (int)(4 * cos)}, 4);
                gc2D.fillPolygon(area);   
            }
            gc2D.setColor(Color.black);
            drawArrow(gc2D, x1, y1, shift, start, (int)distance - end, angle);
            drawName(gc2D, x0, y0);
        }
        
        private void drawArrow(Graphics2D gc2D, int x, int y, int a, int start, int end, double angle)
        {
            gc2D.translate(x, y);
            gc2D.rotate(angle);
            gc2D.drawLine(start, -a, end - 6, -a);
            Polygon triangle = new Polygon(new int[]{end - 6, end - 6, end}, 
                                           new int[]{-3 - a, 3 - a, -a}, 3);
            gc2D.draw(triangle);
            gc2D.fill(triangle);
            gc2D.rotate(-angle);
            gc2D.translate(-x, -y);
        }
        
        /** Detect if (x,y) is within the element boundary.
         * @param x x-coordinate of the point.
         * @param y y-coordinate of the point.
         * @return true if the point is within the element boundary, false otherwise. 
         */
        public boolean contains(int x, int y) 
        {
            Polygon area = new Polygon(new int[]{xStart - (int)(4 * sin), xStart + (int)(4 * sin), 
                                                 xEnd + (int)(4 * sin), xEnd - (int)(4 * sin)},
                                       new int[]{yStart + (int)(4 * cos), yStart - (int)(4 * cos), 
                                                 yEnd - (int)(4 * cos), yEnd + (int)(4 * cos)}, 4);                            
            return area.contains(x, y);
        }
        
       /** Draw nothing.
         * @param gc2D Graphics2D.
         */
        public void drawBoundary(Graphics2D gc2D)
        {

        }
        
        /** Draw name of flux.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */       
        public void drawName(Graphics2D gc2D, int x0, int y0)
        {
            if(element1 != null && element2 != null)
            {
                if(element1 instanceof Element.Compartment)
                {
                    if(element1.number < 10 && element2.number < 10)
                        name = "K" + element1.number + element2.number;
                    else
                        name = "K" + element1.number + "T" + element2.number;
                }
                if(element1 instanceof Element.Delay)
                {
                    if(element1.number < 10 && element2.number < 10)
                        name = "D" + element1.number + element2.number;
                    else
                        name = "D" + element1.number + "T" + element2.number;
                }
            }
            else if(element2 == null)
            {
                if(element1 instanceof Element.Compartment)
                {
                    if(element1.number < 10)
                        name = "K" + element1.number + "0";
                    else
                        name = "K" + element1.number + "T0";
                }
                if(element1 instanceof Element.Delay)
                {
                    if(element1.number < 10)
                        name = "D" + element1.number + "0";
                    else
                        name = "D" + element1.number + "T0";                   
                }
            }
            else
            {
//                name = "U(" + element2.number + ")";
//                if(element2.number < 10)
//                    name = "K" + "0" + element2.number;
//                else
//                    name = "K" + "0T" + element2.number;
            }
            Graphics gc = (Graphics)gc2D;
            int labelWidth = gc.getFontMetrics().stringWidth(name);
            gc2D.setColor(Color.white);
            if(element1 == null)
            {                
                gc2D.fillRect(element2.xCenter + x0 - 47 - labelWidth / 2, element2.yCenter + y0 - 55, labelWidth, 12);
                gc2D.setColor(Color.black);
                gc2D.drawString(name, element2.xCenter + x0 - 47 - labelWidth / 2, element2.yCenter + y0 - 45);                 
            }
            else if(element2 == null)
            {                
                gc2D.fillRect(element1.xCenter + x0 - labelWidth / 2, element1.yCenter + y0 + 50, labelWidth, 12);
                gc2D.setColor(Color.black);
                gc2D.drawString(name, element1.xCenter + x0 - labelWidth / 2, element1.yCenter + y0 + 60);                 
            }
            else
            {
                int xCorner = (xEnd + xStart - labelWidth) / 2;
                int yCorner = (yEnd + yStart) /2 - 5;
                if(Math.abs(cos) < 0.5)
                {
                    if(yEnd > yStart)
                    {
                        yCorner += 7;
                        xCorner += (int)(15 * cos / sin);
                    }
                    else
                    {
                        yCorner -= 9;
                        xCorner -= (int)(9 * cos / sin);                    
                    }
                }
                gc2D.fillRect(xCorner - 2, yCorner, labelWidth + 4, 12);
                gc2D.setColor(Color.black);
                gc2D.drawString(name, xCorner, yCorner + 10);
            }
        }

        /** Starting element */
        protected Element element1;
        /** Ending element */
        protected Element element2;
        private int xStart, yStart, xEnd, yEnd;
        private double cos, sin, tan;
        private static final int shift = 10;
        /** Flow rate */
        protected Parameter flowRate;
//        protected String flowRate;
    }
    
    /** This class defines input. */
    protected static class Input extends Element
    {
        /** Create a new instance of input.
         * @param compartments compartments that the input applies to
         * @param xCenter x-coordinate of the input center.
         * @param yCenter y-coordinate of the input center.
         * @param model the model that this input belongs to.
         */
        public Input(Vector compartments, int xCenter, int yCenter, Model model)
        {
            super.model = model;
            this.compartments = compartments;
            if(compartments.size() == 1)
            {
                Element.Compartment compartment = (Element.Compartment)compartments.get(0);
                super.xCenter = compartment.xCenter - 33;
                super.yCenter = compartment.yCenter + 33;
            }
            else
            {
                super.xCenter = xCenter;
                super.yCenter = yCenter;
            }
            if(!Model.isClone)
            {
                number = newNumber(model.inputs);
                name = "Dose" + number;
            }
        }
        
        /** Draw input.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void draw(Graphics2D gc2D, int x0, int y0) 
        {
            gc2D.translate(xCenter + x0 - 14, yCenter + y0 - 14);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillPolygon(new int[]{-1, 24, 32, 6}, new int[]{22, -3, 4, 29}, 4);
            }
            gc2D.setColor(Color.BLUE);
            gc2D.drawLine(1, 24, 4, 27);
            gc2D.drawLine(2, 25, 8, 19);
            gc2D.drawLine(3, 26, 9, 20);                
            gc2D.drawLine(5, 16, 12, 23);
            gc2D.drawLine(7, 18, 19, 6);
            gc2D.drawLine(10, 21, 22, 9);
            gc2D.drawLine(19, 6, 22, 9);                 
            gc2D.drawLine(20, 7, 21, 7);
            gc2D.drawLine(21, 7, 21, 8);
            gc2D.drawLine(22, 6, 28, 0);
            gc2D.fillPolygon(new int[]{11, 19, 22, 14}, new int[]{14, 6, 9, 17}, 4);
            gc2D.translate(-xCenter - x0 + 14, -yCenter -y0 + 14);
            gc2D.translate(xCenter + x0, yCenter + y0);
            gc2D.setStroke(new BasicStroke(1.0f));
            for(int i = 0; i < compartments.size(); i++)
            {
                Element.Compartment compartment = (Element.Compartment)compartments.get(i);
                int x = compartment.xCenter - xCenter;
                int y = compartment.yCenter - yCenter;
                double d = Math.sqrt(x*x + y*y);
                gc2D.drawLine((int)(x*8/d), (int)(y*8/d), (int)(x - x*26/d), (int)(y - y*26/d));
            }
            gc2D.setStroke(new BasicStroke(2.0f));
            gc2D.translate(-xCenter -x0, -yCenter - y0);            
            drawName(gc2D, x0, y0);
        }
        
        /** Detect if (x,y) is within the element boundary.
         * @param x x-coordinate of the point.
         * @param y y-coordinate of the point.
         * @return true if the point is within the element boundary, false otherwise. 
         */        
        public boolean contains(int x, int y) 
        {
            Polygon area = new Polygon(new int[]{xCenter - 14, xCenter + 13, xCenter + 17, xCenter - 9}, 
                                       new int[]{yCenter + 9, yCenter - 16, yCenter - 11, yCenter + 14}, 4);
            return area.contains(x, y);
        }
        
        /** Draw boundary of input.
         * @param gc2D Graphics2D.
         */
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.BLUE);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.translate(xCenter - 14, yCenter - 14);
            gc2D.drawLine(1, 24, 4, 27);
            gc2D.drawLine(2, 25, 8, 19);
            gc2D.drawLine(3, 26, 9, 20);                
            gc2D.drawLine(5, 16, 12, 23);
            gc2D.drawLine(7, 18, 19, 6);
            gc2D.drawLine(10, 21, 22, 9);
            gc2D.drawLine(19, 6, 22, 9);                 
            gc2D.drawLine(20, 7, 21, 7);
            gc2D.drawLine(21, 7, 21, 8);
            gc2D.drawLine(22, 6, 28, 0);
            gc2D.translate(-xCenter + 14, -yCenter + 14);
            gc2D.setStroke(new BasicStroke());
        }        
        
        /** Draw name of input.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void drawName(Graphics2D gc2D, int x0, int y0)
        {
            gc2D.setColor(Color.BLUE);
            gc2D.translate(xCenter + x0 - 14, yCenter + y0 - 14);
            Graphics gc = (Graphics)gc2D;
            int nameWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, -nameWidth, 40);
            gc2D.translate(-xCenter - x0 + 14, -yCenter - y0 + 14);            
        }

        /** Compartments that the input applies to */
        protected Vector compartments = new Vector();
    }
    
    /** This class defines sample. */
    protected static class Sample extends Element
    {
        /** Create a new instance of sample.
         * @param compartments compartments that the sample applies to
         * @param xCenter x-coordinate of the sample center.
         * @param yCenter y-coordinate of the sample center.
         * @param model the model that this sample belongs to.
         */
        public Sample(Vector compartments, int xCenter, int yCenter, Model model)
        {
            super.model = model;
            this.compartments = compartments;
            if(compartments.size() == 1)
            {
                Element.Compartment compartment = (Element.Compartment)compartments.get(0);
                super.xCenter = compartment.xCenter + 40;
                super.yCenter = compartment.yCenter - 40;
            }
            else
            {
                super.xCenter = xCenter;
                super.yCenter = yCenter;
            }
            if(!Model.isClone)
            {
                number = newNumber(model.samples);
                name = "Obs" + number;
            }
        }
        
        /** Draw sample.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void draw(Graphics2D gc2D, int x0, int y0) 
        {
            gc2D.translate(xCenter + x0, yCenter + y0);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillOval(-6, -6, 12, 12);
            }
            gc2D.setColor(Color.red);
            for(int i = 0; i < compartments.size(); i++)
            {
                Element.Compartment compartment = (Element.Compartment)compartments.get(i);
                int x = compartment.xCenter - xCenter;
                int y = compartment.yCenter - yCenter;
                double d = Math.sqrt(x*x + y*y);
                gc2D.drawLine((int)(x*4/d), (int)(y*4/d), (int)(x - x*26/d), (int)(y - y*26/d));
                gc2D.drawOval(- 4, - 4, 8, 8);
            }
            gc2D.translate(-xCenter - x0, -yCenter - y0);
            drawName(gc2D, x0, y0);
        }
        
        /** Detect if (x,y) is within the element boundary.
         * @param x x-coordinate of the point.
         * @param y y-coordinate of the point.
         * @return true if the point is within the element boundary, false otherwise. 
         */       
        public boolean contains(int x, int y) 
        {
            return (x - xCenter) * (x - xCenter) + (y - yCenter) * (y - yCenter) <= 16;
        }
        
        /** Draw boundary of sample.
         * @param gc2D Graphics2D.
         */        
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.red);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawOval(xCenter - 4, yCenter - 4, 8, 8);
            gc2D.setStroke(new BasicStroke());
        }        
        
        /** Draw name of sample.
         * @param gc2D Graphics2D.
         * @param x0 left margin
         * @param y0 top margin
         */
        public void drawName(Graphics2D gc2D, int x0, int y0)
        {
            gc2D.translate(xCenter + x0, yCenter + y0);
            gc2D.setColor(Color.red);
            Graphics gc = (Graphics)gc2D;
            int nameWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, - nameWidth, - 10);
            gc2D.translate(-xCenter -x0, -yCenter - y0);
        }

        /** Compartments that the sample applies to */
        protected Vector compartments = new Vector();
        /** Error model */
        protected String errorModel = "";
    }

    /** X-coordinate of element center*/
    protected int xCenter;
    
    /** Y-coordinate of element center*/
    protected int yCenter;
    
    /** Element number */
    protected int number;
    
    /** Model comprised of these elements */
    protected Model model;
    
    /** Element name */
    protected String name;
}
