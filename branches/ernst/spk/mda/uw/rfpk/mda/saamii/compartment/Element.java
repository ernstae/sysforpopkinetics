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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Polygon;
import java.awt.BasicStroke;
import java.util.Vector;
import java.util.Properties;

/**
 *
 * @author  Jiaji Du
 */
public abstract class Element implements Cloneable
{
    public abstract void draw(Graphics2D gc2D);
    public abstract boolean contains(int x, int y);
    public abstract void drawBoundary(Graphics2D gc2D);
    public abstract void drawName(Graphics2D gc2D);
    
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

    protected static class Compartment extends Element
    {
        public Compartment(int x, int y, Model model)
        {
            super.xCenter = x;
            super.yCenter = y;
            super.model = model;
            number = newNumber(Model.elements);
            name = "q" + number;
        }
        
        public void draw(Graphics2D gc2D)
        {        
            gc2D.setColor(Color.black);
            gc2D.drawOval(xCenter - 25, yCenter -25, 50, 50);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillOval(xCenter - 24, yCenter -24, 48, 48);
            }
            drawName(gc2D);
        }
        
        public boolean contains(int x, int y) 
        {
            return (x - xCenter) * (x - xCenter) + (y - yCenter) * (y - yCenter) <= 625;
        }
        
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.black);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawOval(xCenter - 25, yCenter -25, 50, 50);
            gc2D.setStroke(new BasicStroke());
        }
        
        public void drawName(Graphics2D gc2D) 
        {
            gc2D.setColor(Color.black);
            Graphics gc = (Graphics)gc2D;
            int labelWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, xCenter - labelWidth, yCenter + 5);
        }
        
        protected String force, function;
    }
    
    protected static class Delay extends Element
    {
        public Delay(int x, int y, Model model)
        {
            super.xCenter = x;
            super.yCenter = y;
            super.model = model;
            number = newNumber(Model.elements);
            name = "d" + number;
            addVariable();
        }
        
        public void draw(Graphics2D gc2D) 
        {            
            gc2D.translate(xCenter, yCenter);
            if(this == model.selectedElement)
            {
                gc2D.setColor(Color.yellow);
                gc2D.fillRect(-21, -16, 44, 32);
            }
            gc2D.setColor(Color.black);
            gc2D.drawRect(-23, -17, 46, 34);
            gc2D.drawLine(-23, 0, -16, 0);
            gc2D.drawOval(-16, -4, 8, 8); 
            gc2D.drawLine(-8, 0, -4, 0);
            gc2D.drawOval(-4, -4, 8, 8); 
            gc2D.drawLine(4, 0, 8, 0);
            gc2D.drawOval(8, -4, 8, 8); 
            gc2D.drawLine(16, 0, 23, 0);
            gc2D.translate(-xCenter, -yCenter);
            drawName(gc2D);
        }
        
        public boolean contains(int x, int y) 
        {
            return Math.abs(x - xCenter) <= 23 &&  Math.abs(y - yCenter) <= 17;
        }

        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.black);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawRect(xCenter - 23, yCenter - 17, 46, 34);
            gc2D.setStroke(new BasicStroke());
        }        
        
        public void drawName(Graphics2D gc2D)
        {
            gc2D.setColor(Color.black);
            Graphics gc = (Graphics)gc2D;
            int labelWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, xCenter - labelWidth, yCenter - 7);            
        }
        
        protected String time;
        protected Vector compartments = new Vector();
    }
    
    protected static class Flux extends Element
    {
        public Flux(Element element1, Element element2, Model model)
        {
            super.model = model;
            this.element1 = element1;
            this.element2 = element2;
            if(element1 != null && element2 != null)
            {
                if(element1 instanceof Element.Compartment) 
                    name = "k(" + element2.number + "," + element1.number + ")";
                if(element1 instanceof Element.Delay)
                {
                    name = "d(" + element2.number + "," + element1.number + ")";
                    ((Element.Delay)element1).compartments.add(element2);
                }
            }
            else if(element2 == null)
            {
                if(element1 instanceof Element.Compartment) name = "k(0," + element1.number + ")";
                if(element1 instanceof Element.Delay) name = "d(0," + element1.number + ")";
            }
            else
                name = "U(" + element2.number + ")";
            addVariable();
        }
        
        public void draw(Graphics2D gc2D) 
        {
            int x1 = element1 != null? element1.xCenter : 0;
            int y1 = element1 != null? element1.yCenter : 0;
            int x2 = element2 != null? element2.xCenter : 0;
            int y2 = element2 != null? element2.yCenter : 0;
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
                drawName(gc2D);               
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
                drawName(gc2D);               
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
            drawName(gc2D);
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
        
        public boolean contains(int x, int y) 
        {
            Polygon area = new Polygon(new int[]{xStart - (int)(4 * sin), xStart + (int)(4 * sin), 
                                                 xEnd + (int)(4 * sin), xEnd - (int)(4 * sin)},
                                       new int[]{yStart + (int)(4 * cos), yStart - (int)(4 * cos), 
                                                 yEnd - (int)(4 * cos), yEnd + (int)(4 * cos)}, 4);                            
            return area.contains(x, y);
        }
 
        public void drawBoundary(Graphics2D gc2D)
        {

        }
        
        public void drawName(Graphics2D gc2D)
        {
            Graphics gc = (Graphics)gc2D;
            int labelWidth = gc.getFontMetrics().stringWidth(name);
            gc2D.setColor(Color.white);
            if(element1 == null)
            {                
                gc2D.fillRect(element2.xCenter - 47 - labelWidth / 2, element2.yCenter - 55, labelWidth, 12);
                gc2D.setColor(Color.black);
                gc2D.drawString(name, element2.xCenter - 47 - labelWidth / 2, element2.yCenter - 45);                 
            }
            else if(element2 == null)
            {                
                gc2D.fillRect(element1.xCenter - labelWidth / 2, element1.yCenter + 50, labelWidth, 12);
                gc2D.setColor(Color.black);
                gc2D.drawString(name, element1.xCenter - labelWidth / 2, element1.yCenter + 60);                 
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
                
        protected Element element1, element2;
        private int xStart, yStart, xEnd, yEnd;
        private double cos, sin, tan;
        private static final int shift = 10;
        protected String flowRate;
    }
    
    protected static class Input extends Element
    {
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
                name = "ex" + number;                
            }
            addVariable();
        }
        
        public void draw(Graphics2D gc2D) 
        {
            gc2D.translate(xCenter - 14, yCenter - 14);
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
            gc2D.translate(-xCenter + 14, -yCenter + 14);
            gc2D.translate(xCenter, yCenter);
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
            gc2D.translate(-xCenter, -yCenter);            
            drawName(gc2D);
        }
        
        public boolean contains(int x, int y) 
        {
            Polygon area = new Polygon(new int[]{xCenter - 14, xCenter + 13, xCenter + 17, xCenter - 9}, 
                                       new int[]{yCenter + 9, yCenter - 16, yCenter - 11, yCenter + 14}, 4);
            return area.contains(x, y);
        }
        
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
        
        public void drawName(Graphics2D gc2D)
        {
            gc2D.setColor(Color.BLUE);
            gc2D.translate(xCenter - 14, yCenter - 14);
            Graphics gc = (Graphics)gc2D;
            int nameWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, -nameWidth, 40);
            gc2D.translate(-xCenter + 14, -yCenter + 14);            
        }
        
        protected Vector compartments = new Vector();
    }
    
    protected static class Sample extends Element
    {
        public Sample(Vector compartments, int xCenter, int yCenter, Model model)
        {
            super.model = model;
            this.compartments = compartments;
            if(!Model.isClone)
            {
                number = newNumber(model.samples);
                name = "s" + number;
            }
            this.xCenter = xCenter;
            this.yCenter = yCenter;
            addVariable();
        }
        
        public void draw(Graphics2D gc2D) 
        {
            gc2D.translate(xCenter, yCenter);
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
                gc2D.drawOval(-4, -4, 8, 8);
                if(isAssociatedWithData)
                    gc2D.fillOval(-4, -4, 8, 8);
            }
            gc2D.translate(-xCenter, -yCenter);
            drawName(gc2D);
        }
        
        public boolean contains(int x, int y) 
        {
            return (x - xCenter) * (x - xCenter) + (y - yCenter) * (y - yCenter) <= 16;
        }
        
        public void drawBoundary(Graphics2D gc2D)
        {
            gc2D.setColor(Color.red);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawOval(xCenter - 4, yCenter - 4, 8, 8);
            gc2D.setStroke(new BasicStroke());
        }        
        
        public void drawName(Graphics2D gc2D)
        {
            gc2D.translate(xCenter, yCenter);
            gc2D.setColor(Color.red);
            Graphics gc = (Graphics)gc2D;
            int nameWidth = gc.getFontMetrics().stringWidth(name)/2;
            gc2D.drawString(name, - nameWidth, - 10);
            gc2D.translate(-xCenter, -yCenter);
        }
        
        protected Vector compartments = new Vector();
        protected boolean isAssociatedWithData = false;
        protected String associatedDataName;
    }
    
    protected void addVariable()
    {
        Model.variables.setProperty(name, "?");
    }

    protected int xCenter, yCenter, number;
    protected Model model;
    protected String name, units, equations;
    protected String[] variables = {""};
}
