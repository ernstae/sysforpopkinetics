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
package uw.rfpk.mda.nonmem.display;

import uw.rfpk.mda.nonmem.Utility;
import javax.swing.JPanel; 
import javax.swing.JOptionPane;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Font; 
import java.awt.BasicStroke; 
import java.awt.Dimension;
import java.awt.geom.*;
import java.awt.Point;
import java.awt.print.*;
import java.text.DecimalFormat;
import java.awt.event.MouseEvent;
import javax.print.attribute.*;

/** 
 * This class implements a XY plotter.
 * @author  Jiaji Du
 */
public class Plotter extends JPanel
{
    /** Create a new Plotter JPanel and save information about
     * the data values and the labeling.
     * @param dataX a double[][] containing X data values for a number(dataX.length) of curves
     * @param dataY a double[][] containing Y data values for a number(dataY.length) of curves
     * @param title the title of the data plot
     * @param titleX the title of the x axis.
     * @param titleY the title of the y axis.
     * @param xLine the flag specifies if x = 0 line is required.
     * @param yLine the flag specifies if y = 0 line is required.
     * @param uLine the flag specifies if unit slope line is required.
     * @param type format of presentation: "dots", "line" or "both".
     */
    public Plotter(double[][] dataX, double[][] dataY, String title, String titleX, String titleY, 
                   String type, boolean xLine, boolean yLine, boolean uLine) 
    {
	this.dataX = dataX;
        this.dataY = dataY;
        this.title = title;
        this.titleX = titleX;
        this.titleY = titleY;
        this.type = type;
        this.xLine = xLine;
        this.yLine = yLine;
        this.uLine = uLine;
        initComponents(); 
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jPopupMenu1 = new javax.swing.JPopupMenu();
        jMenuItem1 = new javax.swing.JMenuItem();

        jMenuItem1.setText("Print");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem1);

        setLayout(new java.awt.BorderLayout());

        addMouseListener(new java.awt.event.MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent evt) {
                formMousePressed(evt);
            }
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                formMouseReleased(evt);
            }
        });

    }//GEN-END:initComponents

    private void formMouseReleased(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_formMouseReleased
        if (evt.isPopupTrigger()) 
            jPopupMenu1.show(evt.getComponent(), evt.getX(), evt.getY());
    }//GEN-LAST:event_formMouseReleased

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
	// Get a PrinterJob object
        PrinterJob printerJob = PrinterJob.getPrinterJob();
        // Set what to print
        printerJob.setPrintable(new Printer());

        PrintRequestAttributeSet attributes = new HashPrintRequestAttributeSet();   
        if(printerJob.printDialog(attributes))
        { 
            try
	    {
                printerJob.print(attributes);
            }
            catch(PrinterException pe)
	    {
                JOptionPane.showMessageDialog(null, "Error printing " + pe,  // Display printing 
                                              "Printer Error",               // error message
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void formMousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_formMousePressed
        if (evt.isPopupTrigger()) 
            jPopupMenu1.show(evt.getComponent(), evt.getX(), evt.getY()); 
    }//GEN-LAST:event_formMousePressed

    /**
     * Repaint the JPanel with all the desired information.  The plot
     * includes x and y axes, a data curve, labels showing the max x
     * and y values, and a title label.
     * @param gc the graphics context provided by the system.
     */
    public void paintComponent(Graphics gc) 
    {
        // ask the superclass to do its work first, then set our 
        // basic color scheme
        super.paintComponent(gc);
        setBackground(bg);
        setForeground(fg);
        
        Graphics2D gc2D = ((Graphics2D)gc);
        
	// determine size of our display
        Dimension d = getSize();
        
	// draw axes
        int width = d.width - rightInset - leftInset;
        int height = d.height - bottomInset - topInset;
        width = width / 25 * 25;
        height = height / 25 * 25;      
        int top = d.height - bottomInset - height;
	gc2D.drawRect(leftInset, top, width, height); 
        
        // determine the data ranges
        maxX = dataX[0][0];
        maxY = dataY[0][0];
        minX = dataX[0][0];
        minY = dataY[0][0];

        for(int i = 0; i < dataX.length; i++)
        {
            for(int j = 1; j < dataX[i].length; j++)
            {
                maxX = Math.max(maxX, dataX[i][j]);
                maxY = Math.max(maxY, dataY[i][j]);
                minX = Math.min(minX, dataX[i][j]);
                minY = Math.min(minY, dataY[i][j]);
            }
        }

        if(maxX == minX)
        {
            maxX += 1;
            minX -= 1;
        }
        
        if(maxY == minY)
        {
            maxY += 1;
            minY -= 1;
        }
        
        int p = 0;
        
        if(Math.abs(maxX) > Math.abs(minX))
            p = (int)(Math.log(maxX)/Math.log(10)); 
        else
            p = (int)(Math.log(minX)/Math.log(10));             
        maxX = (double)((int)(maxX / Math.pow(10, p - 1)) + 1) * Math.pow(10, p - 1); 
        minX = (double)((int)(minX / Math.pow(10, p - 1)) - 1) * Math.pow(10, p - 1);
        
        if(Math.abs(maxY) > Math.abs(minY))
            p = (int)(Math.log(maxY)/Math.log(10)); 
        else
            p = (int)(Math.log(minY)/Math.log(10));             
        maxY = (double)((int)(maxY / Math.pow(10, p - 1)) + 1) * Math.pow(10, p - 1); 
        minY = (double)((int)(minY / Math.pow(10, p - 1)) - 1) * Math.pow(10, p - 1); 
        
        double spanX = maxX - minX;        
        double spanY = maxY - minY;
	         
	// draw the data curve
        newX = new int[dataX.length][dataX[0].length];
        newY = new int[dataY.length][dataY[0].length];

        for (int i = 0; i < dataX.length; i++)
        {	
            if(i == 0) gc2D.setColor(Color.red);
            if(i == 1) gc2D.setColor(Color.yellow);
            if(i == 2) gc2D.setColor(Color.blue);            
            for (int j = 0; j < dataX[i].length; j++)
            {
	        newX[i][j] = (int)(leftInset + (dataX[i][j] - minX)/spanX * width); 
                newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
            }
            if(type.equals("dots") || type.equals("both"))
            {
                for (int j = 0; j < dataX[i].length; j++)
                {                
                    Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8); 
                    gc2D.draw(circle);
                    gc2D.fill(circle);
                }
            }
            if(type.equals("line") || type.equals("both"))
                gc2D.drawPolyline(newX[i], newY[i], dataX[i].length);
        }
	
	// draw labels
        gc2D.setFont(lf);
        gc2D.setColor(Color.black);
        DecimalFormat f = new DecimalFormat("0.00E00");
        for(int i = 0; i < 6; i++)
        { 
            String value = Utility.formatData(6, f.format(minX + spanX*i/5));
            gc2D.drawString(value, 
                            leftInset + width*i/5 - gc.getFontMetrics().stringWidth(value)/2, 
                            top + height + 18); 
            value = Utility.formatData(6, f.format(maxY - spanY*i/5));
            gc2D.drawString(value, 
                            leftInset - gc.getFontMetrics().stringWidth(value) - 2, 
                            top + height*i/5 + 5);            
        }
        
        // draw x = 0 line, y = 0 line, slope = 1 line
        gc2D.setColor(Color.green);
        if(xLine && minX < 0 && maxX > 0)
            gc2D.drawLine(leftInset - (int)(minX * width / spanX), top, 
                          leftInset - (int)(minX * width / spanX), top + height);   
        if(yLine && minY < 0 && maxY > 0)
            gc2D.drawLine(leftInset,         top + (int)(maxY * height / spanY), 
                          leftInset + width, top + (int)(maxY * height / spanY)); 
        if(uLine)
        {
            double point1 = 0;
            double point2 = 0;
            if(maxX > minY && maxY > minX)
            {
                if(maxX > maxY) 
                    point1 = maxY;
                else
                    point1 = maxX;
                if(minX > minY) 
                    point2 = minX;
                else
                    point2 = minY;
            }
            gc2D.drawLine(leftInset + (int)((point1 - minX) * width / spanX), 
                          top + height - (int)((point1 - minY) * height / spanY), 
                          leftInset + (int)((point2 - minX) * width / spanX), 
                          top + height - (int)((point2 - minY) * height / spanY));
        }

        // draw grid
        gc2D.setColor(Color.gray);        
        gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                       BasicStroke.JOIN_BEVEL, 0, 
                                       new float[]{5.0f, 2.0f}, 0));  
        for(int i = 1; i < 5; i++)
        {
            gc2D.drawLine(leftInset + i * width/5, top, 
                          leftInset + i * width/5, top + height);   
            gc2D.drawLine(leftInset,               top + i * height/5, 
                          leftInset + width,       top + i * height/5);             
        }

        // draw ticks
        gc2D.setColor(Color.black); 
        gc2D.setStroke(new BasicStroke());
        int spacingX = width/25;
        int spacingY = height/25;
        for(int i = 1; i < 26; i++)
        {
            gc2D.drawLine(leftInset + i * spacingX, top, 
                          leftInset + i * spacingX, top + 6);
            gc2D.drawLine(leftInset + i * spacingX, top + height - 6, 
                          leftInset + i * spacingX, top + height);
            gc2D.drawLine(leftInset,                top + i * spacingY, 
                          leftInset + 6,            top + i * spacingY);
            gc2D.drawLine(leftInset + width - 6,    top + i * spacingY, 
                          leftInset + width,        top + i * spacingY);            
        }
                
	// draw titles
        gc2D.setColor(Color.black);
        int titleWidth = gc.getFontMetrics().stringWidth(title); 
        gc2D.drawString(title, leftInset + (width - titleWidth)/2, top - 10);
        gc2D.setFont(tf);
        int titleXWidth = gc.getFontMetrics().stringWidth(titleX);
        int titleYWidth = gc.getFontMetrics().stringWidth(titleY);        
        gc2D.drawString(titleX, leftInset + (width - titleXWidth)/2, top + height + 40);      
        gc2D.rotate(Math.PI/2);
        String[] titleYs = titleY.split(" ");
        int shift = 0;
        for(int i = 0; i < dataY.length; i++)
        {
            if(i == 0) gc2D.setColor(Color.red);
            if(i == 1)
            {
                gc2D.setColor(Color.yellow);
                shift = gc.getFontMetrics().stringWidth(titleYs[0] + " ");
            }
            if(i == 2)
            {
                gc2D.setColor(Color.blue);
                shift += gc.getFontMetrics().stringWidth(titleYs[1] + " ");
            }
	    gc2D.drawString(titleYs[i], top + (height - titleYWidth)/2 + shift, -16);
        }
    }

    /** This method specifies the tooltip to display.
     * @param e MouseEvent.
     * @return a String object as the tooltip.
     */    
    public String getToolTipText(MouseEvent e) 
    {
        String toolTip = null;
        Point mousePoint = e.getPoint();
        for (int i = 0; i < dataX.length; i++)
        {	
            for (int j = 0; j < dataX[i].length; j++)
            {
                if(mousePoint.distance((double)newX[i][j], (double)newY[i][j]) <= 4) 
                {
                    toolTip = String.valueOf(dataX[i][j]) + ", " + String.valueOf(dataY[i][j]);
                    break;
                }
            }   
        }       
        return toolTip;    
    }
    
    private class Printer implements Printable{
        
        public int print(Graphics gc, PageFormat pageFormat, int pageIndex) throws PrinterException {
            if(pageIndex != 0)
                return NO_SUCH_PAGE; 
            Graphics2D gc2D = ((Graphics2D)gc); 
            
            // Get upper-left corner coordinates
            int lineInsetX  = (int)pageFormat.getImageableX();
            int lineInsetY  = (int)pageFormat.getImageableY();
            
            // Get printing area dimensions
            Dimension d  = new Dimension((int)pageFormat.getImageableWidth(),
                                         (int)pageFormat.getImageableHeight());
            
            // Adjust size to fit the printing area
            if((double)d.width / (double)d.height <= 1.25)
                d.height = (int)(d.width / 1.25);
            else
                d.width = (int)(d.height * 1.25);
            
            // draw axes
            int width = d.width - rightInset - leftInset;
            int height = d.height - bottomInset - topInset;
            width = width / 25 * 25;
            height = height / 25 * 25;      
            int top = d.height - bottomInset - height + lineInsetY;
            int left = leftInset + lineInsetX;
	    gc2D.drawRect(left, top, width, height); 
            
            // draw the data curve
            double spanX = maxX - minX;        
            double spanY = maxY - minY;

            for (int i = 0; i < dataX.length; i++)
            {
                if(i == 0) gc2D.setColor(Color.red);
                if(i == 1) gc2D.setColor(Color.yellow);
                if(i == 2) gc2D.setColor(Color.blue);                 
                for (int j = 0; j < dataX[i].length; j++)
                {
	            newX[i][j] = (int)(left + (dataX[i][j] - minX)/spanX * width); 
                    newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
                }          
                if(type.equals("dots") || type.equals("both"))
                {
                    for (int j = 0; j < dataX[i].length; j++)
                    {
                        Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8);
                        gc2D.draw(circle);
                        gc2D.fill(circle);
                    }
                }
                if(type.equals("line") || type.equals("both"))
                    gc2D.drawPolyline(newX[i], newY[i], dataX[i].length); 
            }
            
            // draw labels
            gc2D.setFont(lf);
            gc2D.setColor(Color.black);
            DecimalFormat f = new DecimalFormat("0.00E00");
            for(int i = 0; i < 6; i++)
            {
                String value = Utility.formatData(6, f.format(minX + spanX*i/5));
                gc2D.drawString(value, 
                                left + width*i/5 - gc.getFontMetrics().stringWidth(value)/2, 
                                top + height + 18); 
                value = Utility.formatData(6, f.format(maxY - spanY*i/5));
                gc2D.drawString(value, 
                                left - gc.getFontMetrics().stringWidth(value) - 2, 
                                top + height*i/5 + 5);            
            }
            
            // draw x = 0 line, y = 0 line, slope = 1 line
            gc2D.setColor(Color.green);
            if(xLine && minX < 0 && maxX > 0)
                gc2D.drawLine(left - (int)(minX * width / spanX), top, 
                              left - (int)(minX * width / spanX), top + height);   
            if(yLine && minY < 0 && maxY > 0)
                gc2D.drawLine(left,         top + (int)(maxY * height / spanY), 
                              left + width, top + (int)(maxY * height / spanY)); 
            if(uLine)
            {
                double point1 = 0;
                double point2 = 0;
                if(maxX > minY && maxY > minX)
                {
                    if(maxX > maxY) 
                        point1 = maxY;
                    else
                        point1 = maxX;
                    if(minX > minY) 
                        point2 = minX;
                    else
                        point2 = minY;
                }
                gc2D.drawLine(left + (int)((point1 - minX) * width / spanX), 
                              top + height - (int)((point1 - minY) * height / spanY), 
                              left + (int)((point2 - minX) * width / spanX), 
                              top + height - (int)((point2 - minY) * height / spanY));
            }
            
            // draw grid
            gc2D.setColor(Color.gray);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));  
            for(int i = 1; i < 5; i++)
            {
                gc2D.drawLine(left + i * width/5, top, 
                              left + i * width/5, top + height);   
                gc2D.drawLine(left,               top + i * height/5, 
                              left + width,       top + i * height/5);             
            }

            // draw ticks
            gc2D.setColor(Color.black);                               
            gc2D.setStroke(new BasicStroke());
            int spacingX = width/25;
            int spacingY = height/25;
            for(int i = 1; i < 26; i++)
            {
                gc2D.drawLine(left + i * spacingX, top, 
                              left + i * spacingX, top + 6);
                gc2D.drawLine(left + i * spacingX, top + height - 6, 
                              left + i * spacingX, top + height);
                gc2D.drawLine(left,                top + i * spacingY, 
                              left + 6,            top + i * spacingY);
                gc2D.drawLine(left + width - 6,    top + i * spacingY, 
                              left + width,        top + i * spacingY);  
            }
                
	    // draw titles
            int titleWidth = gc.getFontMetrics().stringWidth(title); 
            gc2D.drawString(title, left + (width - titleWidth)/2, top - 10);
            gc2D.setFont(tf);
            int titleXWidth = gc.getFontMetrics().stringWidth(titleX);
            int titleYWidth = gc.getFontMetrics().stringWidth(titleY);
            gc2D.drawString(titleX, left + (width - titleXWidth)/2, top + height + 40);        
            gc2D.rotate(Math.PI/2);            
            String[] titleYs = titleY.split(" ");
            int shift = 0;
            for(int i = 0; i < dataY.length; i++)
            {            
                if(i == 0) gc2D.setColor(Color.red);
                if(i == 1)
                {
                    gc2D.setColor(Color.yellow);
                    shift = gc.getFontMetrics().stringWidth(titleYs[0] + " ");
                }
                if(i == 2)
                {
                    gc2D.setColor(Color.blue);
                    shift += gc.getFontMetrics().stringWidth(titleYs[1] + " ");
                }
                gc2D.drawString(titleYs[i], top + (height - titleYWidth)/2 + shift, -16 - lineInsetX);
            }
            return PAGE_EXISTS; 
        } 
        
    }
    
    //-----------------------------------------------------------------------
	
    private double[][] dataX;
    private double[][] dataY;
    private double maxX;
    private double maxY;
    private double minX;
    private double minY;
    private int[][] newX;
    private int[][] newY;    
    private String title; 
    private String titleX;
    private String titleY;
    private String type;
    private boolean xLine, yLine, uLine;
    private final static Color bg = Color.white;
    private final static Color fg = Color.black;
    private final static int leftInset = 100;
    private final static int rightInset = 20;
    private final static int topInset = 30;
    private final static int bottomInset = 60;
    private final static Font lf = new Font("SansSerif", Font.BOLD, 12);
    private final static Font tf = new Font("SansSerif", Font.BOLD, 16);
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JPopupMenu jPopupMenu1;
    // End of variables declaration//GEN-END:variables
    
}