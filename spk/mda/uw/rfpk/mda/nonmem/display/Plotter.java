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
import java.awt.Polygon;
/** 
 * This class implements a XY plotter.
 * @author  Jiaji Du
 */
public class Plotter extends JPanel
{
    /** Create a new Plotter Jpanel */
    public Plotter(){}
    
    /** Create a new Plotter JPanel and save information about
     * the data values and the labeling.
     * @param dataX a double[][] containing X data values for a number(dataX.length) of curves
     * @param dataY a double[][] containing Y data values for a number(dataY.length) of curves
     * @param title the title of the data plot
     * @param labelX the label for the x axis.
     * @param labelY the label for the y axis.
     * @param name the name of the curve.
     * @param xLine the flag specifies if x = 0 line is required.
     * @param yLine the flag specifies if y = 0 line is required.
     * @param uLine the flag specifies if unit slope line is required.
     * @param type the symbol types of the curves in an int array. Each element represents:  
     *        "dot", "circle", "filled square", "square", "filled up triangle", "up triangle",
     *        "filled down triangle", "down triangle", "cross", "check mark", "thick solid line",
     *        "thin solid line" or "dashed line".
     * @param color the colors of the curves in a Color array. Each element represents: 
     *        "red", "yellow", "orange", "blue", "pink", "magenta", "cyan", "green", "gray", or "black".
     * @param addedLineColor the colors of the added lines in a Color array. Each element represents: 
     *        "red", "yellow", "orange", "blue", "pink", "magenta", "cyan", "green", "gray", or "black".
     * @param legendLocation the location of the legend: "Inside", "Top", "Right", or null if
     *        not showing the legend.
     * @param showHorizontalGrid true for horizontal grid lines are to be shown, false otherwise.
     * @param showVerticalGrid true for vertical grid lines are to be shown, false otherwise.
     * @param showTicksOnX true for ticks on X axis are to be shown, false otherwise. 
     * @param showTicksOnY true for ticks on Y axis are to be shown, false otherwise.
     * @param maxX the upper bound of X.
     * @param minX the lower bound of X.
     * @param maxY the upper bound of Y.
     * @param minY the lower bound of Y.
     */
    public Plotter(double[][] dataX,
                   double[][] dataY,
                   String title,
                   String labelX,
                   String labelY,
                   String[] name,
                   int[] type,
                   Color[] color,
                   boolean xLine,
                   boolean yLine,
                   boolean uLine,
                   Color[] addedLineColor,
                   String legendLocation,
                   boolean showHorizontalGrid,
                   boolean showVerticalGrid,
                   boolean showTicksOnX,
                   boolean showTicksOnY,
                   double maxX,
                   double minX,
                   double maxY,
                   double minY)
    {
	this.dataX = dataX;
        this.dataY = dataY;
        this.title = title;
        this.labelX = labelX;
        this.labelY = labelY;
        this.name = name;
        this.type = type;
        this.color = color;
        this.xLine = xLine;
        this.yLine = yLine;
        this.uLine = uLine;
        this.addedLineColor = addedLineColor;
        this.legendLocation = legendLocation;
        this.showHorizontalGrid = showHorizontalGrid;
        this.showVerticalGrid = showVerticalGrid;
        this.showTicksOnX = showTicksOnX;
        this.showTicksOnY = showTicksOnY;
        this.maxX = maxX;
        this.minX = minX;        
        this.maxY = maxY;
        this.minY = minY;
        if(legendLocation != null)
            start = null;
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
        addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
            public void mouseDragged(java.awt.event.MouseEvent evt) {
                formMouseDragged(evt);
            }
        });

    }//GEN-END:initComponents

    private void formMouseDragged(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_formMouseDragged
        if(legendLocation != null && (evt.getModifiers() & evt.BUTTON1_MASK) != 0 && start != null)
        {            
            last = evt.getPoint();
            repaint((int)legendArea.x, (int)legendArea.y, 
                    (int)legendArea.width + 1, (int)legendArea.height + 1);
            legendArea.setRect(legendArea.x + last.getX() - start.getX(), 
                               legendArea.y + last.getY() - start.getY(), 
                               legendArea.width, legendArea.height);            
            start = last;
            repaint((int)legendArea.x - 1, (int)legendArea.y - 1, 
                    (int)legendArea.width + 2, (int)legendArea.height + 2);      
        }
    }//GEN-LAST:event_formMouseDragged

    private void formMouseReleased(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_formMouseReleased
        if(legendLocation != null && (evt.getModifiers() & evt.BUTTON1_MASK) != 0 && start != null)
        {
            last = null;
            gc2D.setPaintMode();
            repaint();           
        }
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
        start = evt.getPoint();
        if(legendLocation != null && (evt.getModifiers() & evt.BUTTON1_MASK) != 0 && legendArea.contains(start))
            gc2D.setXORMode(bg);
        if(evt.isPopupTrigger()) 
            jPopupMenu1.show(evt.getComponent(), evt.getX(), evt.getY());
    }//GEN-LAST:event_formMousePressed

    /**
     * Repaint the JPanel with all the desired information.  The plot
     * includes x and y axes, data curve,s labels showing the max x
     * and y values, and a title label.
     * @param gc the graphics context provided by the system.
     */
    public void paintComponent(Graphics gc) 
    {
        // Ask the superclass to do its work first, then set our 
        // basic color scheme
        super.paintComponent(gc);
        setBackground(bg);
        setForeground(fg);
        
        gc2D = ((Graphics2D)gc);
        gc2D.setPaintMode();
        
	// Determine size of our display
        Dimension d = getSize();
        int width = 0;
        int height = 0;
        int top = 0;
        
        // Determine size and location of legend
        double legendWidth = 0;
        double legendHeight = 0;
        if(legendLocation != null)
        {
            gc2D.setFont(lf);
            int nameWidth = gc.getFontMetrics().stringWidth(name[0]);
            for(int i = 0; i < name.length; i++)
            {
                if(gc.getFontMetrics().stringWidth(name[i]) > nameWidth)
                    nameWidth = gc.getFontMetrics().stringWidth(name[i]);
            }
            legendWidth = nameWidth + 54;
            legendHeight = 8 + name.length * 12;

            if(legendLocation.equals("Inside"))
            {
                width = d.width - rightInset - leftInset;
                height = d.height - bottomInset - topInset;
                width = width / 25 * 25;
                height = height / 25 * 25;
                top = d.height - bottomInset - height;
                legendX = leftInset + 5;
                legendY = top + 5;
            }
            
            if(legendLocation.equals("Top"))
            {
                width = d.width - rightInset - leftInset;            
                height = d.height - bottomInset - topInset - (int)legendHeight - 5;
                width = width / 25 * 25;
                height = height / 25 * 25;
                top = d.height - bottomInset - height;
                legendX = leftInset + width / 2 - legendWidth / 2;
                legendY = top - legendHeight - 5;
            }
            
            if(legendLocation.equals("Right"))
            {
                width = d.width - rightInset - leftInset - (int)legendWidth + 5;
                height = d.height - bottomInset - topInset;
                width = width / 25 * 25;
                height = height / 25 * 25;
                top = d.height - bottomInset - height;
                legendX = leftInset + width + 5;
                legendY = top + height / 2 - legendHeight / 2;
            }
        }
        else
        {
            width = d.width - rightInset - leftInset;
            height = d.height - bottomInset - topInset;
            width = width / 25 * 25;
            height = height / 25 * 25;
            top = d.height - bottomInset - height;
        }
        
	// Draw axes
	gc2D.drawRect(leftInset, top, width, height); 
        
        // Determine the data ranges
        double[] range = new double[2];
        if(maxX ==0 && minX == 0)
        {
            range = getDefaultRange(dataX);
            minX = range[0];
            maxX = range[1];
        }
        if(maxY == 0 && minY == 0)
        {
            range = getDefaultRange(dataY);
            minY = range[0];
            maxY = range[1];
        }
        range = null;
        
        double spanX = maxX - minX;        
        double spanY = maxY - minY;
        
        // Draw grid
        gc2D.setColor(Color.gray);
        gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                       BasicStroke.JOIN_BEVEL, 0, 
                                       new float[]{5.0f, 2.0f}, 0));  
        
        if(showVerticalGrid)                               
            for(int i = 1; i < 5; i++)
                gc2D.drawLine(leftInset + i * width/5, top, 
                              leftInset + i * width/5, top + height);
        if(showHorizontalGrid)                               
            for(int i = 1; i < 5; i++)                                       
                gc2D.drawLine(leftInset,               top + i * height/5, 
                              leftInset + width,       top + i * height/5);             

        // Draw ticks
        gc2D.setColor(Color.black); 
        gc2D.setStroke(new BasicStroke());
        int spacingX = width/25;
        int spacingY = height/25;
        int tickSize = 4;
        for(int i = 1; i < 26; i++)
        {
            if(i % 5 == 0)
                tickSize = 6;
            else
                tickSize = 4;
            if(!showTicksOnX && tickSize == 4)
                continue;
            gc2D.drawLine(leftInset + i * spacingX,     top, 
                          leftInset + i * spacingX,     top + tickSize);
            gc2D.drawLine(leftInset + i * spacingX,     top + height - tickSize, 
                          leftInset + i * spacingX,     top + height);
           
            if(!showTicksOnY && tickSize == 4)
                continue;
            gc2D.drawLine(leftInset,                    top + i * spacingY, 
                          leftInset + tickSize,         top + i * spacingY);
            gc2D.drawLine(leftInset + width - tickSize, top + i * spacingY, 
                          leftInset + width,            top + i * spacingY);
        }
        
        // Draw x = 0 line, y = 0 line, slope = 1 line
        if(xLine && minX < 0 && maxX > 0)
        {
            gc2D.setColor(addedLineColor[0]);
            gc2D.drawLine(leftInset - (int)(minX * width / spanX), top, 
                          leftInset - (int)(minX * width / spanX), top + height);
        }
        if(yLine && minY < 0 && maxY > 0)
        {
            gc2D.setColor(addedLineColor[1]);
            gc2D.drawLine(leftInset,         top + (int)(maxY * height / spanY), 
                          leftInset + width, top + (int)(maxY * height / spanY));
        }
        if(uLine)
        {
            gc2D.setColor(addedLineColor[2]);
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
                	         
	// Draw the data curve
        newX = new int[dataX.length][];
        newY = new int[dataY.length][];

        for(int i = 0; i < dataX.length; i++)
        {
            newX[i] = new int[dataX[i].length];
            newY[i] = new int[dataY[i].length];
            gc2D.setColor(color[i]);          
            for(int j = 0; j < dataX[i].length; j++)
            {
	        newX[i][j] = (int)(leftInset + (dataX[i][j] - minX)/spanX * width); 
                newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
            }
            drawCurve(gc2D, type[i], i);
        }
	
	// Draw labels
        gc2D.setFont(pf);
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

	// Draw titles
        gc2D.setColor(Color.black);
        gc2D.setFont(tf);
        int titleWidth = gc.getFontMetrics().stringWidth(title);
        if(legendLocation.equals("Top"))
            gc2D.drawString(title, leftInset + (width - titleWidth)/2, top - (int)legendHeight - 10);
        else
            gc2D.drawString(title, leftInset + (width - titleWidth)/2, top - 10);
        int labelXWidth = gc.getFontMetrics().stringWidth(labelX);
        int labelYWidth = gc.getFontMetrics().stringWidth(labelY);        
        gc2D.drawString(labelX, leftInset + (width - labelXWidth)/2, top + height + 40);      
        gc2D.rotate(Math.PI/2);
        gc2D.drawString(labelY, top + (height - labelYWidth)/2, -16);
        gc2D.rotate(Math.PI/-2);
       
        // Draw legend
        if(legendLocation != null)
        {
            if(start == null)
                legendArea = new Rectangle2D.Double(legendX, legendY, legendWidth, legendHeight);
            drawLegend(gc2D);
        }
    }
    
    private void drawLegend(Graphics2D gc2D)
    {
        gc2D.setColor(bg);        
        gc2D.fill(legendArea);
        gc2D.setColor(fg);
        gc2D.draw(legendArea);        
        for(int i = 0; i < name.length; i++)
        {
            int x = (int)legendArea.getMinX() + 26;
            int y = (int)legendArea.getMinY() + 12;
            gc2D.setColor(color[i]);
            drawSymbol(gc2D, type[i],  x, y + 12 * i);
            gc2D.setColor(fg);
            gc2D.setFont(lf);
            gc2D.drawString(name[i], x + 25, y + 4 + 12 * i);
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
        for(int i = 0; i < dataX.length; i++)
        {	
            for(int j = 0; j < dataX[i].length; j++)
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
            
            // Get printing area dimensions
            Dimension d  = new Dimension((int)pageFormat.getImageableWidth(),
                                         (int)pageFormat.getImageableHeight());            
            int width = 0;
            int height = 0;
            int top = 0;
            int left = 0;
            
            // Determine size and location of legend
            double legendWidth = 0;
            double legendHeight = 0;
            double legendXp = 0;
            double legendYp = 0;
            
            // Get upper-left corner coordinates
            int lineInsetX  = (int)pageFormat.getImageableX();
            int lineInsetY  = (int)pageFormat.getImageableY();
                        
            // Adjust size to fit the printing area
            if((double)d.width / (double)d.height <= 1.2)
                d.height = (int)(d.width / 1.2);
            else
                d.width = (int)(d.height * 1.2);

            if(legendLocation != null)
            {
                gc2D.setFont(lf);
                int nameWidth = gc.getFontMetrics().stringWidth(name[0]);
                for(int i = 0; i < name.length; i++)
                {
                    if(gc.getFontMetrics().stringWidth(name[i]) > nameWidth)
                        nameWidth = gc.getFontMetrics().stringWidth(name[i]);
                }
                legendWidth = nameWidth + 54;
                legendHeight = 8 + name.length * 12;            
            
                if(legendLocation.equals("Inside"))
                {
                    width = d.width - rightInset - leftInset;
                    height = d.height - bottomInset - topInset;
                    width = width / 25 * 25;
                    height = height / 25 * 25;
                    top = d.height - bottomInset - height + lineInsetY;
                    left = leftInset + lineInsetX;
                    legendXp = left + 5;
                    legendYp = top + 5;
                }
            
                if(legendLocation.equals("Top"))
                {
                    width = d.width - rightInset - leftInset;            
                    height = d.height - bottomInset - topInset - (int)legendHeight - 5;
                    width = width / 25 * 25;
                    height = height / 25 * 25;
                    top = d.height - bottomInset - height + lineInsetY;
                    left = leftInset + lineInsetX;
                    legendXp = left + width / 2 - legendWidth / 2;
                    legendYp = top - legendHeight - 5;              
                }
            
                if(legendLocation.equals("Right"))
                {
                    width = d.width - rightInset - leftInset - (int)legendWidth + 5;
                    height = d.height - bottomInset - topInset;
                    width = width / 25 * 25;
                    height = height / 25 * 25;
                    top = d.height - bottomInset - height + lineInsetY;
                    left = leftInset + lineInsetX;
                    legendXp = left + width + 5;
                    legendYp = top + height / 2 - legendHeight / 2;                    
                }
            }
            else
            {
                width = d.width - rightInset - leftInset;
                height = d.height - bottomInset - topInset;
                width = width / 25 * 25;
                height = height / 25 * 25;
                top = d.height - bottomInset - height + lineInsetY;
                left = leftInset + lineInsetX;
            }
            
            // Draw axes
	    gc2D.drawRect(left, top, width, height); 
            
            // Draw grid
            gc2D.setColor(Color.gray);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
                                           
            if(showVerticalGrid)                                                              
                for(int i = 1; i < 5; i++)
                    gc2D.drawLine(left + i * width/5, top, 
                                  left + i * width/5, top + height);
            if(showHorizontalGrid)                               
                for(int i = 1; i < 5; i++)                               
                    gc2D.drawLine(left,               top + i * height/5, 
                                  left + width,       top + i * height/5);             

            // Draw ticks
            gc2D.setColor(Color.black);                               
            gc2D.setStroke(new BasicStroke());
            int spacingX = width/25;
            int spacingY = height/25;
            int tickSize = 4;
            for(int i = 1; i < 26; i++)
            {
                if(i % 5 == 0)
                    tickSize = 6;
                else
                    tickSize = 4;
                if(!showTicksOnX && tickSize == 4)
                    continue;
                gc2D.drawLine(left + i * spacingX,     top, 
                              left + i * spacingX,     top + tickSize);
                gc2D.drawLine(left + i * spacingX,     top + height - tickSize, 
                              left + i * spacingX,     top + height);
              
                if(!showTicksOnY && tickSize == 4)
                    continue;        
                gc2D.drawLine(left,                    top + i * spacingY, 
                              left + tickSize,         top + i * spacingY);
                gc2D.drawLine(left + width - tickSize, top + i * spacingY, 
                              left + width,            top + i * spacingY);
            }
            
            // Draw x = 0 line, y = 0 line, slope = 1 line
            double spanX = maxX - minX;        
            double spanY = maxY - minY;
            
            gc2D.setColor(Color.green);
            if(xLine && minX < 0 && maxX > 0)
            {
                gc2D.setColor(addedLineColor[0]);
                gc2D.drawLine(left - (int)(minX * width / spanX), top, 
                              left - (int)(minX * width / spanX), top + height);
            }
            if(yLine && minY < 0 && maxY > 0)
            {
                gc2D.setColor(addedLineColor[1]);
                gc2D.drawLine(left,         top + (int)(maxY * height / spanY), 
                              left + width, top + (int)(maxY * height / spanY));
            }
            if(uLine)
            {
                gc2D.setColor(addedLineColor[2]);
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
            
            // Draw the data curve
            for(int i = 0; i < dataX.length; i++)
            {
                gc2D.setColor(color[i]);
                for(int j = 0; j < dataX[i].length; j++)
                {
	            newX[i][j] = (int)(left + (dataX[i][j] - minX)/spanX * width); 
                    newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
                }
                drawCurve(gc2D, type[i], i);
            }
            
            // Draw labels
            gc2D.setFont(pf);
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

	    // Draw titles
            gc2D.setFont(tf);
            int titleWidth = gc.getFontMetrics().stringWidth(title);
            if(legendLocation.equals("Top"))
                gc2D.drawString(title, left + (width - titleWidth)/2, top - (int)legendHeight - 10);
            else
                gc2D.drawString(title, left + (width - titleWidth)/2, top - 10);
            int labelXWidth = gc.getFontMetrics().stringWidth(labelX);
            int labelYWidth = gc.getFontMetrics().stringWidth(labelY);
            gc2D.drawString(labelX, left + (width - labelXWidth)/2, top + height + 40);        
            gc2D.rotate(Math.PI/2);            
            gc2D.drawString(labelY, top + (height - labelYWidth)/2, -16 - lineInsetX);
            gc2D.rotate(Math.PI/-2);
            
            // Draw legend
            if(legendLocation != null)
            {
                double x = legendArea.x;
                double y = legendArea.y;
                if(x != legendX && y == legendY)
                    legendArea.setRect(x + lineInsetX, legendYp, legendArea.width, legendArea.height);            
                if(x == legendX && y != legendY)    
                    legendArea.setRect(legendXp, y + lineInsetY, legendArea.width, legendArea.height);
                if(x == legendX && y == legendY)    
                    legendArea.setRect(legendXp, legendYp, legendArea.width, legendArea.height);
                if(x != legendX && y != legendY)    
                    legendArea.setRect(x + lineInsetX, y + lineInsetY, legendArea.width, legendArea.height);
                drawLegend(gc2D);
                legendArea.setRect(x, y, legendArea.width, legendArea.height);                
            }
            
            return PAGE_EXISTS;
        } 
    }

    /** Draw symbols of the plot.
     * @param gc2D the graphics context.
     * @param symbolType the inty value represents the type of the symbol to draw:
     *        "dot", "circle", "filled square", "square", "filled up triangle", "up triangle",
     *        "filled down triangle", "down triangle", "cross", "check mark", "thick solid line",
     *        "thin solid line" or "dashed line".
     * @param x the x center of the symbol location.
     * @param y the y center of the symbol location. 
     */    
    public void drawSymbol(Graphics2D gc2D, int symbolType, int x, int y)
    {
        switch(symbolType)
        {
            case 0:
                Ellipse2D.Double circle = new Ellipse2D.Double(x - 4, y - 4, 8, 8); 
                gc2D.draw(circle);
                gc2D.fill(circle);
                break;
            case 1:
                circle = new Ellipse2D.Double(x - 4, y - 4, 8, 8); 
                gc2D.draw(circle);
                break;
            case 2:
                Rectangle2D.Double square = new Rectangle2D.Double(x - 3, y - 3, 6, 6);
                gc2D.draw(square);
                gc2D.fill(square);
                break;
            case 3:
                square = new Rectangle2D.Double(x - 3, y - 3, 6, 6);
                gc2D.draw(square);
                break;
            case 4:
                Polygon triangle = new Polygon(new int[]{x - 4, x + 4, x}, 
                                               new int[]{y + 3, y + 3, y - 3}, 3);
                gc2D.draw(triangle);
                gc2D.fill(triangle);
                break;
            case 5:
                triangle = new Polygon(new int[]{x - 4, x + 4, x}, 
                                       new int[]{y + 3, y + 3, y - 3}, 3);
                gc2D.draw(triangle);
                break;
            case 6:
                triangle = new Polygon(new int[]{x - 4, x + 4, x}, 
                                       new int[]{y - 3, y - 3, y + 3}, 3);
                gc2D.draw(triangle);
                gc2D.fill(triangle);
                break;
            case 7:
                triangle = new Polygon(new int[]{x - 4, x + 4, x}, 
                                       new int[]{y - 3, y - 3, y + 3}, 3);
                gc2D.draw(triangle);
                break;                    
            case 8:
                gc2D.drawLine(x - 3, y, x + 3, y);
                gc2D.drawLine(x, y + 3, x, y - 3);
                break;
            case 9:
                gc2D.drawLine(x - 3, y - 3, x + 3, y + 3);
                gc2D.drawLine(x - 3, y + 3, x + 3, y - 3);
                break;
            case 10:
                gc2D.setStroke(new BasicStroke(2.0f));
                gc2D.drawLine(x - 20, y, x + 20, y);
                gc2D.setStroke(new BasicStroke());                
                break;                
            case 11:
                gc2D.drawLine(x - 20, y, x + 20, y);
                break;                
            case 12:
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                               BasicStroke.JOIN_BEVEL, 0, 
                                               new float[]{5.0f, 2.0f}, 0));
                gc2D.drawLine(x - 20, y, x + 20, y);
                gc2D.setStroke(new BasicStroke());
        }
    }

    private void drawCurve(Graphics2D gc2D, int symbolType, int i)
    {
        switch(symbolType)
        {
            case 0:
                for(int j = 0; j < dataX[i].length; j++)
                {                
                    Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8); 
                    gc2D.draw(circle);
                    gc2D.fill(circle);
                }
                break;
            case 1:
                for(int j = 0; j < dataX[i].length; j++)
                {                
                    Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8); 
                    gc2D.draw(circle);
                }
                break;                    
            case 2:
                for(int j = 0; j < dataX[i].length; j++)
                {
                    Rectangle2D.Double square = new Rectangle2D.Double(newX[i][j]- 3, newY[i][j] - 3, 6, 6);
                    gc2D.draw(square);
                    gc2D.fill(square);
                }
                break;
            case 3:
                for(int j = 0; j < dataX[i].length; j++)
                {
                    Rectangle2D.Double square = new Rectangle2D.Double(newX[i][j]- 3, newY[i][j] - 3, 6, 6);
                    gc2D.draw(square);
                }
                break;
            case 4:
                for(int j = 0; j < dataX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] + 3, newY[i][j] + 3, newY[i][j] - 3}, 3);
                    gc2D.draw(triangle);
                    gc2D.fill(triangle);
                }
                break;
            case 5:
                for(int j = 0; j < dataX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                           new int[]{newY[i][j] + 3, newY[i][j] + 3, newY[i][j] - 3}, 3);
                    gc2D.draw(triangle);
                }
                break;
            case 6:
                for(int j = 0; j < dataX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] - 3, newY[i][j] - 3, newY[i][j] + 3}, 3);
                    gc2D.draw(triangle);
                    gc2D.fill(triangle);
                }
                break;
            case 7:
                for(int j = 0; j < dataX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] - 3, newY[i][j] - 3, newY[i][j] + 3}, 3);
                    gc2D.draw(triangle);
                }
                break;                
            case 8:
                for(int j = 0; j < dataX[i].length; j++)
                {
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j], newX[i][j] + 3, newY[i][j]);
                    gc2D.drawLine(newX[i][j], newY[i][j] - 3, newX[i][j], newY[i][j] + 3);
                }
                break;
            case 9:
                for(int j = 0; j < dataX[i].length; j++)
                {
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j] - 3, newX[i][j] + 3, newY[i][j] + 3);
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j] + 3, newX[i][j] + 3, newY[i][j] - 3);
                }
                break;
            case 10:
                gc2D.setStroke(new BasicStroke(2.0f)); 
                gc2D.drawPolyline(newX[i], newY[i], dataX[i].length);
                gc2D.setStroke(new BasicStroke());
                break;
            case 11: 
                gc2D.drawPolyline(newX[i], newY[i], dataX[i].length);
                break;                
            case 12:
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                               BasicStroke.JOIN_BEVEL, 0, new float[]{5.0f, 2.0f}, 0));
                gc2D.drawPolyline(newX[i], newY[i], dataX[i].length);
                gc2D.setStroke(new BasicStroke());
        }        
    }
    
    /** Get the default data range for plotting.
     * @param data a double[][] containing the data.
     * @return a double array containing minimum value and maximum value of the data in order.
     */
    public static double[] getDefaultRange(double[][] data)
    {
        double min = data[0][0];
        double max = min;
        for(int i = 0; i < data.length; i++)
            for(int j = 0; j < data[i].length; j++)
            {
                max = Math.max(max, data[i][j]);
                min = Math.min(min, data[i][j]);
            }    
        if(max == min)
        {
                max += 1;
                min -= 1;
        }
        int p = 0;
        if(Math.abs(max) > Math.abs(min))
        {
            p = (int)(Math.log(max)/Math.log(10)) - 1;
            if(Math.abs(max) < 1)
                p -= 1;
        }
        else
        {
            p = (int)(Math.log(min)/Math.log(10)) - 1;
            if(Math.abs(min) < 1)
                p -= 1;            
        }
         
        max = (double)((int)(max / Math.pow(10, p)) + 1) * Math.pow(10, p); 
        min = (double)((int)(min / Math.pow(10, p)) - 1) * Math.pow(10, p);
        
        double[] range = {min, max};
        return range;
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
    private String labelX;
    private String labelY;
    private String[] name;
    private int[] type;
    private Color[] color;
    private boolean xLine, yLine, uLine;
    private Color[] addedLineColor;
    private Rectangle2D.Double legendArea = null;
    private final static Color bg = Color.white;
    private final static Color fg = Color.black;
    private final static int leftInset = 100;
    private final static int rightInset = 20;
    private final static int topInset = 30;
    private final static int bottomInset = 60;
    private String legendLocation = null;
    private boolean showHorizontalGrid;
    private boolean showVerticalGrid;
    private boolean showTicksOnX;
    private boolean showTicksOnY;
    private final static Font lf = new Font("SansSerif", Font.BOLD, 12);
    private final static Font tf = new Font("SansSerif", Font.BOLD, 14);
    private final static Font pf = new Font("SansSerif", Font.BOLD, 10);
    private Graphics2D gc2D = null;
    private Point2D start, last;
    private double legendX, legendY;
 
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JPopupMenu jPopupMenu1;
    // End of variables declaration//GEN-END:variables
    
}
