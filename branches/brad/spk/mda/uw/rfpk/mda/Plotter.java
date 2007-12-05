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
package uw.rfpk.mda;

import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Font; 
import java.awt.BasicStroke; 
import java.awt.Dimension;
import java.awt.geom.*;
import java.awt.Point;
import java.awt.print.*;
import java.awt.image.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.awt.event.MouseEvent;
import javax.print.attribute.*;
import java.awt.Polygon;
import java.util.Vector;
import java.util.Arrays;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
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
     * @param xLine the flag specifies if a vertical line is required.
     * @param yLine the flag specifies if a horizontal line is required.
     * @param uLine the flag specifies if a unit slope line is required.
     * @param rLine the flag specifies if regression line is required.
     * @param pLine the flag specifies if percentile line is required.
     * @param hGrid the flag specifies if horizontal grid lines are required.
     * @param vGrid the flag specifies if vertical grid lines are required.
     * @param xLineX the X value of which the vertical line intersects the X axis.
     * @param yLineY the Y value of which the horizontal line intersects the Y axis
     * @param regression the name of the selected curve for regression.
     * @param percentage the percentage of the percentiles for the regression.
     * @param addedLineColor the colors of the five added lines in a Color array. Each element represents:
     *        "red", "yellow", "orange", "blue", "pink", "magenta", "cyan", "green", "gray", or "black".
     * @param symbol the symbol types of the curves in an int array. Each element represents:
     *        "dot", "circle", "filled square", "square", "filled up triangle", "up triangle",
     *        "filled down triangle", "down triangle", "cross", "check mark", "thick solid line",
     *        "thin solid line", "thin dashed line", "thick dashed line or vertical bar".
     * @param color the colors of the curves in a Color array. Each element represents:
     *        "red", "yellow", "orange", "blue", "pink", "magenta", "cyan", "green", "gray", or "black".
     * @param legendLocation the location of the legend: "Inside", "Top", "Right", or null if
     *        not showing the legend.
     * @param nHDivi number of horizontal divisions.
     * @param nVDivi number of vertical divisions.
     * @param markLengthX length of division marks on X axis.
     * @param markLengthY length of division marks on Y axis.
     * @param nTickX number of ticks on X axis between adjacent vertical grid.
     * @param nTickY number of ticks on X axis between adjacent horizontal grid.
     * @param tickLengthX length of ticks on X axis.
     * @param tickLengthY length of ticks on Y axis.
     * @param maxX the upper bound of X.
     * @param minX the lower bound of X.
     * @param maxY the upper bound of Y.
     * @param minY the lower bound of Y.
     * @param titleFont the title font.
     * @param labelFont the label font.
     * @param legendFont the legend font.
     * @param numberFont the number font.
     * @param topInset the additional top inset.
     * @param bottomInset the additional bottom inset.
     * @param leftInset the additional left inset.
     * @param rightInset the additional right inset.
     * @param isExpX the flag that specifies if the X numerical lables use exponential expression.
     * @param isExpY the flag that specifies if the Y numerical lables use exponential expression.
     * @param isLogX the flag that specifies if X axis is in log scale.
     * @param isLogY the flag that specifies if Y axis is in log scale.
     * @param isHistogram the flag that specifies if plotting a histogram.
     * @param intervalSize the size of intervals of the histogram.
     * @param baseline the flag that specifies the baseline should be drawn in the histogram.
     * @param baseLabel the label for the baseline of the histogram.
     * @param nDigitX number of digits right to the decimal point of the X numerical lable.
     * @param nDigitY number of digits right to the decimal point of the Y numerical lable.
     * @param frame window of the plot.
     * @param indPoints an array of int arrays each containing number of data points for each individual
     *        of a curve, null for not individualized.
     */
    public Plotter(double[][] dataX, double[][] dataY, String title, String labelX, String labelY, 
                   String[] name, int[] symbol, Color[] color, boolean xLine, boolean yLine, 
                   boolean uLine, boolean rLine, boolean pLine, boolean hGrid, boolean vGrid,
                   double xLineX, double yLineY,
                   String regression, String percentage, Color[] addedLineColor, 
                   String legendLocation, int nHDivi, int nVDivi, int markLengthX, int markLengthY, 
                   int nTickX, int nTickY, int tickLengthX, int tickLengthY, 
                   double maxX, double minX, double maxY, double minY, 
                   Font titleFont, Font labelFont, Font legendFont, Font numberFont, 
                   int topInset, int bottomInset, int leftInset, int rightInset, 
                   boolean isExpX, boolean isExpY, boolean isLogX, boolean isLogY, 
                   boolean isHistogram, double intervalSize, boolean baseline, String baseLabel,
                   int nDigitX, int nDigitY, JFrame frame, int[][] indPoints)
    {
        this.frame = frame;
	this.dataX = dataX;
        this.dataY = dataY;
        this.title = title;
        this.labelX = labelX;
        this.labelY = labelY;
        nCurve = dataX.length;
        if(xLine || yLine || uLine || rLine || pLine)
        {    
            int more = 0;
            if(xLine) more++;
            if(yLine) more++;
            if(uLine) more++;
            if(rLine) more++;
            if(pLine) more++;
            this.name = new String[nCurve + more];
            this.symbol = new int[nCurve + more];
            legendColor = new Color[nCurve + more];            
            for(int i = 0; i < nCurve; i++)
            {
                this.name[i] = name[i];
                this.symbol[i] = symbol[i];
                legendColor[i] = color[i];
            }
            more = nCurve;
            if(xLine)
            {
                this.name[more] = "X = " + xLineX;
                this.symbol[more] = 12;
                legendColor[more++] = addedLineColor[0];
            }
            if(yLine)
            {
                this.name[more] = "Y = " + yLineY;
                this.symbol[more] = 12;
                legendColor[more++] = addedLineColor[1];
            }
            if(uLine)
            {
                this.name[more] = "Y = X";
                this.symbol[more] = 12;
                legendColor[more++] = addedLineColor[2];
            }
            if(rLine)
            {
                this.name[more] = regression + " Regression";
                this.symbol[more] = 10;
                legendColor[more++] = addedLineColor[3];
                int i = -1;
                while(!regression.equals(name[++i]));
                regressionParameters(dataX[i], dataY[i]);                
            }
            if(pLine)
            {
                this.name[more] = percentage + " Percentile";
                this.symbol[more] = 12;
                legendColor[more] = addedLineColor[4];
            }            
        }
        else if(isHistogram)
        {
            this.name = new String[]{"Mean", "Median"};
            this.symbol = new int[]{11, 13};
            legendColor = new Color[]{Color.red, Color.blue};
            if(baseline)
            {
                this.name = new String[]{"Mean", "Median", baseLabel};
                this.symbol = new int[]{11, 13, 10};
                legendColor = new Color[]{Color.red, Color.blue, Color.black};
                double[] temp = new double[dataX[0].length - 1];
                for(int i = 0; i < temp.length; i++)
                    temp[i] = dataX[0][i + 1];
                baselineX = dataX[0][0];
                this.dataX[0] = temp;
            }
        }
        else
        {
            this.name = name;
            this.symbol = symbol;
            legendColor = color;
        }
        this.color = color;
        this.xLine = xLine;
        this.yLine = yLine;
        this.uLine = uLine;
        this.rLine = rLine;
        this.pLine = pLine;
        this.hGrid = hGrid;
        this.vGrid = vGrid;
        this.isLogX = isLogX;
        this.isLogY = isLogY;
        this.isHistogram = isHistogram;
        this.baseline = baseline;
        this.intervalSize = intervalSize;
        this.baseLabel = baseLabel;
        this.xLineX = xLineX;
        this.yLineY = yLineY;
        this.regression = regression;
        this.percentage = percentage;
        this.addedLineColor = addedLineColor;
        this.legendLocation = legendLocation;
        this.nHDivi = nHDivi;
        this.nVDivi = nVDivi;
        this.markLengthX = markLengthX;
        this.markLengthY = markLengthY;
        this.nTickX = nTickX;
        this.nTickY = nTickY;
        this.tickLengthX = tickLengthX;
        this.tickLengthY = tickLengthY;
        this.maxX = maxX;
        this.minX = minX;
        this.maxY = maxY;
        this.minY = minY;
        this.titleFont = titleFont;
        this.labelFont = labelFont;
        this.legendFont = legendFont;
        this.numberFont = numberFont;
        this.topInset += topInset;
        this.bottomInset += bottomInset;
        if(rLine || isHistogram) this.bottomInset += 20;
        this.leftInset += leftInset;
        this.rightInset += rightInset;        
        if(legendLocation != null)
            start = null;
        dFormat = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        dFormat.applyPattern("0.000E00");
        nFormat = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        nFormat.applyPattern("0.0000");
        String form = "";
        if(isExpX)
        {
            for(int i = 0; i < nDigitX; i++)
                form += "0";
            formatX = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
            formatX.applyPattern("0." + form + "E00");
        }
        else
        {
            for(int i = 0; i < nDigitX; i++)
                form += "#";
            formatX = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
            formatX.applyPattern("###." + form);
        }
        form = "";
        if(isExpY)
        {
            for(int i = 0; i < nDigitY; i++)
                form += "0";
            formatY = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
            formatY.applyPattern("0." + form + "E00");
        }
        else
        {
            for(int i = 0; i < nDigitY; i++)
                form += "#";
            formatY = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
            formatY.applyPattern("###." + form);
        }
        this.indPoints = indPoints;
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jPopupMenu1 = new javax.swing.JPopupMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        jMenuItem2 = new javax.swing.JMenuItem();
        scaleDialog = new javax.swing.JDialog();
        jLabel1 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jSlider1 = new javax.swing.JSlider();
        jPanel1 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();

        jMenuItem1.setText("Print");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem1);

        jMenuItem2.setText("Save");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem2);

        scaleDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        scaleDialog.setTitle("Print Settings");
        scaleDialog.setLocationRelativeTo(jPanel1);
        scaleDialog.setModal(true);
        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Select scaling factor for printing.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 12, 9, 11);
        scaleDialog.getContentPane().add(jLabel1, gridBagConstraints);

        jLabel3.setText("Right");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 16, 0, 4);
        scaleDialog.getContentPane().add(jLabel3, gridBagConstraints);

        jSlider1.setPreferredSize(new java.awt.Dimension(180, 16));
        jSlider1.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jSlider1StateChanged(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 8, 12);
        scaleDialog.getContentPane().add(jSlider1, gridBagConstraints);

        jButton1.setText("Preview");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        jButton2.setText("Cancel");
        jButton2.setMaximumSize(new java.awt.Dimension(82, 25));
        jButton2.setMinimumSize(new java.awt.Dimension(82, 25));
        jButton2.setPreferredSize(new java.awt.Dimension(82, 25));
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        scaleDialog.getContentPane().add(jPanel1, gridBagConstraints);

        jLabel4.setText("1.0");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        scaleDialog.getContentPane().add(jLabel4, gridBagConstraints);

        jLabel5.setText("Down");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 13, 0, 4);
        scaleDialog.getContentPane().add(jLabel5, gridBagConstraints);

        jTextField1.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        jTextField1.setText("0.0");
        jTextField1.setPreferredSize(new java.awt.Dimension(40, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        scaleDialog.getContentPane().add(jTextField1, gridBagConstraints);

        jTextField2.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        jTextField2.setText("0.0");
        jTextField2.setPreferredSize(new java.awt.Dimension(40, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        scaleDialog.getContentPane().add(jTextField2, gridBagConstraints);

        jLabel2.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel2.setText("Enter offsets in inches.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 6, 12);
        scaleDialog.getContentPane().add(jLabel2, gridBagConstraints);

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

    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        // Create an image to save
        // Create a buffered image in which to draw
        BufferedImage bufferedImage = new BufferedImage(plotWidth, plotHeight, 
                                                        BufferedImage.TYPE_INT_RGB);
    
        // Create a graphics contents on the buffered image
        Graphics2D g2d = bufferedImage.createGraphics();
        g2d.setColor(Color.white);
        g2d.fillRect(0, 0, plotWidth, plotHeight);
        g2d.setColor(Color.black);
        PageFormat format = new PageFormat();
        format.setOrientation(PageFormat.PORTRAIT);
        Paper paper = new Paper();
        paper.setImageableArea(0, 0, plotWidth, plotHeight);
        format.setPaper(paper);
        Printable printer = new Printer();
        try
        {
            printer.print(g2d, format, 0);
            fileChooser.setDialogTitle("Save Image File");
            fileChooser.removeChoosableFileFilter(fileChooser.getFileFilter());
            fileChooser.addChoosableFileFilter(new MyFilter("jpg"));
            fileChooser.addChoosableFileFilter(new MyFilter("png"));
            int result = fileChooser.showSaveDialog(null);
            if(result == fileChooser.APPROVE_OPTION)
	    {
                FileFilter filter = fileChooser.getFileFilter();
                File file = fileChooser.getSelectedFile();
                String pathname = file.getPath();
                String type = filter.getDescription();
                if(pathname.indexOf(".") == -1)
                    pathname += "." + type;
                else
                    pathname = pathname.substring(0, pathname.indexOf(".")) + "." + type;
                ImageIO.write(bufferedImage, type, new File(pathname));
            }
        }
        catch(PrinterException e)
        {
            JOptionPane.showMessageDialog(null, e, "Printing Error", JOptionPane.ERROR_MESSAGE);
        }
        catch (IOException e) 
        {
            JOptionPane.showMessageDialog(null, e, "File Error", JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private class MyFilter extends FileFilter 
    {
        public MyFilter(String type)
        {
            this.type = type;    
        }
        
        public boolean accept(File file) 
        {
            if(file.isDirectory()) return true;
            String filename = file.getName();
            return filename.endsWith(type);
        }
        public String getDescription() {
            return type;
        }
        
        String type;
    }
    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        scaleDialog.setVisible(false);
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        try
        {
            scale = Double.parseDouble(jLabel4.getText());
            xOff = (int)(Double.parseDouble(jTextField1.getText()) * 72);
            yOff = (int)(Double.parseDouble(jTextField2.getText()) * 72);
        }
        catch(NumberFormatException e)
        {
            JOptionPane.showMessageDialog(null, e, "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        PrintPreview preview = new PrintPreview(frame, new Printer(), pageFormat);
        preview.setVisible(true);
        if(!preview.isCancelled)
        {
            // Close print setting dialog.
            scaleDialog.dispose();
            
            // Set what to print.
            printerJob.setPrintable(new Printer());            
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
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jSlider1StateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_jSlider1StateChanged
        jLabel4.setText(String.valueOf(((double)(jSlider1.getValue() + 50)) / 100));
    }//GEN-LAST:event_jSlider1StateChanged

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
        // Get a PrinterJob object and a PageFormat object.
        printerJob = PrinterJob.getPrinterJob();
        attributes = new HashPrintRequestAttributeSet();         
        pageFormat = printerJob.pageDialog(attributes);

        // Show print setting dialog if pageFormat != null.
        if(pageFormat != null)
        {
            scaleDialog.setSize(227, 225);
            scaleDialog.setVisible(true);
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
   
        // Determine the data ranges
        double[] range;
        if(maxX ==0 && minX == 0)
        {
            range = getDefaultRange(dataX, isLogX);
            minX = range[0];
            maxX = range[1];
            nHDivi = (int)(range[2] + 0.1);
        }
        if(!isHistogram && maxY == 0 && minY == 0)
        {
            range = getDefaultRange(dataY, isLogY);
            minY = range[0];
            maxY = range[1];
            nVDivi = (int)(range[2] + 0.1);
        }
        range = null;
        
        double spanX = maxX - minX;
        double spanY = maxY - minY; 
        
	// Determine size of our display
        Dimension d = getSize();
        int width = 0;
        int height = 0;
        int top = 0;
        int nX = (nTickX + 1) * nHDivi;
        int nY = (nTickY + 1) * nVDivi; 
        
        // Determine size and location of legend
        double legendWidth = 0;
        double legendHeight = 0;
        if(legendLocation != null)
        {
            gc2D.setFont(legendFont);
            int nameWidth = gc.getFontMetrics().stringWidth(name[0]);
            for(int i = 0; i < name.length; i++)
            {
                if(gc.getFontMetrics().stringWidth(name[i]) > nameWidth)
                    nameWidth = gc.getFontMetrics().stringWidth(name[i]);
            }
            legendWidth = nameWidth + 54;
            legendHeight = 8 + name.length * 12;
            if(nCurve == 1) legendHeight -= 12;
            if(legendLocation.equals("Inside"))
            {
                width = d.width - rightInset - leftInset;
                height = d.height - bottomInset - topInset;
                width = width / nX * nX;
                height = height / nY * nY;
                top = d.height - bottomInset - height;
                legendX = leftInset + 5;
                legendY = top + 5;
            }
            
            if(legendLocation.equals("Top"))
            {
                width = d.width - rightInset - leftInset;            
                height = d.height - bottomInset - topInset - (int)legendHeight - 5;
                width = width / nX * nX;
                height = height / nY * nY;
                top = d.height - bottomInset - height;
                legendX = leftInset + width / 2 - legendWidth / 2;
                legendY = top - legendHeight - 5;
            }
            
            if(legendLocation.equals("Right"))
            {
                width = d.width - rightInset - leftInset - (int)legendWidth + 5;
                height = d.height - bottomInset - topInset;
                width = width / nX * nX;
                height = height / nY * nY;
                top = d.height - bottomInset - height;
                legendX = leftInset + width + 5;
                legendY = top + height / 2 - legendHeight / 2;
            }
        }
        else
        {
            width = d.width - rightInset - leftInset;
            height = d.height - bottomInset - topInset;
            width = width / nX * nX;
            height = height / nY * nY;
            top = d.height - bottomInset - height;
        }
        plotWidth = d.width; 
        plotHeight = d.height;
        
	// Draw axes
	gc2D.drawRect(leftInset, top, width, height); 
        
        // Draw grid lines
        if((hGrid && nVDivi != 1) || (vGrid && nHDivi != 1));
        {
            gc2D.setColor(Color.gray);
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));  
        
            if(hGrid)
                for(int i = 1; i < nHDivi; i++)
                    gc2D.drawLine(leftInset + i * width/nHDivi, top, 
                                  leftInset + i * width/nHDivi, top + height);
            if(vGrid)
                for(int i = 1; i < nVDivi; i++)                                       
                    gc2D.drawLine(leftInset,               top + i * height/nVDivi, 
                                  leftInset + width,       top + i * height/nVDivi);
                                           
            gc2D.setColor(Color.black);                              
            gc2D.setStroke(new BasicStroke());                                           
        }

        // Draw divisions
        if(nHDivi != 1 && markLengthX != 0)
            for(int i = 1; i < nHDivi; i++)
            {
                gc2D.drawLine(leftInset + i * width/nHDivi, top, 
                              leftInset + i * width/nHDivi, top + markLengthX);
                gc2D.drawLine(leftInset + i * width/nHDivi, top + height - markLengthX, 
                              leftInset + i * width/nHDivi, top + height);                
            }
        if(nVDivi != 1 && markLengthY != 0)
            for(int i = 1; i < nVDivi; i++)
            {
                gc2D.drawLine(leftInset,                       top + i * height/nVDivi,
                              leftInset + markLengthY,         top + i * height/nVDivi);
                gc2D.drawLine(leftInset + width - markLengthY, top + i * height/nVDivi,
                              leftInset + width,               top + i * height/nVDivi);
            }
                
        // Draw ticks
        if(nTickX != 0 && tickLengthX != 0)
        {
            int spacingX = width/(nTickX + 1)/nHDivi;
            for(int i = 1; i < (nTickX + 1)*nHDivi; i++)
            {
                if(i % (nTickX + 1) == 0) continue;
                gc2D.drawLine(leftInset + i * spacingX,     top, 
                              leftInset + i * spacingX,     top + tickLengthX);
                gc2D.drawLine(leftInset + i * spacingX,     top + height - tickLengthX, 
                              leftInset + i * spacingX,     top + height);
            }
        }
        if(nTickY != 0 && tickLengthY != 0)
        {
            int spacingY = height/(nTickY + 1)/nVDivi;
            for(int i = 1; i < (nTickY + 1)*nVDivi; i++)
            {
                if(i % (nTickY + 1) == 0) continue;                
                gc2D.drawLine(leftInset,                       top + i * spacingY, 
                              leftInset + tickLengthY,         top + i * spacingY);
                gc2D.drawLine(leftInset + width - tickLengthY, top + i * spacingY, 
                              leftInset + width,               top + i * spacingY);
            }
        }
        
        // Draw x = 0 line, y = 0 line, slope = 1 line
        if(xLine && minX < xLineX && maxX > xLineX)
        {
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.setColor(addedLineColor[0]);
            gc2D.drawLine(leftInset - (int)((minX - xLineX) * width / spanX), top, 
                          leftInset - (int)((minX - xLineX) * width / spanX), top + height);
            gc2D.setStroke(new BasicStroke());
        }
        if(yLine && minY < yLineY && maxY > yLineY)
        {
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.setColor(addedLineColor[1]);
            gc2D.drawLine(leftInset,         top + (int)((maxY - yLineY) * height / spanY), 
                          leftInset + width, top + (int)((maxY - yLineY) * height / spanY));
            gc2D.setStroke(new BasicStroke());
        }
        if(uLine)
        {
            gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
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
            gc2D.setStroke(new BasicStroke());
        }
                	         	
	// Draw numbers for not being histogram
        if(!isHistogram)
        {
            gc2D.setFont(numberFont);
            gc2D.setColor(Color.black);
            String value;
            for(int i = 0; i <= nHDivi; i++)
            {
                value = formatX.format(minX + spanX*i/nHDivi);
                gc2D.drawString(value,
                                leftInset + width*i/nHDivi - gc.getFontMetrics().stringWidth(value)/2, 
                                top + height + 18);
            }

            for(int i = 0; i <= nVDivi; i++)
            {
                value = formatY.format(maxY - spanY*i/nVDivi);
                gc2D.drawString(value,
                                leftInset - gc.getFontMetrics().stringWidth(value) - 2, 
                                top + height*i/nVDivi + 5);
            }
        }
        
	// Draw titles
        gc2D.setColor(Color.black);
        gc2D.setFont(titleFont);
        int titleWidth = gc.getFontMetrics().stringWidth(title);
        if(legendLocation != null && legendLocation.equals("Top"))
            gc2D.drawString(title, leftInset + (width - titleWidth)/2, top - (int)legendHeight - 10);
        else
            gc2D.drawString(title, leftInset + (width - titleWidth)/2, top - 10);

        // Draw labels
        gc2D.setFont(labelFont);
        int labelXWidth = gc.getFontMetrics().stringWidth(labelX);
        int labelYWidth = gc.getFontMetrics().stringWidth(labelY);        
        gc2D.drawString(labelX, leftInset + (width - labelXWidth)/2, top + height + 40);      
        gc2D.rotate(-Math.PI/2);
        gc2D.drawString(labelY, -top - (height + labelYWidth)/2, 26);
        gc2D.rotate(Math.PI/2);
       
        // Draw regression line
        if(rLine)
        {
            gc2D.setColor(addedLineColor[3]);
            double[][] xy = lineEnds(intersect, slope);
            gc2D.setStroke(new BasicStroke(2.0f));
            gc2D.drawLine(leftInset + (int)((xy[0][0] - minX) * width / spanX), 
                          top + height - (int)((xy[0][1] - minY) * height / spanY), 
                          leftInset + (int)((xy[1][0] - minX) * width / spanX), 
                          top + height - (int)((xy[1][1] - minY) * height / spanY));
            if(pLine)
            {
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                               BasicStroke.JOIN_BEVEL, 0, 
                                               new float[]{5.0f, 2.0f}, 0));               
                gc2D.setColor(addedLineColor[4]);
                double band = 0;
                if(percentage.equals("99%")) band = stdDeviation * 2.576;
                if(percentage.equals("95%")) band = stdDeviation * 1.960;
                if(percentage.equals("90%")) band = stdDeviation * 1.645;
                if(percentage.equals("80%")) band = stdDeviation * 1.282;
                if(percentage.equals("50%")) band = stdDeviation * 0.675;                
                xy = lineEnds(intersect + band, slope);
                gc2D.drawLine(leftInset + (int)((xy[0][0] - minX) * width / spanX), 
                              top + height - (int)((xy[0][1] - minY) * height / spanY), 
                              leftInset + (int)((xy[1][0] - minX) * width / spanX), 
                              top + height - (int)((xy[1][1] - minY) * height / spanY));
                xy = lineEnds(intersect - band, slope);
                gc2D.drawLine(leftInset + (int)((xy[0][0] - minX) * width / spanX), 
                              top + height - (int)((xy[0][1] - minY) * height / spanY), 
                              leftInset + (int)((xy[1][0] - minX) * width / spanX), 
                              top + height - (int)((xy[1][1] - minY) * height / spanY));                 
            }
            gc2D.setStroke(new BasicStroke());
            
            gc2D.setColor(fg);
            gc2D.setFont(numberFont);
            String regressionLine = "Regression Line Y = (" + dFormat.format(intersect) + ") + (" +
                                    dFormat.format(slope) + ") X,   Sample Size = " + nRegressionData;
            int labelWidth = gc.getFontMetrics().stringWidth(regressionLine);
            int start = (d.width - labelWidth) / 2;
            int bottom = top + height + bottomInset;
            gc2D.drawString(regressionLine, start, bottom - 20);
            regressionLine = "Standard Deviation = " + dFormat.format(stdDeviation) +
                             ",   Correlation Coefficient = " + nFormat.format(corrCoefficient); 
            gc2D.drawString(regressionLine, start, bottom - 10);            
        }
        
        // Draw legend
        if(legendLocation != null)
        {
            if(start == null)
                legendArea = new Rectangle2D.Double(legendX, legendY, legendWidth, legendHeight);
            drawLegend(gc2D);
        }
        
        // Draw the data curves
        if(isHistogram)
        {
            double min = dataX[0][0];
            double max = min;
            for(int i = 0; i < dataX[0].length; i++)
            {
                max = Math.max(max, dataX[0][i]);
                min = Math.min(min, dataX[0][i]);
            }
            if(baseline && min >= baselineX) min = baselineX - 0.05 * (max - baselineX);
            double span = max - min + intervalSize;
            double size = intervalSize * width/span;
            int[] frequencies = getFrequency(dataX[0], min, max, intervalSize);
            int nInterval = frequencies.length;
            max = frequencies[0];
            for(int i = 0; i < nInterval; i++)
                max = Math.max(max, frequencies[i]);
            double unit = 0.8 * height / max;
            gc2D.setColor(Color.black);
            for(int i = 0; i < nInterval; i++)
                gc2D.drawRect(leftInset + (int)(i * size), height + top - (int)(frequencies[i] * unit), 
                              (int)size, (int)(frequencies[i] * unit));
            
            // Draw data mean
            meanX = leftInset + (int)((getMean(dataX[0]) - min)/span * width);
            gc2D.setColor(Color.red);
            gc2D.drawLine(meanX, height + top, meanX, top);
                        
            // Draw data median
            medianX = leftInset + (int)((getMedian(dataX[0]) - min)/span * width);
            gc2D.setColor(Color.blue);
            gc2D.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
            gc2D.drawLine(medianX, height + top, medianX, top);
                        
            // Draw baseline
            if(baseline)
            {
                baseX = leftInset + (int)((baselineX - min)/span * width);
                gc2D.setColor(Color.black);
                gc2D.setStroke(new BasicStroke(2.0f));
                gc2D.drawLine(baseX, height + top, baseX, top);
                gc2D.setStroke(new BasicStroke());
            }
            
            // Draw numbers for histogram
            gc2D.setFont(numberFont);
            gc2D.setColor(Color.black);
            String value;
            for(int i = 0; i <= nHDivi; i++)
            {
                value = formatX.format(min + span*i/nHDivi);
                gc2D.drawString(value,
                                leftInset + width*i/nHDivi - gc.getFontMetrics().stringWidth(value)/2, 
                                top + height + 18);
            }
            for(int i = 0; i <= nVDivi; i++)
            {
                value = formatY.format(max*i/nVDivi * 5 / 4);
                gc2D.drawString(value,
                                leftInset - gc.getFontMetrics().stringWidth(value) - 2, 
                                top + height - height*i/nVDivi + 5);
            }
            
            String parameters = "Sample Size = " + dataX[0].length +
                                ",   Interval Size = " + dFormat.format(intervalSize);
            if(baseline)
                parameters += ",   True Value = " + dFormat.format(baselineX);
            gc2D.drawString(parameters, (d.width - gc.getFontMetrics().stringWidth(parameters))/2,
                            top + height + bottomInset - 20);
            parameters = "Mean = " + dFormat.format(getMean(dataX[0])) +
                         ",   Median = " + dFormat.format(getMedian(dataX[0])) +
                         ",   Std. Deviation = " + dFormat.format(getStdDev(dataX[0]));
            gc2D.drawString(parameters, (d.width - gc.getFontMetrics().stringWidth(parameters))/2,
                            top + height + bottomInset - 10);
            return;
        }
        
        if(isLogX)
        {
            double log10 = Math.log(10);
            for(int i = 0; i < nCurve; i++)
                for(int j = 0; j < dataX[i].length; j++)
                    dataX[i][j] = Math.log(dataX[i][j]) / log10;
            minX = Math.log(minX) / log10;
            maxX = Math.log(maxX) / log10;
            isLogX = false;
        }
        if(isLogY)
        {
            double log10 = Math.log(10);
            for(int i = 0; i < nCurve; i++)
                for(int j = 0; j < dataY[i].length; j++)
                    dataY[i][j] = Math.log(dataY[i][j]) / log10;
            minY = Math.log(minY) / log10;
            maxY = Math.log(maxY) / log10;
            isLogY = false;
        }
        newX = new int[nCurve][];
        newY = new int[nCurve][];        
        for(int i = 0; i < nCurve; i++)
        {
            Vector<Double> selectedX = new Vector<Double>(dataX[i].length);
            Vector<Double> selectedY = new Vector<Double>(dataY[i].length);
            for(int j = 0; j < dataX[i].length; j++)
                if(dataX[i][j] >= minX && dataX[i][j] <= maxX &&
                   dataY[i][j] >= minY && dataY[i][j] <= maxY)
                {
                    selectedX.add(new Double(dataX[i][j]));
                    selectedY.add(new Double(dataY[i][j]));
                }
            int size = selectedX.size();
            for(int j = 0; j < size; j++)
            {
                dataX[i][j] = ((Double)selectedX.get(j)).doubleValue();
                dataY[i][j] = ((Double)selectedY.get(j)).doubleValue();
            }
            newX[i] = new int[size];
            newY[i] = new int[size];
            gc2D.setColor(color[i]);          
            for(int j = 0; j < size; j++)
            {
	        newX[i][j] = (int)(leftInset + (dataX[i][j] - minX)/spanX * width); 
                newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
            }
            drawCurve(gc2D, symbol[i], i, height + top);
        }
    }
    
    private void drawLegend(Graphics2D gc2D)
    {
        if(name.length == 1) return;
        gc2D.setColor(bg);        
        gc2D.fill(legendArea);
        gc2D.setColor(fg);
        gc2D.draw(legendArea);
        int j = nCurve > 1? 0 : 1;
        for(int i = j; i < name.length; i++)
        {
            int x = (int)legendArea.getMinX() + 26;
            int y = (int)legendArea.getMinY() + 12;
            gc2D.setColor(legendColor[i]);
            drawSymbol(gc2D, symbol[i],  x, y + 12 * (i - j));
            gc2D.setColor(fg);
            gc2D.setFont(legendFont);
            gc2D.drawString(name[i], x + 25, y + 4 + 12 * (i - j));
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
        if(isHistogram)
        {
            if(baseline && Math.abs(mousePoint.x - baseX) < 2)
                toolTip = baseLabel + " = " + dFormat.format(baselineX);
            if(Math.abs(mousePoint.x - meanX) < 2)
                toolTip = "Mean = " + dFormat.format(getMean(dataX[0]));
            if(Math.abs(mousePoint.x - medianX) < 2)
                toolTip = "Median = " + dFormat.format(getMedian(dataX[0]));
        }
        else
        {
            for(int i = 0; i < nCurve; i++)
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
        }
        return toolTip;    
    }
    
    private class Printer implements Printable{
        
        public int print(Graphics gc, PageFormat pageFormat, int pageIndex) throws PrinterException {
            if(pageIndex != 0)
                return NO_SUCH_PAGE; 
            Graphics2D gc2D = ((Graphics2D)gc);
            gc2D.scale(scale, scale);
            
            // Get printing area dimensions
            Dimension d  = new Dimension((int)pageFormat.getImageableWidth(),
                                         (int)pageFormat.getImageableHeight());            
            int width = 0;
            int height = 0;
            int top = 0;
            int left = 0;
            int nX = (nTickX + 1) * nHDivi;
            int nY = (nTickY + 1) * nVDivi;
            
            // Determine size and location of legend
            double legendWidth = 0;
            double legendHeight = 0;
            double legendXp = 0;
            double legendYp = 0;
            
            // Get upper-left corner coordinates
            int lineInsetX  = (int)(pageFormat.getImageableX() / scale) + xOff;
            int lineInsetY  = (int)(pageFormat.getImageableY() / scale) + yOff;

            d.width = plotWidth;
            d.height = plotHeight;
            
            if(legendLocation != null)
            {
                gc2D.setFont(legendFont);
                int nameWidth = gc.getFontMetrics().stringWidth(name[0]);
                for(int i = 0; i < name.length; i++)
                {
                    if(gc.getFontMetrics().stringWidth(name[i]) > nameWidth)
                        nameWidth = gc.getFontMetrics().stringWidth(name[i]);
                }
                legendWidth = nameWidth + 54;
                legendHeight = 8 + name.length * 12;            
                if(nCurve == 1) legendHeight -= 12;
                if(legendLocation.equals("Inside"))
                {
                    width = d.width - rightInset - leftInset;
                    height = d.height - bottomInset - topInset;
                    width = width / nX * nX;
                    height = height / nY * nY;
                    top = d.height - bottomInset - height + lineInsetY;
                    left = leftInset + lineInsetX;
                    legendXp = left + 5;
                    legendYp = top + 5;
                }
            
                if(legendLocation.equals("Top"))
                {
                    width = d.width - rightInset - leftInset;            
                    height = d.height - bottomInset - topInset - (int)legendHeight - 5;
                    width = width / nX * nX;
                    height = height / nY * nY;
                    top = d.height - bottomInset - height + lineInsetY;
                    left = leftInset + lineInsetX;
                    legendXp = left + width / 2 - legendWidth / 2;
                    legendYp = top - legendHeight - 5;              
                }
            
                if(legendLocation.equals("Right"))
                {
                    width = d.width - rightInset - leftInset - (int)legendWidth + 5;
                    height = d.height - bottomInset - topInset;
                    width = width / nX * nX;
                    height = height / nY * nY;
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
                width = width / nX * nX;
                height = height / nY * nY;
                top = d.height - bottomInset - height + lineInsetY;
                left = leftInset + lineInsetX;
            }
            
            // Draw axes
	    gc2D.drawRect(left, top, width, height); 
            

            
            // Draw x = 0 line, y = 0 line, slope = 1 line
            double spanX = maxX - minX;        
            double spanY = maxY - minY;
            
            if(xLine && minX < xLineX && maxX > xLineX)
            {
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
                gc2D.setColor(addedLineColor[0]);
                gc2D.drawLine(left - (int)((minX - xLineX) * width / spanX), top, 
                              left - (int)((minX - xLineX) * width / spanX), top + height);
                gc2D.setStroke(new BasicStroke());
            }
            if(yLine && minY < yLineY && maxY > yLineY)
            {
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
                gc2D.setColor(addedLineColor[1]);
                gc2D.drawLine(left,         top + (int)((maxY - yLineY) * height / spanY), 
                              left + width, top + (int)((maxY - yLineY) * height / spanY));
                gc2D.setStroke(new BasicStroke());
            }
            if(uLine)
            {
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                           BasicStroke.JOIN_BEVEL, 0, 
                                           new float[]{5.0f, 2.0f}, 0));
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
                gc2D.setStroke(new BasicStroke());
            }
            
            // Draw grid lines
            if((hGrid && nVDivi != 1) || (vGrid && nHDivi != 1));
            {
                gc2D.setColor(Color.gray);
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                               BasicStroke.JOIN_BEVEL, 0, 
                                               new float[]{5.0f, 2.0f}, 0));  
        
                if(hGrid)
                    for(int i = 1; i < nHDivi; i++)
                        gc2D.drawLine(left + i * width/nHDivi, top, 
                                      left + i * width/nHDivi, top + height);
                if(vGrid)
                    for(int i = 1; i < nVDivi; i++)                                       
                        gc2D.drawLine(left,               top + i * height/nVDivi, 
                                      left + width,       top + i * height/nVDivi);
                                           
                gc2D.setColor(Color.black);                              
                gc2D.setStroke(new BasicStroke());                                           
            }

            // Draw divisions
            if(nHDivi != 1 && markLengthX != 0)
                for(int i = 1; i < nHDivi; i++)
                {
                    gc2D.drawLine(left + i * width/nHDivi, top, 
                                  left + i * width/nHDivi, top + markLengthX);
                    gc2D.drawLine(left + i * width/nHDivi, top + height - markLengthX, 
                                  left + i * width/nHDivi, top + height);                
                }
            if(nVDivi != 1 && markLengthY != 0)
                for(int i = 1; i < nVDivi; i++)
                {
                    gc2D.drawLine(left,                       top + i * height/nVDivi,
                                  left + markLengthY,         top + i * height/nVDivi);
                    gc2D.drawLine(left + width - markLengthY, top + i * height/nVDivi,
                                  left + width,               top + i * height/nVDivi);
                }
                
            // Draw ticks
            if(nTickX != 0 && tickLengthX != 0)
            {
                int spacingX = width/(nTickX + 1)/nHDivi;
                for(int i = 1; i < (nTickX + 1)*nHDivi; i++)
                {
                    if(i % (nTickX + 1) == 0) continue;
                    gc2D.drawLine(left + i * spacingX,     top, 
                                  left + i * spacingX,     top + tickLengthX);
                    gc2D.drawLine(left + i * spacingX,     top + height - tickLengthX, 
                                  left + i * spacingX,     top + height);
                }
            }
            if(nTickY != 0 && tickLengthY != 0)
            {
                int spacingY = height/(nTickY + 1)/nVDivi;
                for(int i = 1; i < (nTickY + 1)*nVDivi; i++)
                {
                    if(i % (nTickY + 1) == 0) continue;
                    gc2D.drawLine(left,                       top + i * spacingY,
                                  left + tickLengthY,         top + i * spacingY);
                    gc2D.drawLine(left + width - tickLengthY, top + i * spacingY,
                                  left + width,               top + i * spacingY);
                }
            }
                                   
            // Draw numbers for not being histogram
            if(!isHistogram)
            {
                gc2D.setFont(numberFont);
                gc2D.setColor(Color.black);
                String value;
                for(int i = 0; i <= nHDivi; i++)
                {
                    value = formatX.format(minX + spanX*i/nHDivi);
                    gc2D.drawString(value, 
                                    left + width*i/nHDivi - gc.getFontMetrics().stringWidth(value)/2, 
                                    top + height + 18);
                }
                for(int i = 0; i <= nVDivi; i++)
                {            
                    value = formatY.format(maxY - spanY*i/nVDivi);
                    gc2D.drawString(value, 
                                    left - gc.getFontMetrics().stringWidth(value) - 2, 
                                    top + height*i/nVDivi + 5);            
                }
            }
            
	    // Draw titles
            gc2D.setFont(titleFont);
            int titleWidth = gc.getFontMetrics().stringWidth(title);
            if(legendLocation != null && legendLocation.equals("Top"))
                gc2D.drawString(title, left + (width - titleWidth)/2, top - (int)legendHeight - 10);
            else
                gc2D.drawString(title, left + (width - titleWidth)/2, top - 10);
            
            // Draw labels
            gc2D.setFont(labelFont);
            int labelXWidth = gc.getFontMetrics().stringWidth(labelX);
            int labelYWidth = gc.getFontMetrics().stringWidth(labelY);
            gc2D.drawString(labelX, left + (width - labelXWidth)/2, top + height + 40);        
            gc2D.rotate(-Math.PI/2);            
            gc2D.drawString(labelY, -top - (height + labelYWidth)/2, 26 + lineInsetX);
            gc2D.rotate(Math.PI/2);
         
            // Draw regression line
            if(rLine)
            {
                gc2D.setColor(addedLineColor[3]);
                double[][] xy = lineEnds(intersect, slope);
                gc2D.setStroke(new BasicStroke(2.0f));
                gc2D.drawLine(left + (int)((xy[0][0] - minX) * width / spanX), 
                              top + height - (int)((xy[0][1] - minY) * height / spanY), 
                              left + (int)((xy[1][0] - minX) * width / spanX), 
                              top + height - (int)((xy[1][1] - minY) * height / spanY));
                if(pLine)
                {
                    gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                                                   BasicStroke.JOIN_BEVEL, 0, 
                                                   new float[]{5.0f, 2.0f}, 0));               
                    gc2D.setColor(addedLineColor[4]);
                    double band = 0;
                    if(percentage.equals("99%")) band = stdDeviation * 2.576;
                    if(percentage.equals("95%")) band = stdDeviation * 1.960;
                    if(percentage.equals("90%")) band = stdDeviation * 1.645;
                    if(percentage.equals("80%")) band = stdDeviation * 1.282;
                    if(percentage.equals("50%")) band = stdDeviation * 0.675;                
                    xy = lineEnds(intersect + band, slope);
                    gc2D.drawLine(left + (int)((xy[0][0] - minX) * width / spanX), 
                                  top + height - (int)((xy[0][1] - minY) * height / spanY), 
                                  left + (int)((xy[1][0] - minX) * width / spanX), 
                                  top + height - (int)((xy[1][1] - minY) * height / spanY));
                    xy = lineEnds(intersect - band, slope);
                    gc2D.drawLine(left + (int)((xy[0][0] - minX) * width / spanX), 
                                  top + height - (int)((xy[0][1] - minY) * height / spanY), 
                                  left + (int)((xy[1][0] - minX) * width / spanX), 
                                  top + height - (int)((xy[1][1] - minY) * height / spanY));                 
                }
                gc2D.setStroke(new BasicStroke());
            
                gc2D.setColor(fg);
                gc2D.setFont(numberFont);
                String secondTerm = "";
                String regressionLine = "Regression Line Y = (" + dFormat.format(intersect) + ") + (" +
                                        dFormat.format(slope) + ") X,   Sample Size = " + nRegressionData;
                int labelWidth = gc.getFontMetrics().stringWidth(regressionLine);
                int start = (d.width - labelWidth) / 2 + lineInsetX;
                int bottom = top + height + bottomInset;
                gc2D.drawString(regressionLine, start, bottom - 20);
                regressionLine = "Standard Deviation = " + dFormat.format(stdDeviation) +
                                 ",   Correlation Coefficient = " + nFormat.format(corrCoefficient);                
                gc2D.drawString(regressionLine, start, bottom - 10);                
            }
            
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
            
            // Draw the data curves
            if(isHistogram)
            {
                double min = dataX[0][0];
                double max = min;
                for(int i = 0; i < dataX[0].length; i++)
                {
                    max = Math.max(max, dataX[0][i]);
                    min = Math.min(min, dataX[0][i]);
                }
                if(baseline && min >= baselineX) min = baselineX - 0.05 * (max - baselineX);
                double span = max - min + intervalSize;
                double size = intervalSize * width/span;
                int[] frequencies = getFrequency(dataX[0], min, max, intervalSize);
                int nInterval = frequencies.length;
                max = frequencies[0];
                for(int i = 0; i < nInterval; i++)
                    max = Math.max(max, frequencies[i]);
                double unit = 0.8 * height / max;
                gc2D.setColor(Color.black);
                for(int i = 0; i < nInterval; i++)
                    gc2D.drawRect(left + (int)(i * size), height + top - (int)(frequencies[i] * unit), 
                                  (int)size, (int)(frequencies[i] * unit));
            
                // Draw data mean
                gc2D.setColor(Color.red);
                gc2D.drawLine(meanX + left - leftInset, height + top, meanX + left - leftInset, top);

                // Draw data median
                gc2D.setColor(Color.blue);
                gc2D.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, 
                                               BasicStroke.JOIN_BEVEL, 0, 
                                               new float[]{5.0f, 2.0f}, 0));
                gc2D.drawLine(medianX + left - leftInset, height + top, medianX + left - leftInset, top);            
                                
                // Draw baseline
                if(baseline)
                {
                    gc2D.setColor(Color.black);
                    gc2D.setStroke(new BasicStroke(2.0f));
                    gc2D.drawLine(baseX + left - leftInset, height + top, baseX + left - leftInset, top);
                    gc2D.setStroke(new BasicStroke());
                }
            
                // Draw numbers for histogram
                gc2D.setFont(numberFont);
                gc2D.setColor(Color.black);
                String value;
                for(int i = 0; i <= nHDivi; i++)
                {
                    value = formatX.format(min + span*i/nHDivi);
                    gc2D.drawString(value,
                                    left + width*i/nHDivi - gc.getFontMetrics().stringWidth(value)/2, 
                                    top + height + 18);
                }
                for(int i = 0; i <= nVDivi; i++)
                {
                    value = formatY.format(max*i/nVDivi * 5 / 4);
                    gc2D.drawString(value,
                                    left - gc.getFontMetrics().stringWidth(value) - 2, 
                                    top + height - height*i/nVDivi + 5);
                }
                String parameters = "Sample Size = " + dataX[0].length +
                                    ",   Interval Size = " + dFormat.format(intervalSize);
                if(baseline)
                    parameters += ",   True Value = " + dFormat.format(baselineX);
                gc2D.drawString(parameters, (d.width - gc.getFontMetrics().stringWidth(parameters))/2 +
                                lineInsetX, top + height + bottomInset - 20);
                parameters = "Mean = " + dFormat.format(getMean(dataX[0])) +
                             ",   Median = " + dFormat.format(getMedian(dataX[0])) +
                             ",   Std. Deviation = " + dFormat.format(getStdDev(dataX[0]));
                gc2D.drawString(parameters, (d.width - gc.getFontMetrics().stringWidth(parameters))/2 +
                                lineInsetX, top + height + bottomInset - 10);
            }
            else
            {
                for(int i = 0; i < nCurve; i++)
                {
                    gc2D.setColor(color[i]);
                    for(int j = 0; j < dataX[i].length; j++)
                    {
	                newX[i][j] = (int)(left + (dataX[i][j] - minX)/spanX * width); 
                        newY[i][j] = (int)(height + top - (dataY[i][j] - minY)/spanY * height);
                    }
                    drawCurve(gc2D, symbol[i], i, height + top);
                }
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
                break;
            case 13:
                gc2D.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, 
                                               BasicStroke.JOIN_BEVEL, 0, 
                                               new float[]{5.0f, 2.0f}, 0));
                gc2D.drawLine(x - 20, y, x + 20, y);
                gc2D.setStroke(new BasicStroke());
                break;
            case 14:
                gc2D.setStroke(new BasicStroke(2.0f));
                gc2D.drawLine(x, y - 5, x, y + 5);
                gc2D.setStroke(new BasicStroke());
        }
    }

    private void drawCurve(Graphics2D gc2D, int symbolType, int i, int yBase)
    {
        switch(symbolType)
        {
            case 0:
                for(int j = 0; j < newX[i].length; j++)
                {                
                    Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8); 
                    gc2D.draw(circle);
                    gc2D.fill(circle);
                }
                break;
            case 1:
                for(int j = 0; j < newX[i].length; j++)
                {                
                    Ellipse2D.Double circle = new Ellipse2D.Double(newX[i][j] - 4, newY[i][j] - 4, 8, 8); 
                    gc2D.draw(circle);
                }
                break;                    
            case 2:
                for(int j = 0; j < newX[i].length; j++)
                {
                    Rectangle2D.Double square = new Rectangle2D.Double(newX[i][j]- 3, newY[i][j] - 3, 6, 6);
                    gc2D.draw(square);
                    gc2D.fill(square);
                }
                break;
            case 3:
                for(int j = 0; j < newX[i].length; j++)
                {
                    Rectangle2D.Double square = new Rectangle2D.Double(newX[i][j]- 3, newY[i][j] - 3, 6, 6);
                    gc2D.draw(square);
                }
                break;
            case 4:
                for(int j = 0; j < newX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] + 3, newY[i][j] + 3, newY[i][j] - 3}, 3);
                    gc2D.draw(triangle);
                    gc2D.fill(triangle);
                }
                break;
            case 5:
                for(int j = 0; j < newX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                           new int[]{newY[i][j] + 3, newY[i][j] + 3, newY[i][j] - 3}, 3);
                    gc2D.draw(triangle);
                }
                break;
            case 6:
                for(int j = 0; j < newX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] - 3, newY[i][j] - 3, newY[i][j] + 3}, 3);
                    gc2D.draw(triangle);
                    gc2D.fill(triangle);
                }
                break;
            case 7:
                for(int j = 0; j < newX[i].length; j++)
                {   
                    Polygon triangle = new Polygon(new int[]{newX[i][j] - 4, newX[i][j] + 4, newX[i][j]}, 
                                                   new int[]{newY[i][j] - 3, newY[i][j] - 3, newY[i][j] + 3}, 3);
                    gc2D.draw(triangle);
                }
                break;                
            case 8:
                for(int j = 0; j < newX[i].length; j++)
                {
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j], newX[i][j] + 3, newY[i][j]);
                    gc2D.drawLine(newX[i][j], newY[i][j] - 3, newX[i][j], newY[i][j] + 3);
                }
                break;
            case 9:
                for(int j = 0; j < newX[i].length; j++)
                {
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j] - 3, newX[i][j] + 3, newY[i][j] + 3);
                    gc2D.drawLine(newX[i][j] - 3, newY[i][j] + 3, newX[i][j] + 3, newY[i][j] - 3);
                }
                break;
            case 10:
                gc2D.setStroke(new BasicStroke(2.0f));
                if(indPoints != null)
                    drawIndLines(gc2D, newX[i], newY[i], i);
                else
                    gc2D.drawPolyline(newX[i], newY[i], newX[i].length);
                gc2D.setStroke(new BasicStroke());
                break;
            case 11:
                if(indPoints != null) drawIndLines(gc2D, newX[i], newY[i], i);
                else gc2D.drawPolyline(newX[i], newY[i], newX[i].length);
                break;                
            case 12:
                gc2D.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
                               BasicStroke.JOIN_BEVEL, 0, new float[]{5.0f, 2.0f}, 0));
                if(indPoints != null) drawIndLines(gc2D, newX[i], newY[i], i);
                else gc2D.drawPolyline(newX[i], newY[i], newX[i].length);
                gc2D.setStroke(new BasicStroke());
                break;
            case 13:
                gc2D.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, 
                               BasicStroke.JOIN_BEVEL, 0, new float[]{5.0f, 2.0f}, 0));
                if(indPoints != null) drawIndLines(gc2D, newX[i], newY[i], i);
                else gc2D.drawPolyline(newX[i], newY[i], newX[i].length);
                gc2D.setStroke(new BasicStroke());
                break;
            case 14:
                gc2D.setStroke(new BasicStroke(2.0f));
                for(int j = 0; j < newX[i].length; j++)
                    gc2D.drawLine(newX[i][j], newY[i][j], newX[i][j], yBase);
                gc2D.setStroke(new BasicStroke());
        }        
    }
    
    private void drawIndLines(Graphics2D gc2D, int[] allX, int[] allY, int curve)
    {
        int[] indX, indY;
        int offset = 0;
        for(int k = 0; k < indPoints[curve].length; k++)
        {
            indX = new int[indPoints[curve][k]];
            indY = new int[indPoints[curve][k]];
            for(int l = 0; l < indX.length; l++)
            {
                indX[l] = allX[offset + l];
                indY[l] = allY[offset + l];                
            }
            offset += indX.length;
            gc2D.drawPolyline(indX, indY, indX.length);
        }        
    }
    
    /** Get the default data range for plotting.
     * @param data a double[][] containing the data.
     * @return a double array containing minimum value and maximum value of the data in order.
     */
    public static double[] getDefaultRange(double[][] data, boolean isLog)
    {
        double min = 0;
        for(int i = 0; i < data.length; i++)
        {
            if(data[i].length > 0)
            {
                min = data[i][0];
                break;
            }
        }
        
        double max = min;
        for(int i = 0; i < data.length; i++)
            for(int j = 0; j < data[i].length; j++)
            {
                max = Math.max(max, data[i][j]);
                min = Math.min(min, data[i][j]);
            }
        double[] range;
        if(!isLog)
        {
            range = optDivisions(6, min, max);
        }
        else
        {
            range = optDivisions(6, Math.log(min) / Math.log(10), Math.log(max) / Math.log(10));
            range[0] = Math.pow(10, range[0]);
            range[1] = Math.pow(10, range[1]);
        }
/*        
        double[] range = new double[2];
        if(max == min)
        {
            range[0] = min - 1;
            range[1] = max + 1;
        }
        else
        {
            if(!isLog)
            {
                range[0] = min - (max - min) / 20;
                range[1] = max + (max - min) / 20;
            }
            else
            {
                range[0] = Math.pow(10, 0.05 * (21 * Math.log(min) - Math.log(max)) / Math.log(10));
                range[1] = Math.pow(10, 0.05 * (21 * Math.log(max) - Math.log(min)) / Math.log(10));
            }
        }
*/
        return range;
    }

    // Calculate divisions for auto-scale with a being either 1, 2, or 5.
    private static double[] calcDivisions(int maxSteps, double min, double max, int a)
    {
        double diff = max - min;
        double adjMax, adjMin;
        int numSteps;
        if(diff == 0)
        {
            if(max > 0)
            {
                adjMax = Math.pow(10, Math.ceil(Math.log(max) / Math.log(10)));
                adjMin = 0;
                numSteps = 5;
            }
            else if(max < 0)
            {
                adjMin = -Math.pow(10, Math.ceil(Math.log(-max) / Math.log(10)));
                adjMax = 0;
                numSteps = 5;
            }
            else
            {
                adjMin = min - 1;
                adjMax = max + 1;
                numSteps = 2;
            }
        }
        else
        {
            double ld = Math.floor(Math.log(diff) / Math.log(10));
            if(min > 0 && min < Math.pow(10, ld))
                min = 0;
            double stepSize = Math.pow(10, ld) / a;
            adjMax = Math.ceil(max / stepSize) * stepSize;
            adjMin = Math.floor(min / stepSize) * stepSize;
            double adjDiff = adjMax - adjMin;
            numSteps = (int)(adjDiff / stepSize + .1);
            while(numSteps > maxSteps)
            {
                stepSize = Math.pow(10, ++ld) / a;
                numSteps = (int)(adjDiff / stepSize + .1);
            }
            adjMin = Math.floor(min / stepSize) * stepSize;
            adjMax = Math.ceil(max / stepSize) * stepSize;
        }
        double[] list = {adjMin, adjMax, numSteps};
        
        return list;
    }
    
    /** Find the best aoto-sacale among divisors 1, 2 and 5.
     * @param maxSteps Maximum number of steps allowed.
     * @param min Mimimum value.
     * @param max maximum value.
     * @return A double array containing adjasted min, adjasted max and number of divisions.
     */
    public static double[] optDivisions(int maxSteps, double min, double max)
    {
        double[] list1 = calcDivisions(maxSteps, min, max, 1);
        double[] list2 = calcDivisions(maxSteps, min, max, 2);
        double[] list3 = calcDivisions(maxSteps, min, max, 5);
        int i = 1;
        if((list2[1] - list2[0] < list1[1] - list1[0]) ||
           (list2[1] - list2[0] == list1[1] - list1[0] && list2[2] > list1[2]))
        {
            i = 2;
        }
        if(i == 1)
        {
            if((list3[1] - list3[0] < list1[1] - list1[0]) ||
               (list3[1] - list3[0] == list1[1] - list1[0] && list3[2] > list1[2]))
            {
                i = 3;
            }
        }
        if(i == 2)
        {
            if((list3[1] - list3[0] < list2[1] - list2[0]) ||
               (list3[1] - list3[0] == list2[1] - list2[0] && list3[2] > list2[2]))
            {
                i = 3;
            }
        }
        if(i == 1) return list1;
        if(i == 2) return list2;
        return list3;
    }
    
    // Calculate regression parameters: intersect, slope, stdDeviation, corrCoefficient.
    private void regressionParameters(double[] x, double[] y)
    {   
        nRegressionData = x.length;
        
        // Calculate x mean, y mean, x square sum, y square sum and xy sum.
        double xMean = x[0];
        double yMean = y[0];
        double x2Sum = x[0] * x[0];
        double y2Sum = y[0] * y[0];
        double xySum = x[0] * y[0];
        for(int i = 1; i < nRegressionData; i++)
        {
            xMean += x[i];
            yMean += y[i];
            x2Sum += x[i] * x[i];
            y2Sum += y[i] * y[i];
            xySum += x[i] * y[i];
        }
        xMean /= nRegressionData;
        yMean /= nRegressionData;
        
        // Calculate parameters
        double numerator = 0;
        double denominator = 0;
        for(int i = 0; i < nRegressionData; i++)
        {
            double difference = x[i] - xMean;
            numerator += y[i] * difference;
            denominator += difference * difference;
        }
        slope = numerator / denominator;
        intersect = yMean - slope * xMean;
        stdDeviation = Math.sqrt((y2Sum - intersect * nRegressionData * yMean - slope * xySum)
                       / (nRegressionData - 2));
        corrCoefficient = (xySum - nRegressionData * xMean * yMean) 
                          / Math.sqrt(x2Sum - nRegressionData * xMean * xMean) 
                          / Math.sqrt(y2Sum - nRegressionData * yMean * yMean);
    }
    
    // Calculate x, y values of line ends.
    private double[][] lineEnds(double intersect, double slope)
    {
        double[][] xy = new double[2][2];
        double yAtMinX = intersect + slope * minX;
        double yAtMaxX = intersect + slope * maxX;
        double xAtMaxY = (maxY - intersect) / slope;
        double xAtMinY = (minY - intersect) / slope;
        if(yAtMinX >= minY && yAtMinX <= maxY && yAtMaxX >= minY && yAtMaxX <= maxY)
        {
            xy[0][0] = minX;
            xy[0][1] = yAtMinX;
            xy[1][0] = maxX;
            xy[1][1] = yAtMaxX;
        }
        else if(yAtMinX >= minY && yAtMinX <= maxY)
        {
            xy[0][0] = minX;
            xy[0][1] = yAtMinX;
            if(slope > 0)
            {
                xy[1][0] = xAtMaxY;
                xy[1][1] = maxY;
            }
            else
            {
                xy[1][0] = xAtMinY;
                xy[1][1] = minY;
            }
        }
        else if(yAtMaxX >= minY && yAtMaxX <= maxY)
        {
            xy[1][0] = maxX;
            xy[1][1] = yAtMaxX;
            if(slope > 0)
            {
                xy[0][0] = xAtMinY;
                xy[0][1] = minY;
            }
            else
            {
                xy[0][0] = xAtMaxY;
                xy[0][1] = maxY;
            }
        }
        return xy;
    }

    private static int[] getFrequency(double[] data, double min, double max, double interSize)
    {
        int nInter = (int)((max - min) /interSize) + 1;
        int frequency, index;
        int[] frequencies = new int[nInter];
        for(int i = 0; i < nInter; i++)
            frequencies[i] = 0;
        for(int i = 0; i < data.length; i++)
        {
            index = (int)((data[i] - min) / interSize); 
            frequencies[index] = frequencies[index] + 1;
        }
        return frequencies;
    }
    
    private static double getMean(double[] data)
    {
        int n = data.length;
        double x = data[0];
        for(int i = 1; i < n; i++) 
            x += data[i];
        return x / n;
    }
    
    private static double getMedian(double[] data)
    {
        int n = data.length;
        double[] temp = (double[])data.clone();
        Arrays.sort(temp);        
        if(n % 2 == 0)
            return 0.5 * (temp[n / 2] + temp[n / 2 + 1]);
        return temp[(n + 1) / 2];
    }
    
    private static double getStdDev(double[] data)
    {
        int n = data.length;
        double x = data[0] * data[0];
        for(int i = 1; i < n; i++)
            x += data[i] * data[i];
        double mean = getMean(data);
        return Math.sqrt((x - n * mean * mean) / (n - 1));
    }
    
    //--------------------------------------------------------------------------
    private double[][] dataX, dataY;
    private double maxX, maxY, minX, minY, xLineX, yLineY;
    private int[][] newX, newY;    
    private String title, labelX, labelY, regression, percentage;
    private String[] name;
    private int[] symbol;
    private Color[] color, legendColor, addedLineColor;
    private boolean xLine, yLine, uLine, rLine, pLine, hGrid, vGrid, isLogX, isLogY, isHistogram;
    private Rectangle2D.Double legendArea = null;
    private final static Color bg = Color.white;
    private final static Color fg = Color.black;
    private int topInset = 30;
    private int bottomInset = 60;
    private int leftInset = 90;
    private int rightInset = 25;
    private String legendLocation = null;
    private int nHDivi = 5;
    private int nVDivi = 5;
    private int markLengthX = 6;
    private int markLengthY = 6;    
    private int nTickX = 4;
    private int nTickY = 4;
    private int tickLengthX = 4;
    private int tickLengthY = 4;
    private double scale = 1.0;
    private int xOff = 0;
    private int yOff = 0;    
    private Font titleFont, labelFont, legendFont, numberFont;
    private Graphics2D gc2D = null;
    private Point2D start, last;
    private double legendX, legendY, intersect, slope, stdDeviation, corrCoefficient;
    private int nRegressionData;
    private int plotWidth, plotHeight, nCurve;
    private DecimalFormat dFormat, nFormat, formatX, formatY;
    private JFrame frame;
    private PrinterJob printerJob;
    private PageFormat pageFormat;
    private PrintRequestAttributeSet attributes;
    private double intervalSize;
    private boolean baseline;
    private String baseLabel;
    private int baseX, meanX, medianX;
    private double baselineX;
    private int[][] indPoints;
    private JFileChooser fileChooser = new JFileChooser();
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPopupMenu jPopupMenu1;
    private javax.swing.JSlider jSlider1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JDialog scaleDialog;
    // End of variables declaration//GEN-END:variables
    
}
