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

import java.util.Vector;
import java.awt.print.*;
import java.awt.font.*;
import java.awt.*; 

/**
 * This class implements Printable interface that controls how to print.
 * @author  Jiaji Du
 */
public class Printer implements Printable{
    
    /** Creates a new instance of Printer.
     * @param text a String object to initialize the field text.
     */
    public Printer(String text) {
        this.text = text;
    }

    /** This method implements print() in Printable interface.
     * @param gc the graphiocs context for printer.
     * @param pageFormat the pageFormat object.
     * @param pageIndex the page index.
     * @return PAGE_EXISTS or NO_SUCH_PAGE.
     */    
    public int print(Graphics gc, PageFormat pageFormat, int pageIndex)
    {
        // Get graphic context
        Graphics2D g2 = (Graphics2D)gc;
        
        // Get upper-left corner coordinates
        int lineInsetX  = (int)pageFormat.getImageableX();
        int lineInsetY  = (int)pageFormat.getImageableY();
 
        // Set font for the text
        g2.setFont(printFont);
        FontRenderContext frc = g2.getFontRenderContext();

        // Get LineMetrics object
        LineMetrics lineMetrics = printFont.getLineMetrics(text,frc);
        
        // Set color and stroke
        g2.setPaint(Color.black);
        g2.setStroke(new BasicStroke());
       
        if(firstEnter)
        {
            firstEnter = false;
            
            // Get text height
            textHeight = (int)lineMetrics.getHeight();

            // Calculate the number of lines per page
            int nLinesPage = ((int)pageFormat.getImageableHeight()) / textHeight; 
                
            // Find the beginning index of each page
            beginIndex.add(new Integer(0));
            int nLines = 1;
            int nLinesPages = nLinesPage;
            int textLength = text.length();
            for(int i = 0; i < textLength; i++)
            {
                if(text.charAt(i) == '\n') 
                {
                    nLines++;
                    if(nLines == nLinesPages)
                    {
                        beginIndex.add(new Integer(i + 1)); 
                        nLinesPages += nLinesPage; 
                    }
                }
            }
            beginIndex.add(new Integer(text.length()));
        }
        
        if(pageIndex >= beginIndex.size() - 1) 
            return NO_SUCH_PAGE;  
        
        // Draw text content line by line
        int j = ((Integer)beginIndex.get(pageIndex)).intValue();
        int begin = ((Integer)beginIndex.get(pageIndex)).intValue();
        int end   = ((Integer)beginIndex.get(pageIndex + 1)).intValue();      
        for(int i = begin; i < end; i++) 
        {                        
            if(text.charAt(i) == '\n') 
            {
                lineInsetY += textHeight;
                g2.drawString(text.substring(j,i),lineInsetX,lineInsetY); 
                j = i + 1; 
            }      
        }                 

        return PAGE_EXISTS;
    }
        
    private String text = null;
    private boolean firstEnter = true;
    private int textHeight = 0;
    private Vector<Integer> beginIndex = new Vector<Integer>();  
    private PageFormat pageFormat;
        
    // Print font
    private Font printFont = new Font("Courier", Font.PLAIN, 9);  
}
