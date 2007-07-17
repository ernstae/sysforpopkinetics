/*
 *                 Sun Public License Notice
 *
 * The contents of this file are subject to the Sun Public License
 * Version 1.0 (the "License"). You may not use this file except in
 * compliance with the License. A copy of the License is available at
 * http://www.sun.com/
 *
 * The Original Code is NetBeans. The Initial Developer of the Original
 * Code is Sun Microsystems, Inc. Portions Copyright 1997-2000 Sun
 * Microsystems, Inc. All Rights Reserved.
 */
/*
 * This is the modified version of the original file.  The modification 
 * was made by RFPK University of Washington to include it in a open source
 * software product of which the licencse notice is stated bellow.
 */
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
package org.netbeans.ui.wizard;

import javax.swing.JList;
import java.awt.Color;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import java.awt.Font;
import java.awt.BorderLayout;
import javax.swing.JTextArea;
import javax.swing.BorderFactory;
import java.lang.String;
import javax.swing.SwingConstants;
import java.awt.Insets;
import java.awt.Dimension;
import javax.swing.UIManager;
import java.awt.Component;
import javax.swing.JComponent;
import java.awt.Graphics;
import java.awt.Image;

/** This class defines the content panel of the wizard.
 */
public class ContentPanel extends ImagedPanel {

    /** The selected index. */
    protected int selectedIndex = -1;

    private JList contentList;

    private ContentCellRenderer cellRenderer;

    private JLabel title;


    /** The content numbered indicator. */
    transient protected  boolean contentNumbered = true;
    /** The content visibility indicator. */
    transient protected boolean visibleContent = true;
    /** The selected color. */
    transient protected  Color selectedColor = new Color(204, 204, 255);
    /** The selection method. */
    transient protected  int selectedMetod = JWizardPane.SELECT_BY_FONT;
    /** The accessible support indicator. */
    transient protected boolean accessibleSupport = true;
    /** The title text. */
    transient protected String titleText = "Steps:";

    /** Construct a content pane. */
    public ContentPanel(){ 
	super(null);
	this.setBorder(BorderFactory.createEmptyBorder(12, 12, 12, 12));

	contentList = new JList(){
		public boolean isFocusable(){
		    return accessibleSupport;
		}
	    };

	cellRenderer = new ContentCellRenderer(this);
	contentList.setCellRenderer(cellRenderer);
	cellRenderer.setOpaque(false);
	contentList.setOpaque(false);

	contentList.setEnabled(accessibleSupport);

	JScrollPane scroll = new JScrollPane(contentList);
	scroll.setBorder(null);
	scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	//scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	scroll.getViewport().setOpaque(false);
	scroll.setOpaque(false);

	title = new JLabel(titleText){
		public boolean isFocusable(){
		    return accessibleSupport;
		}
	    };


	setTitleColor(Color.black);
	title.setFont(title.getFont().deriveFont(Font.BOLD));
	title.setOpaque(false);

	title.getAccessibleContext().setAccessibleName("Steps:");
	title.getAccessibleContext().setAccessibleDescription("Content steps");

  	JPanel contentLabelPanel = new JPanel(new BorderLayout());
  	contentLabelPanel.setBorder(BorderFactory.createEmptyBorder(0, 0, 12, 0));
  	contentLabelPanel.setOpaque(false);
      	contentLabelPanel.add(title, BorderLayout.NORTH);
	add(contentLabelPanel, BorderLayout.NORTH);
        	
	add(scroll, BorderLayout.CENTER);
	setBackground(Color.white);
    }

    /** Setter for title.
     * @param value a string for the title.
     */
    public void setTitle(String value){		
	if (value != title.getText()) {		
	    title.setText(value);		
	} 					
    }						

    /** Setter for title color.
     * @param color a Color object for the title color.
     */
    public void setTitleColor(Color color){
	if (title.getForeground() != color){
	    title.setForeground(color);
	    title.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, color));
	}
    }

    /** Getter for title color.
     * @return the title color.
     */
    public Color getTitleColor(){
	return title.getForeground();
    }

    /** Setter for selected color.
     * @param color a Color object for the selected color.
     */
    public  void setSelectedColor(Color color){
	selectedColor = color;
    }
    
    /** Getter for selected color.
     * @return the selected color.
     */
    public Color getSelectedColor(){
	return selectedColor;
    }
    
    /** Setter for selection method.
     * @param value an int to specify the selection method.
     */
    public void setSelectMetod(int value){
	if (selectedMetod != value) {
	    selectedMetod = value;	
	    if (isVisible() && visibleContent) repaint();
	}
    }
    
    /** Getter for selected method.
     * @return the selected method.
     */
    public int getSelectedMetod(){
	return selectedMetod;
    }

    /** Setter for foreground.
     * @param color a Color object for the foreground.
     */
    public void setForeground(Color color){
	if (title.getForeground() == this.getForeground()) title.setForeground(color);	    
  	super.setForeground(color);
    }
    
    /** Setter for background.
     * @param color a Color object for the background.
     */
    public void setBackground(Color color){
	super.setBackground(color);
	title.setBackground(color);
    }
    
    /** Setter for content numbered flag.
     * @param flag a boolean to specify if the content being numbered.
     */
    public void setContentNumbered(boolean flag){
	if (contentNumbered != flag ) {
	    contentNumbered = flag;
	    if (isVisible()) repaint();
	} 
    }

    /** Getter for content numbered flag.
     * @return the content numbered flag.
     */
    public boolean getContentNumbered(){
	return contentNumbered;
    }

    /** Setter for visible content flag.
     * @param flag a boolean for the visible content.
     */
    public void setVisibleContent(boolean flag){
	if (visibleContent != flag) {
	    visibleContent = flag;
	    title.setVisible(flag);
	    if (isVisible()) repaint();
	} 
    }

    /** Getter for visible content flag.
     * @return the visible content flag.
     */
    public boolean getVisibleContent(){
	return visibleContent;
    }

    /** Setter for content items.
     * @param content a String array for the content.
     */    
    public void setContentItems(String[] content) {
	if (contentList != null) {
	    contentList.setListData(content);
	    contentList.revalidate();
	    contentList.repaint();
	    //contentLabelPanel.setVisible(content.length > 0);
	}
    }

    /** Setter for selected index.
     * @param index an int for the selected index.
     */    
    public void setSelectedIndex(int index) {
       	if (index != selectedIndex) {
	    selectedIndex = index;

	    contentList.ensureIndexIsVisible(index);
	    if (isVisible() && visibleContent) repaint();
	}
    }

//      /** */
//      public void setSelectedItem(String value) {
//         	if (contentItems != null)) {
//  	while (contentList) {
	    
//  	} // end of while ()
	

//  	    selectedItem = value;
//  	    contentList.ensureIndexIsVisible(index);
//  	    if (isVisible() && visibleContent) repaint();
//  	} 
	
//  	//  	if (cellRenderer != null) {
//  	//  	    cellRenderer.setSelectedIndex(index);
//  	//  	    contentList.ensureIndexIsVisible(index);
//  	//  	    // Fix of #10787.
//  	//  	    // This is workaround for swing bug - BasicListUI doesn't ask for preferred
//  	//  	    // size of rendered list cell as a result of property selectedIndex change. 
//  	//  	    // It does only on certain JList property changes (e.g. fixedCellWidth).
//  	//  	    // Maybe subclassing BasicListUI could be better fix.
//  	//contentList.setFixedCellWidth(0);
//  	// contentList.setFixedCellWidth(-1);
//  	//  	}
//      }

    /** Getter for selected index.
     * @return the selected index.
     */    
    public int  getSelectedIndex(){
	return selectedIndex;
    }

    /** Getter for preferred size.
     * @return the preferred size.
     */    
    public Dimension getPreferredSize(){
	return(new Dimension(204, 0));
    }

    /** Getter for minimum size.
     * @return the minimum size.
     */    
    public Dimension getMinimumSize(){
        return getPreferredSize();
    }

    private static class ContentCellRenderer extends JPanel implements ListCellRenderer {
        

        JTextArea ta = new JTextArea();
        JLabel numberLabel = new JLabel();
        ContentPanel owner;

	public boolean isFocusable(){
	    return true; //accessibleSupport;
	}

        public ContentCellRenderer(Object owner) {
	    super(new BorderLayout());

	    this.owner = (ContentPanel)owner;
            ta.setOpaque(false);
	    //ta.setBackground(Color.green);

            ta.setEditable(false);
            ta.setLineWrap(true);
            ta.setWrapStyleWord(true);
//            ta.setFont(UIManager.getFont("Label.font"));
            ta.setFont(new Font("SansSerif", Font.PLAIN, 11));
            //************************
            //int taWidth = owner.getWidth() - 12 - 12;
	    //System.out.println(Integer.toString(owner.getWidth()));
	    int taWidth = 204 - 12 - 12 - 10 - 25;
            
	    numberLabel.setHorizontalAlignment(SwingConstants.LEFT);
	    numberLabel.setVerticalAlignment(SwingConstants.TOP);
	    numberLabel.setFont(ta.getFont());
	    numberLabel.setOpaque(false);
	    numberLabel.setPreferredSize(new Dimension(25, 0));
	    add(numberLabel, BorderLayout.WEST);            
	    taWidth -= numberLabel.getWidth();

            Insets taInsets = ta.getInsets();
            ta.setSize(taWidth, taInsets.top + taInsets.bottom + 1);
	    
            add(ta, BorderLayout.CENTER);
            this.setOpaque(false);
	    this.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));
	    this.setEnabled(true );  //!!!!!!!!!!!!!!!

	}


        public Component getListCellRendererComponent(
						      JList list,
						      Object value,
						      int index,
						      boolean isSelected,
						      boolean cellHasFocus)
        {

	    ta.setVisible(owner.visibleContent);
	    numberLabel.setVisible(owner.visibleContent);

            if (owner.contentNumbered) {
		//numberLabel.setVisible(true);
                numberLabel.setText(Integer.toString(index + 1) + ".");
		numberLabel.setForeground( owner.getForeground());
            } else {
                numberLabel.setText("-");
		numberLabel.setForeground( owner.getForeground());
		//numberLabel.setVisible(false);
	    } // end of else
	    
	    switch (owner.selectedMetod){
		case  JWizardPane.SELECT_BY_FONT:
		    if (index == owner.selectedIndex) {
			ta.setFont(ta.getFont().deriveFont(Font.BOLD));
			if (numberLabel != null)
			    numberLabel.setFont(numberLabel.getFont().deriveFont(Font.BOLD));
		    } else {
			this.setOpaque(false);
			ta.setFont(ta.getFont().deriveFont(Font.PLAIN));
			if (numberLabel != null)
			    numberLabel.setFont(numberLabel.getFont().deriveFont(Font.PLAIN));
		    }
		break;
	    
		case  JWizardPane.SELECT_BY_COLOR:
		    if (index == owner.selectedIndex) {
			this.setBackground(owner.selectedColor);
			this.setOpaque(true);
		    } else {
			this.setOpaque(false);
			ta.setFont(ta.getFont().deriveFont(Font.PLAIN));
			if (numberLabel != null)
			    numberLabel.setFont(numberLabel.getFont().deriveFont(Font.PLAIN));
		    }
		break;
	    }

            ta.setText((String)value);
	    ta.setForeground( owner.getForeground());

  	    this.getAccessibleContext().setAccessibleName("Content Item " + Integer.toString(index + 1));
  	    this.getAccessibleContext().setAccessibleDescription((String) value);



            return this;
        }
    }
}

/** This class defines a image panel.
 */ 
class ImagedPanel extends JComponent {

    /** */    
    private Image image = null;
    private int alignment = SwingConstants.CENTER;
    private boolean visibleImage = true;

    /** Consruct a image panel.
     * @param im an Image object for the image panel.
     */    
    public ImagedPanel(Image im) {
	setImage(im);
	setLayout(new BorderLayout());
	setOpaque(true);
    }

    /** This method is called by the system to paint the component.
     * @param graphics the graphics context.
     */    
    protected void paintComponent(Graphics graphics) {
	graphics.setColor(getBackground());
	graphics.fillRect(0, 0, getWidth(), getHeight());
	if ((image != null) && visibleImage) {
            int x = 0, y = 0;
	    switch (alignment) {
	    case SwingConstants.CENTER:
		x = (getWidth() - image.getWidth(null)) / 2;   
		y = (getHeight() - image.getHeight(null)) / 2;
		break;
	    case SwingConstants.NORTH:
		x = (getWidth() - image.getWidth(null)) / 2;   
		break;
	    case  SwingConstants.SOUTH:
		x = (getWidth() - image.getWidth(null)) / 2;   
		y = getHeight() - image.getHeight(null);    
		break;
	    case  SwingConstants.WEST:
		y = (getHeight() - image.getHeight(null)) / 2;
		break;
	    case  SwingConstants.EAST:
		x = getWidth() - image.getWidth(null);   
		y = (getHeight() - image.getHeight(null)) / 2;		    
		break;
	    case  SwingConstants.NORTH_WEST:
		break;
    	    case  SwingConstants.NORTH_EAST:
    		x = getWidth() - image.getWidth(null);   
    		break;
  	    case  SwingConstants.SOUTH_WEST:
  		y = getHeight() - image.getHeight(null);		    
  		break;
  	    case  SwingConstants.SOUTH_EAST:
  		x = getWidth() - image.getWidth(null);   
  		y = getHeight() - image.getHeight(null);		    
		break;
	    }
	    graphics.drawImage(image, x, y, null);
	}
    }
        
    /** Setter for image alignment.
     * @param align an int for the image alignment.
     */    
    public void setImageAlignment(int align) {
	if (alignment != align ) {
	    alignment = align;
	    repaint();	    
	} 
    }

    /** Getter for image alignment.
     * @return the image alignment.
     */    
    public int getImageAlignment(){
	return alignment;
    }

    /** Setter for image.
     * @param image an Image object for the image.
     */    
    public void setImage(Image image) {
	if (this.image != image) {
	    this.image = image;	    
	    if (visibleImage && isVisible()) {
		repaint();		
	    } 
	} 
    }

    /** Getter for image.
     * @return the image.
     */    
    public Image getImage(){
	return image;
    }

    /** Getter for visibel image flag.
     * @return the visibel image flag.
     */    
    public boolean getVisibleImage(){
	return visibleImage;
    }

    /** Setter for visibel image flag
     * @param flag a boolean for the visibel image flag.
     */    
    public void setVisibleImage(boolean flag){
	if (visibleImage != flag) {
	    visibleImage = flag;
	    if (isVisible()) repaint();		
	}
    }
}

