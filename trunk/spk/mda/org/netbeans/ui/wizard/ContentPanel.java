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

class ContentPanel extends ImagedPanel {

    protected int selectedIndex = -1;

    private JList contentList;

    private ContentCellRenderer cellRenderer;

    private JLabel title;


    /** */
    transient protected  boolean contentNumbered = true;
    /** */
    transient protected boolean visibleContent = true;
    /** */
    transient protected  Color selectedColor = new Color(204, 204, 255);
    /** */
    transient protected  int selectedMetod = JWizardPane.SELECT_BY_FONT;
    /** */
    transient protected boolean accessibleSupport = true;
    /** */
    transient protected String titleText = "Steps:";

    public ContentPanel(){ 
	super(null);
	this.setBorder(BorderFactory.createEmptyBorder(12, 12, 12, 12));

	contentList = new JList(){
		public boolean isFocusTraversable(){
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
		public boolean isFocusTraversable(){
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

    public void setTitle(String value){		
	if (value != title.getText()) {		
	    title.setText(value);		
	} 					
    }						

    public void setTitleColor(Color color){
	if (title.getForeground() != color){
	    title.setForeground(color);
	    title.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, color));
	}
    }

    public Color getTitleColor(){
	return title.getForeground();
    }

    public  void setSelectedColor(Color color){
	selectedColor = color;
    }

    public Color getSelectedColor(){
	return selectedColor;
    }

    public void setSelectMetod(int value){
	if (selectedMetod != value) {
	    selectedMetod = value;	
	    if (isVisible() && visibleContent) repaint();
	}
    }

    public int getSelectedMetod(){
	return selectedMetod;
    }

    public void setForeground(Color color){
	if (title.getForeground() == this.getForeground()) title.setForeground(color);	    
  	super.setForeground(color);
    }

    public void setBackground(Color color){
	super.setBackground(color);
	title.setBackground(color);
    }

    public void setContentNumbered(boolean flag){
	if (contentNumbered != flag ) {
	    contentNumbered = flag;
	    if (isVisible()) repaint();
	} 
    }

    public boolean getContentNumbered(){
	return contentNumbered;
    }

    public void setVisibleContent(boolean flag){
	if (visibleContent != flag) {
	    visibleContent = flag;
	    title.setVisible(flag);
	    if (isVisible()) repaint();
	} 
    }

    public boolean getVisibleContent(){
	return visibleContent;
    }

    public void setContentItems(String[] content) {
	if (contentList != null) {
	    contentList.setListData(content);
	    contentList.revalidate();
	    contentList.repaint();
	    //contentLabelPanel.setVisible(content.length > 0);
	}
    }

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

    public int  getSelectedIndex(){
	return selectedIndex;
    }

    public Dimension getPreferredSize(){
	return(new Dimension(204, 0));
    }

    public Dimension getMinimumSize(){
        return getPreferredSize();
    }

    private static class ContentCellRenderer extends JPanel implements ListCellRenderer {
        

        JTextArea ta = new JTextArea();
        JLabel numberLabel = new JLabel();
        ContentPanel owner;

	public boolean isFocusTraversable(){
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
 
class ImagedPanel extends JComponent {

    Image image = null;
    int alignment = SwingConstants.CENTER;
    boolean visibleImage = true;

    public ImagedPanel(Image im) {
	setImage(im);
	setLayout(new BorderLayout());
	setOpaque(true);
    }

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
        
    public void setImageAlignment(int align) {
	if (alignment != align ) {
	    alignment = align;
	    repaint();	    
	} 
    }

    public int getImageAlignment(){
	return alignment;
    }

    public void setImage(Image image) {
	if (this.image != image) {
	    this.image = image;	    
	    if (visibleImage && isVisible()) {
		repaint();		
	    } 
	} 
    }

    public Image getImage(){
	return image;
    }

    public boolean getVisibleImage(){
	return visibleImage;
    }

    public void setVisibleImage(boolean flag){
	if (visibleImage != flag) {
	    visibleImage = flag;
	    if (isVisible()) repaint();		
	}
    }
}

