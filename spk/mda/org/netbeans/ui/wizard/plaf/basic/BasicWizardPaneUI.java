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

package org.netbeans.ui.wizard.plaf.basic;

import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.OptionPaneUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.awt.GridBagLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import java.awt.GridLayout;
import javax.swing.BorderFactory;
import java.awt.GridBagConstraints;
import org.netbeans.ui.wizard.JWizardPane;
import org.netbeans.ui.wizard.plaf.WizardPaneUI;
import java.util.ResourceBundle;

/**
 *
 * <pre>
 *         --------------------------------------------
 *         |            | StepTitle                   |
 *         |            |-----------------------------|
 *         |     C      |                             |
 *         |     o      |                             |
 *         |     n      |                             |
 *         |     t      |                             |
 *         |     e      |          StepPanel          |
 *         |     n      |                             |
 *         |     t      |                             |
 *         |            |                             |
 *         |            |                             |
 *         |            |                             |
 *         --------------------------------------------
 *         |            | LeftOptions    RightOptions |
 *         --------------------------------------------
 * </pre>
 */

public class BasicWizardPaneUI extends WizardPaneUI { 

    public static final int MinimumWidth = 700;
    public static final int MinimumHeight = 450;
    private static boolean DEBUG = false;

    private ActionListener buttonListener = new ActionListener() {
	    public void actionPerformed(ActionEvent e){
		wizardPane.setValue(new Integer(-1));
		if (e.getSource() == lastButton) wizardPane.setValue(new Integer(JWizardPane.LAST_OPTION));
      		if (e.getSource() == nextButton) 
                    wizardPane.setValue(new Integer(JWizardPane.NEXT_OPTION));
		if (e.getSource() == previousButton) wizardPane.setValue(new Integer(JWizardPane.PREVIOUS_OPTION));
		if (e.getSource() == finishButton) wizardPane.setValue(new Integer(JWizardPane.FINISH_OPTION));
		if (e.getSource() == cancelButton) wizardPane.setValue(new Integer(JWizardPane.CANCEL_OPTION));
		if (e.getSource() == helpButton) {
		    wizardPane.setValue(new Integer(JWizardPane.HELP_OPTION));
		    wizardPane.getCurrentStep().getStepDescription().getHelpAction().actionPerformed(e);
		}
	    }};

    /** 
     * real buttons to be placed instead of the options
     */
    private final JButton nextButton = new JButton();
    private final JButton previousButton = new JButton();
    private final JButton lastButton = new JButton();
    private final JButton helpButton = new JButton();
    private final JButton finishButton = new JButton();
    private final JButton cancelButton = new JButton();
 
    {
	/** 
	 * button initialization
	 */
       	ResourceBundle rb = ResourceBundle.getBundle("org.netbeans.ui.wizard.plaf.basic.resources.bundle"); 

	nextButton.setText( rb.getString("Button.Next.Text"));
	nextButton.setMnemonic( rb.getString("Button.Next.Mnemonic").charAt(0));
	nextButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("icons/next.gif")));
	nextButton.setHorizontalTextPosition(SwingConstants.LEADING);
	nextButton.addActionListener( buttonListener);

	previousButton.setText( rb.getString("Button.Previous.Text"));
	previousButton.setMnemonic(rb.getString("Button.Previous.Mnemonic").charAt(0));
	previousButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("icons/back.gif")));
	previousButton.addActionListener( buttonListener);

	lastButton.setText( rb.getString("Button.Last.Text"));
	lastButton.setMnemonic(rb.getString("Button.Last.Mnemonic").charAt(0));
	lastButton.addActionListener( buttonListener);

	helpButton.setText( rb.getString("Button.Help.Text"));
	helpButton.setMnemonic(rb.getString("Button.Help.Mnemonic").charAt(0));
	helpButton.addActionListener( buttonListener);

	finishButton.setText( rb.getString("Button.Finish.Text"));
	finishButton.setMnemonic(rb.getString("Button.Finish.Mnemonic").charAt(0));
	finishButton.addActionListener( buttonListener);

	cancelButton.setText( rb.getString("Button.Cancel.Text"));
	cancelButton.setMnemonic(rb.getString("Button.Cancel.Mnemonic").charAt(0));
	cancelButton.addActionListener( buttonListener);

    }

    /** 
     * JWizardPane that the reciever is providing the look and feel for.
     */
    protected JWizardPane wizardPane;

    protected Component stepPanel = null;

    protected JComponent titlePanel = null;

    protected JComponent leftOptions = null; 

    protected JComponent rightOptions = null; 

    protected Dimension minimumSize;

    protected PropertyChangeListener propertyChangeListener;

    /**
     * Creates a new BasicWizardPaneUI instance.
     */
    public static ComponentUI createUI(JComponent x) {
	return new BasicWizardPaneUI();
    }

    /**
     * Installs the reciever as the L&F for the passed in JWizardPane
     */
    public void installUI(JComponent c) {
	wizardPane = (JWizardPane) c;
        //installDefaults();
        wizardPane.setLayout(createLayoutManager());
	installComponents();
        installListeners(); 
        //installKeyboardActions();
	System.out.println("BasicWizardPaneUI-installUI");
    }

    /**
     * Removes the receiver from the L&F controller of the passed in split
     * pane.
     */
    public void uninstallUI(JComponent c) {
        uninstallComponents();
        wizardPane.setLayout(null);
	//uninstallKeyboardActions();
        uninstallListeners();
        uninstallDefaults();
	wizardPane = null;
    }

    protected void installDefaults() {
        LookAndFeel.installColorsAndFont(wizardPane, "OptionPane.background","OptionPane.foreground", "OptionPane.font");
	LookAndFeel.installBorder(wizardPane, "OptionPane.border");
        minimumSize = UIManager.getDimension("OptionPane.minimumSize");
	wizardPane.setOpaque(true);
    }

    protected void uninstallDefaults() {
	LookAndFeel.uninstallBorder(wizardPane);
    }

    protected void installComponents() {

	this.addContent();
	this.updateStepTitle();
	this.updateStep();
	this.addSeparator();
	//this.updateButtons();

	if (DEBUG) {
	    System.out.println("BasicWizardPaneUI-installComponents()");
	} // end of if ()
    }

    protected void uninstallComponents() {
	wizardPane.removeAll();
    }

    /** 
     */
    protected LayoutManager createLayoutManager() {
	return new GridBagLayout();
    }

    /**
     */
    protected void installListeners() {
        if ((propertyChangeListener = createPropertyChangeListener()) != null) {
            wizardPane.addPropertyChangeListener(propertyChangeListener);
        }
    }

    /**
     */
    protected void uninstallListeners() {
        if (propertyChangeListener != null) {
            wizardPane.removePropertyChangeListener(propertyChangeListener);
            propertyChangeListener = null;
        }
    }

    /**
     */
    protected PropertyChangeListener createPropertyChangeListener() {
        return new PropertyChangeHandler();
    }

    /**
     *
     */
    protected void updateStepTitle(){

	if (titlePanel != null) wizardPane.remove(titlePanel);

	JLabel l = new JLabel(wizardPane.getStepTitleText());
	l.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, l.getForeground()));
	//l.setForeground(wizardPane.getStepTitleColor());

	java.awt.GridBagConstraints gbc  = new java.awt.GridBagConstraints();
        gbc.fill = java.awt.GridBagConstraints.BOTH;
        gbc.insets = new java.awt.Insets(12, 12, 12, 12);
	gbc.gridx = 1;   gbc.gridy = 0;
	gbc.gridwidth = GridBagConstraints.REMAINDER; 
        gbc.anchor = java.awt.GridBagConstraints.WEST;
	wizardPane.add(l , gbc);
	titlePanel = l;
	if (wizardPane.isVisible()) wizardPane.revalidate();

	if (DEBUG) {
	    System.out.println("BasicWizardPaneUI-updateStepTitleText()"); 	    
	} // end of if ()
	
    }

    /**
     */
    protected Container createSeparator() {
        return new JSeparator();
    }

    /**
     */
    protected void addSeparator(){

	java.awt.GridBagConstraints gbc  = new java.awt.GridBagConstraints();
        gbc.fill = java.awt.GridBagConstraints.BOTH;
	gbc.gridx = 0;   gbc.gridy = 2;
	gbc.weightx = 1.0;
	gbc.gridwidth = GridBagConstraints.REMAINDER; 
        gbc.anchor = java.awt.GridBagConstraints.CENTER;
	wizardPane.add(createSeparator(), gbc); 

	if (DEBUG) {
	    System.out.println("BasicWizardPaneUI-addSeparator()");
	} // end of if ()
    }

    /**
     */
    protected void updateStep(){

	if (wizardPane.getStepPanel() != null) {

	    if (stepPanel != null) wizardPane.remove(stepPanel);

	    GridBagConstraints gbc  = new GridBagConstraints();
	    gbc.fill = GridBagConstraints.BOTH;
	    gbc.gridx = 1;
	    gbc.gridy = 1;
	    gbc.weightx = 1.0;
	    gbc.weighty = 1.0;
	    gbc.gridwidth = GridBagConstraints.REMAINDER; 
	    gbc.anchor = GridBagConstraints.CENTER;
	    wizardPane.add(wizardPane.getStepPanel(), gbc);
	    if (wizardPane.isVisible()) wizardPane.revalidate();	    
	    stepPanel = wizardPane.getStepPanel();

	    if (DEBUG) {
		System.out.println("BasicWizardPaneUI-updateStep()"); 
	    } // end of if ()
	}
    }

    /**
     */
    protected void addContent(){

	if (wizardPane.getContentPanel() != null) {
	    java.awt.GridBagConstraints gbc  = new java.awt.GridBagConstraints();
	    gbc.fill = java.awt.GridBagConstraints.BOTH;
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.gridheight = 2;
	    gbc.anchor = java.awt.GridBagConstraints.CENTER;
	    wizardPane.add(wizardPane.getContentPanel(), gbc); 

	    if (DEBUG) {
		System.out.println("BasicWizardPaneUI-addContent()");
	    } // end of if ()
	}
    }

    /** 
     */
    private JButton getButton(int b) {

	switch (b) {

	case JWizardPane.PREVIOUS_OPTION: 
	    previousButton.setEnabled(true);
	    return previousButton; 

	case JWizardPane.PREVIOUS_OPTION_DISABLED: 
	    previousButton.setEnabled(false);
	    return previousButton; 

	case  JWizardPane.NEXT_OPTION:
	    nextButton.setEnabled(true);
	    return nextButton;	    

	case  JWizardPane.NEXT_OPTION_DISABLED:
	    nextButton.setEnabled(false);
	    return nextButton;	    

	case  JWizardPane.LAST_OPTION:
	    lastButton.setEnabled(true);
	    return lastButton;

	case  JWizardPane.LAST_OPTION_DISABLED:
	    lastButton.setEnabled(false);
	    return lastButton;

	case  JWizardPane.FINISH_OPTION:
	    finishButton.setEnabled(true);
	    return finishButton;	

	case  JWizardPane.FINISH_OPTION_DISABLED:
	    finishButton.setEnabled(false);
	    return finishButton;	

	case  JWizardPane.HELP_OPTION:
	    return helpButton;

	case  JWizardPane.CANCEL_OPTION:
	    return cancelButton;

	} // end of switch ()

	return cancelButton;
    }

    /** 
     */
    protected void updateLeftOptions(){

	if (leftOptions != null) wizardPane.remove(leftOptions);

        JPanel left = new JPanel();
  	left.setLayout(new GridLayout(1, 0, 5, 0)); 


  	Object[] lo =  wizardPane.getLeftOptions();

  	for (int i = 0; i < lo.length; i++) {
	    if (lo[i] instanceof JButton) {
		left.add((JComponent)lo[i]);		
	    } else {
		left.add( getButton( ((Integer)lo[i]).intValue())); 
	    }
  	} 


        java.awt.GridBagConstraints gbc  = new java.awt.GridBagConstraints();
        gbc.fill = java.awt.GridBagConstraints.VERTICAL;
        gbc.insets = new java.awt.Insets(12, 0, 12, 12);
	gbc.gridx = 1;   gbc.gridy = 3;
        gbc.anchor = java.awt.GridBagConstraints.WEST;
	leftOptions = left;
	wizardPane.add(left, gbc);
	if (wizardPane.isVisible()){
	    wizardPane.revalidate(); 
	    wizardPane.repaint(); 
	}
    }


    /**
     */
    protected void updateRightOptions(){
	if (rightOptions != null) wizardPane.remove(rightOptions);
        JPanel right = new JPanel();
  	right.setLayout( new GridLayout(1, 0, 5, 0));

  	Object[] ro = wizardPane.getRightOptions();


 	for (int i = 0; i < ro.length; i++) {
	    if (ro[i] instanceof JButton) {
		right.add((JComponent)ro[i]);		
	    } else {
		right.add( getButton(((Integer)ro[i]).intValue())); 
	    }
  	} 



        java.awt.GridBagConstraints gbc  = new java.awt.GridBagConstraints();
        gbc.fill = java.awt.GridBagConstraints.VERTICAL;
        gbc.insets = new java.awt.Insets(12, 12, 12, 12);
	gbc.gridx = 2;   gbc.gridy = 3;
        gbc.anchor = java.awt.GridBagConstraints.EAST;
	rightOptions = right;
	wizardPane.add(right, gbc);
	if (wizardPane.isVisible()){
	    wizardPane.revalidate(); 
	    wizardPane.repaint(); 
	}
    }



    /**
     * Returns the minimum size the option pane should be. Primarily
     * provided for subclassers wishin to offer a different minimum size.
     */
    public Dimension getMinimumOptionPaneSize() {
        if (minimumSize == null) {
            //minimumSize = UIManager.getDimension("OptionPane.minimumSize");
            // this is called before defaults initialized?!!!
            return new Dimension(MinimumWidth, MinimumHeight);
        }
	return new Dimension(minimumSize.width,
			     minimumSize.height);
    }

    /**
     * If c is the JWizardPane the reciever is contained in, the preferred
     * size that is returned is the maximum of the preferred size of
     * the LayoutManager for the JWizardPane, and
     * <code>getMinimumOptionPaneSize</code>.
     */
    public Dimension getPreferredSize(JComponent c) {
	if ((JWizardPane)c == wizardPane) {
	    Dimension            ourMin = getMinimumOptionPaneSize();
	    LayoutManager        lm = c.getLayout();

	    if (lm != null) {
		Dimension         lmSize = lm.preferredLayoutSize(c);

		if (ourMin != null)
		    return new Dimension
			(Math.max(lmSize.width, ourMin.width),
			 Math.max(lmSize.height, ourMin.height));
		return lmSize;
	    }
	    return ourMin;
	}
	return null;
    }

    /**
     * Messages getPreferredSize.
     */
    public Dimension getMinimumSize(JComponent c) {
	return getPreferredSize(c);
    }

    /**
     * Messages getPreferredSize.
     */
    public Dimension getMaximumSize(JComponent c) {
	return getPreferredSize(c);
    }

    /**
     *
     */
    public void selectDefaultButton (JWizardPane wp) {
	Object b = wp.getDefaultButton(); 

	if (b instanceof Integer) {
	    b = getButton(((Integer)b).intValue());
	}
	if (b instanceof JButton) {
	    JRootPane root = SwingUtilities.getRootPane((JButton)b);
	    if (root != null) {
		root.setDefaultButton((JButton)b);
	    }
	}
    }

    /**
     * This inner class is marked &quot;public&quot; due to a compiler bug.
     * This class should be treated as a &quot;protected&quot; inner class.
     * Instantiate it only within subclasses of BasicOptionPaneUI.
     */  
    public class PropertyChangeHandler implements PropertyChangeListener {
        /**
         * If the source of the PropertyChangeEvent <code>e</code> equals the
         * wizardPane and is one of the ICON_PROPERTY, MESSAGE_PROPERTY,
         * OPTIONS_PROPERTY or INITIAL_VALUE_PROPERTY,
         * validateComponent is invoked.
         */
        public void propertyChange(PropertyChangeEvent e) {
            if(e.getSource() == wizardPane) {
	        String changeName = e.getPropertyName();

	        if(changeName.equals(JWizardPane.LEFT_OPTIONS_PROPERTY)) updateLeftOptions();
		if (changeName.equals(JWizardPane.RIGHT_OPTIONS_PROPERTY)) updateRightOptions();
		if (changeName.equals(JWizardPane.CONTENT_PANEL_PROPERTY)) addContent();
		if (changeName.equals(JWizardPane.STEP_PANEL_PROPERTY)) updateStep();
		if (changeName.equals(JWizardPane.STEP_TITLE_TEXT_PROPERTY)) updateStepTitle();
		if (changeName.equals(JWizardPane.STEP_TITLE_COLOR_PROPERTY)) updateStepTitle();

		if (changeName.equals(JWizardPane.DEFAULT_BUTTON_PROPERTY)) {
		    ((JWizardPane)e.getSource()).selectDefaultButton();
		}
	    }
	}

    }

}
