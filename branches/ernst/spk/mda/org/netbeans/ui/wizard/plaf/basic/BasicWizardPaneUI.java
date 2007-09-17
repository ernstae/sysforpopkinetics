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
import javax.help.*;
import java.net.URL;
import uw.rfpk.mda.nonmem.MDAFrame;
import javax.help.*;

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

    /** The minimum width. */
    public static final int MinimumWidth = 700;
    /** The minimum height. */
    public static final int MinimumHeight = 450;
    private static boolean DEBUG = false;

    private ActionListener buttonListener = new ActionListener() {
	    public void actionPerformed(ActionEvent e){
		wizardPane.setValue(new Integer(-1));
		if (e.getSource() == lastButton) wizardPane.setValue(new Integer(JWizardPane.LAST_OPTION));
      		if (e.getSource() == nextButton) 
                    if (wizardPane.getCurrentStep().getStepDescription().checkingStep(wizardPane))
                        wizardPane.setValue(new Integer(JWizardPane.NEXT_OPTION));
		if (e.getSource() == previousButton) wizardPane.setValue(new Integer(JWizardPane.PREVIOUS_OPTION));
		if (e.getSource() == finishButton) wizardPane.setValue(new Integer(JWizardPane.FINISH_OPTION));
		if (e.getSource() == cancelButton) wizardPane.setValue(new Integer(JWizardPane.CANCEL_OPTION));
		if (e.getSource() == helpButton) {
		    wizardPane.setValue(new Integer(JWizardPane.HELP_OPTION));
                    CSH.setHelpIDString(helpButton, wizardPane.getCurrentStep().getStepDescription().getHelpID());
//                    wizardPane.getCurrentStep().getStepDescription().getHelpAction().actionPerformed(e);
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
	helpButton.addActionListener(new CSH.DisplayHelpFromSource(getHelpBroker()));
        helpButton.addActionListener( buttonListener);
        
	finishButton.setText( rb.getString("Button.Finish.Text"));
	finishButton.setMnemonic(rb.getString("Button.Finish.Mnemonic").charAt(0));
	finishButton.addActionListener( buttonListener);

	cancelButton.setText( rb.getString("Button.Cancel.Text"));
	cancelButton.setMnemonic(rb.getString("Button.Cancel.Mnemonic").charAt(0));
	cancelButton.addActionListener( buttonListener);

    }
    
    private HelpBroker getHelpBroker()
    {
        HelpSet hs = null;
        try
        {
            ClassLoader cl = MDAFrame.class.getClassLoader();
            String hsName = "spkhelp.hs";
            URL hsURL = HelpSet.findHelpSet(cl, hsName);
            hs = new HelpSet(null, hsURL);
        }
        catch(Exception e)
        {
            System.out.println(e);
        }
        return hs.createHelpBroker();
    }
    
    /** 
     * JWizardPane that the reciever is providing the look and feel for.
     */
    protected JWizardPane wizardPane;
    /** The step pane. */
    protected Component stepPanel = null;
    /** The title pane. */
    protected JComponent titlePanel = null;
    /** The left options. */
    protected JComponent leftOptions = null; 
    /** The right options. */
    protected JComponent rightOptions = null; 
    /** The miminum size. */
    protected Dimension minimumSize;
    /** The property change listener. */
    protected PropertyChangeListener propertyChangeListener;

    /** Creates a new BasicWizardPaneUI instance.
     * @param x a JComponent for creating UI.
     * @return the BasicWizardPaneUI object.
     */
    public static ComponentUI createUI(JComponent x) {
	return new BasicWizardPaneUI();
    }

    /** Installs the reciever as the L&F for the passed in JWizardPane
     * @param c a JComponent for installing UI.
     */
    public void installUI(JComponent c) {
	wizardPane = (JWizardPane) c;
        //installDefaults();
        wizardPane.setLayout(createLayoutManager());
	installComponents();
        installListeners(); 
        //installKeyboardActions();
        if(DEBUG)
	    System.out.println("BasicWizardPaneUI-installUI");
    }

    /** Removes the receiver from the L&F controller of the passed in split
     * pane.
     * @param c a JComponent for uninstalling UI.
     */
    public void uninstallUI(JComponent c) {
        uninstallComponents();
        wizardPane.setLayout(null);
	//uninstallKeyboardActions();
        uninstallListeners();
        uninstallDefaults();
	wizardPane = null;
    }

    /** Install defaults. */
    protected void installDefaults() {
        LookAndFeel.installColorsAndFont(wizardPane, "OptionPane.background","OptionPane.foreground", "OptionPane.font");
	LookAndFeel.installBorder(wizardPane, "OptionPane.border");
        minimumSize = UIManager.getDimension("OptionPane.minimumSize");
	wizardPane.setOpaque(true);
    }
    
    /** Uninstall defaults. */
    protected void uninstallDefaults() {
	LookAndFeel.uninstallBorder(wizardPane);
    }

    /** Install components. */    
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
    
    /** Uninstall components. */
    protected void uninstallComponents() {
	wizardPane.removeAll();
    }

    /** Create layout manager.
     * @return a GridBagLayout.
     */
    protected LayoutManager createLayoutManager() {
	return new GridBagLayout();
    }

    /** Install listeners.
     */
    protected void installListeners() {
        if ((propertyChangeListener = createPropertyChangeListener()) != null) {
            wizardPane.addPropertyChangeListener(propertyChangeListener);
        }
    }

    /** Uninstall listeners.
     */
    protected void uninstallListeners() {
        if (propertyChangeListener != null) {
            wizardPane.removePropertyChangeListener(propertyChangeListener);
            propertyChangeListener = null;
        }
    }

    /** Create property change listeners.
     * @return a PropertyChangeHandler.
     */
    protected PropertyChangeListener createPropertyChangeListener() {
        return new PropertyChangeHandler();
    }

    /** Update step title.
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

    /** Create separator.
     * @return a JSeparator.
     */
    protected Container createSeparator() {
        return new JSeparator();
    }

    /** Add separator.
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

    /** Update step.
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

    /** add content.
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

    /** Update left options.
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


    /** Update right options
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



    /** Returns the minimum size the option pane should be. Primarily
     * provided for subclassers wishin to offer a different minimum size.
     * @return the minimum size the option pane should be.
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

    /** If c is the JWizardPane the reciever is contained in, the preferred
     * size that is returned is the maximum of the preferred size of
     * the LayoutManager for the JWizardPane, and
     * <code>getMinimumOptionPaneSize</code>.
     * @param c a JComponent to get preferred size.
     * @return the preferred size
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

    /** Messages getPreferredSize.
     * @param c a JComponent to get minimum size.
     * @return the minimum size.
     */
    public Dimension getMinimumSize(JComponent c) {
	return getPreferredSize(c);
    }

    /** Messages getPreferredSize.
     * @param c a JComponent to get maximum size.
     * @return the maximum size.
     */
    public Dimension getMaximumSize(JComponent c) {
	return getPreferredSize(c);
    }

    /** Select default button.
     * @param wp a JWizardPane object for the selection.
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
        /** If the source of the PropertyChangeEvent <code>e</code> equals the
         * wizardPane and is one of the ICON_PROPERTY, MESSAGE_PROPERTY,
         * OPTIONS_PROPERTY or INITIAL_VALUE_PROPERTY,
         * validateComponent is invoked.
         * @param e the PropertyChangeEvent.
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
