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

import javax.swing.JOptionPane;
import java.util.ResourceBundle;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JComponent;
import java.util.Vector;
import javax.swing.SwingConstants;
import javax.swing.plaf.OptionPaneUI;
import org.netbeans.ui.wizard.plaf.WizardPaneUI;
import org.netbeans.ui.wizard.plaf.basic.BasicWizardPaneUI;

import javax.swing.JDialog;
import java.awt.Component;
import java.awt.Window;
import java.awt.Container;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.awt.Frame;
import java.awt.Dialog;
import javax.swing.SwingUtilities;
import java.util.NoSuchElementException;
import javax.swing.event.ChangeListener;
import java.awt.List;
import javax.swing.event.ChangeEvent;
import java.awt.Image;

public class JWizardPane extends JComponent {

    private static final boolean DEBUG = false;

    public final static int SELECT_BY_FONT = 0, SELECT_BY_COLOR = 1;

    private StepIterator iterator;
    private Object customizedObject;
    private ChangeListener cl;
    private PropertyChangeListener pcl;
    private JWizardPane wizard = this;
    private Component parent;

    /** Bounds property name for value. */
    public static final String VALUE_PROPERTY = "value";
    /** Bounds property name for defaultButton. */
    public static final String DEFAULT_BUTTON_PROPERTY = "defaultButton";
    /** Bound property name for */
    public static final String LEFT_OPTIONS_PROPERTY = "leftOptions";
    /** Bound property name for */
    public static final String RIGHT_OPTIONS_PROPERTY = "rightOptions";
    //** Bound property name for */
    public static final String CONTENT_PANEL_PROPERTY = "contentPanel";
    //** Bound property name for */
    public static final String STEP_PANEL_PROPERTY = "stepPanel";
    //** Bound property name for current step */
    public static final String CURRENT_STEP_PROPERTY = "currentStep";
    /** Bound property name for step title text*/
    public static final String STEP_TITLE_TEXT_PROPERTY = "stepTitleText";
    /** Bound property name for step title color*/
    public static final String STEP_TITLE_COLOR_PROPERTY = "stepTitleColor";
 

    /** */
    transient protected WizardStep currentStep;
    /** */
    transient protected Object[] leftOptions;
    /** */
    transient protected Object[] rightOptions;
    /** */
    transient protected ContentPanel contentPanel;
    /** */
    transient protected Component stepPanel = null;
    /** */
    transient protected Object value;
    /** */
    transient protected Object defaultButton;
    /** */
    transient protected int contentWidth;
    /** */
    transient protected Color stepTitleColor;
    /** */
    transient protected String stepTitleText;


    /** "Previous" button option.*/
    public static final int  PREVIOUS_OPTION = 1;
    public static final int  PREVIOUS_OPTION_DISABLED = 2;
    /** "Next" button option.*/
    public static final int  NEXT_OPTION = 3;
    public static final int  NEXT_OPTION_DISABLED = 4;
    /** "Finish" button option.*/
    public static final int  FINISH_OPTION = 5;
    public static final int  FINISH_OPTION_DISABLED = 6;
    /** "Last" button option.*/
    public static final int  LAST_OPTION = 7;
    public static final int  LAST_OPTION_DISABLED = 8;
    /** "Cancel" button option.*/
    public static final int  CANCEL_OPTION = 9;
    /** Windows is closed by user*/
    public static final int  CLOSED_OPTION = 10;
    /** "Help" button option */
    public static final int  HELP_OPTION = 11;


   /** Create wizard for a sequence of panels, with settings
    * defaulted to <CODE>this</CODE>.
    *
    * @param panels iterator over all {@link WizardStep}s that can appear in the wizard
    */
    public JWizardPane(StepIterator iterator){
	this(iterator, null);
    }

    /** Create wizard for a sequence of panels, passing some settings to the panels.
    * @param iterator the iterator over all {@link WizardStep}s that can appear in the wizard
    * @param setter the settings to provide to the panels (may be any data understood by them)
    * @see WizardDescriptor.Panel#readSettings
    * @see WizardDescriptor.Panel#storeSettings
    */
    public JWizardPane(StepIterator iterator, Object setter){

	this.iterator = iterator;
	this.customizedObject = setter;

  	this.setUI( BasicWizardPaneUI.createUI(this));

  	setContentPanel(new ContentPanel());

	cl = new ChangeListener(){
		public void stateChanged(ChangeEvent e){
		    if (DEBUG) {
			System.out.println("JWizardPane-ChangeEvent is here.");
		    } // end of if ()
		    
		    updateState();
		}
	    };

	pcl = new PropertyChangeListener(){
		public void propertyChange(PropertyChangeEvent e){

		    if (DEBUG) {
			System.out.println("JWizardPane-PropertyChangeEvent is here.");
		    } // end of if ()

		    if (e.getPropertyName().equals(VALUE_PROPERTY)) {
			if (e.getNewValue()instanceof Integer) {
			    int val = ((Integer)e.getNewValue()).intValue();

			    System.out.println(val);
			    System.out.println("----");

			    switch (val) {

			    case JWizardPane.PREVIOUS_OPTION: 
				BackAction();
				break;
			    case  JWizardPane.NEXT_OPTION:
				NextAction();
				break;
			    case  JWizardPane.LAST_OPTION:
				LastAction();
				break;
			    case  JWizardPane.FINISH_OPTION:
				FinishAction();
				break;
			    case  JWizardPane.HELP_OPTION:                                
				break;
			    case  JWizardPane.CANCEL_OPTION:
				CancelAction();
				break;
			    } // end of switch ()
			}
		    }
		    
		    if (e.getPropertyName().equals(CURRENT_STEP_PROPERTY)) {

			WizardStep ows = (WizardStep) e.getOldValue();
			if (ows != null) {
			    ows.getStepDescription().removeChangeListener(cl);
                            ows.getStepDescription().hidingStep(wizard);
			}

			WizardStep nws = (WizardStep) e.getNewValue();
			nws.getStepDescription().addChangeListener(cl);
//			nws.getStepDescription().showingStep(wizard); 
		    } 
		}
	    };
	    
	this.addPropertyChangeListener(pcl);

	iterator.reset();
	updateState();

  	if (DEBUG) {
	    System.out.println("JWizardPane-JWizardPane()");
	} // end of if ()
    }


    //*********************************************************************************************************

    /** Action called when Back button is pressed
     *
     * @see NextAction()
     * @see LastAction() 
     * @see CancelAction()
     * @see FinishAction()
     */
    public void BackAction(){

  	if (DEBUG) {
	    System.out.println("BACK is pressed.");
	} // end of if ()
        iterator.setIsBack(true);
	setValue(new Integer(PREVIOUS_OPTION));
	iterator.previousStep();
	updateState();
    }

    /** Action called when Next button is pressed
     *
     * @see BackAction()
     * @see LastAction() 
     * @see CancelAction()
     * @see FinishAction() 
     */
    public void NextAction(){

  	if (DEBUG) {
	    System.out.println("NEXT is pressed.");
	} // end of if ()
        iterator.setIsBack(false);
	setValue(new Integer(NEXT_OPTION));
        iterator.setSteps();
	iterator.nextStep();
	updateState();
    }

    /** Action called when Lst button is pressed
     *
     * @see NextAction()
     * @see BackAction()
     * @see CancelAction()
     * @see FinishAction()
     */
    public void LastAction(){

  	if (DEBUG) {
	    System.out.println("LAST is pressed.");
	} // end of 
        iterator.setIsBack(false);
	setValue(new Integer(LAST_OPTION));
	iterator.lastStep();
	updateState();
    }

    /** Action called when Finish button is pressed
     *
     * @see NextAction()
     * @see BackAction()
     * @see LastAction() 
     * @see CancelAction()
     */
    public void FinishAction(){

  	if (DEBUG) {
	    System.out.println("FINISH is pressed.");
	} // end of if ()

	setValue(new Integer(FINISH_OPTION));
    }
    
    /** Action called when Cancel button is pressed
     *
     * @see NextAction()
     * @see BackAction()
     * @see LastAction() 
     * @see FinishAction()
     */
    public void CancelAction(){

  	if (DEBUG) {
	    System.out.println("CANCEL is pressed.");
	} // end of if ()

	setValue(new Integer(CANCEL_OPTION));
    }

    private void updateContent(){

	// update items
	// build actual list of steps
	List items = new List();
	while (iterator.hasPrevious()) iterator.previousStep(); 
	while (iterator.hasNext()) {
	    items.add(iterator.getCurrent().getStepDescription().getContentItem());
	    iterator.nextStep();
	}

	//....
	items.add(iterator.getCurrent().getStepDescription().getContentItem());

	while (iterator.hasPrevious() && (iterator.getCurrent() != this.currentStep)) iterator.previousStep(); 
	String[] c = items.getItems();
	// ----

	contentPanel.setContentItems(c);
	// update selected item
	String s = iterator.getCurrent().getStepDescription().getContentItem();
	for (int i = 0; i < c.length; i++) {
	    if (s.equals(c[i])) {
		contentPanel.setSelectedIndex(i);
		break;
	    }
	}
    }

    private void updateButtons(){
	setLeftOptions(getUpdatedLeftOptions().toArray());
	setRightOptions(getUpdatedRightOptions().toArray());
    }

    /**
     *
     */
    public Vector getUpdatedLeftOptions(){

	Vector lb = new Vector();
	int b;

	// BACK button
       	b = iterator.hasPrevious() ? PREVIOUS_OPTION : PREVIOUS_OPTION_DISABLED;
	lb.add( new Integer(b));

	// NEXT button
	if ((iterator.hasNext() && currentStep.getStepDescription().isValid())) {
	    b = NEXT_OPTION;
	    defaultButton = new Integer(NEXT_OPTION);
	} else {
	    b = NEXT_OPTION_DISABLED;
	} 
	lb.add( new Integer(b));	
	
	// LAST or FINISH button
       	if (iterator.canFinish()) {
		if (iterator.hasNext()){
		    b = (currentStep.getStepDescription().isValid()) ? LAST_OPTION: LAST_OPTION_DISABLED;
		}
		else { 
		    b = (currentStep.getStepDescription().isValid()) ? FINISH_OPTION : FINISH_OPTION_DISABLED;
		    if (b == FINISH_OPTION) defaultButton = new Integer(FINISH_OPTION);
		}
	    } 
	lb.add( new Integer(b));

	return lb;
    }

    /**
     *
     */
    public Vector getUpdatedRightOptions(){

	Vector rb = new Vector();
	int b;

	// CANCEL button
	b = CANCEL_OPTION;
	rb.add( new Integer(b));

	// HELP button
	if (currentStep.getStepDescription().getHelpAction() instanceof ActionListener) {
	    b = HELP_OPTION;
	    rb.add( new Integer(b));
	}
	
	return rb;
    }

    private synchronized void updateState(){

	setCurrentStep(iterator.getCurrent());

	setStepPanel( currentStep.getStepDescription().getComponent());
	setStepTitleText( currentStep.getStepDescription().getStepTitle());
	updateContent();
      	updateButtons();
//	selectDefaultButton();   
        currentStep.getStepDescription().showingStep(wizard);
    }

    /** Return object which is customized by this wizard
     */
    public Object getCustomizedObject(){
	return customizedObject;
    }

    public void setCurrentStep(WizardStep ws){
	if (currentStep != ws) {
	    WizardStep ows = currentStep;
	    currentStep = ws;
	    firePropertyChange(CURRENT_STEP_PROPERTY, ows, ws);
	} 
    }

    public WizardStep getCurrentStep(){
	return currentStep;
    }

    public void setLeftOptions(Object[] options){
        Object[] oldOptions  = leftOptions;

        leftOptions = options;
        firePropertyChange(LEFT_OPTIONS_PROPERTY, oldOptions, leftOptions);
    }

    public Object[] getLeftOptions(){
	return leftOptions;
    }

    public void setRightOptions(Object[] options){
        Object[] oldOptions  = rightOptions;

        rightOptions = options;
        firePropertyChange(RIGHT_OPTIONS_PROPERTY, oldOptions, rightOptions);
    }

    public Object[] getRightOptions(){
	return rightOptions;
    }

    public void setContentPanel(JComponent content){
        JComponent oldPanel  = contentPanel;

        contentPanel = (ContentPanel) content;
        firePropertyChange(CONTENT_PANEL_PROPERTY, oldPanel, contentPanel);
    }

    public JComponent getContentPanel(){
	return contentPanel;
    }

    public void setStepPanel(Component step){
        Component oldStep  = stepPanel;

        stepPanel = step;
        firePropertyChange(STEP_PANEL_PROPERTY, oldStep, stepPanel);
    }

    public Component getStepPanel(){
	return stepPanel;
    }

    /** Setter for step title text*/
    public void setStepTitleText(String title){
	String  oldTitle  = stepTitleText;
	stepTitleText = title;
	firePropertyChange(STEP_TITLE_TEXT_PROPERTY, oldTitle, stepTitleText);
    }

    /** Getter for step title text */
    public String getStepTitleText(){
	return stepTitleText;
    }

    /** Setter for step title color */
    public void setStepTitleColor(Color c){
	Color old = stepTitleColor;
	stepTitleColor = c;
       	firePropertyChange(STEP_TITLE_COLOR_PROPERTY, old, stepTitleColor);
    }

    /** Getter for step title color */
    public Color getStepTitleColor(){
	//return contentPanel.getTitleColor();
	return stepTitleColor;
    }

    /** Getter for value*/
    public Object getValue(){
	return value;
    }

    /** Setter for value*/
    public void setValue(Object newValue){
        Object oldValue = value;
        value = newValue;
        firePropertyChange(VALUE_PROPERTY, oldValue, value);
    }

    /** Setter for contnts image*/
    public void setContentImage(Image im){
	contentPanel.setImage(im);
    }

    /** Getter for contnts image*/
    public Image getContentImage(){
	return contentPanel.getImage();
    }

    /** Getter for content image alignment*/
    public int getContentImageAlignment(){
	return contentPanel.getImageAlignment();
    }

    /** Setter for content image alignment*/
    public void setContentImageAlignment(int al){
	contentPanel.setImageAlignment(al);
    }

    /** Getter for content image visilility*/
    public boolean getContentVisibleImage(){
	return contentPanel.getVisibleImage();
    }

    /** Setter for content image visilility*/
    public void setContentVisibleImage(boolean vi){
	contentPanel.setVisibleImage(vi);
    }


    public void setContentTitleColor(Color c){
	contentPanel.setTitleColor(c);
    }

    public void setContentForeground(Color c){
	contentPanel.setForeground(c);
    }

    public void setContentSelectionType(int val){
	contentPanel.setSelectMetod(val);
    }

    public void setContentSelectionColor(Color c){
	contentPanel.setSelectedColor(c);
    }

    public void setContentNumbered(boolean val){
	contentPanel.setContentNumbered(val);
    }





    public void setDefaultButton(Object newValue){
        Object oldValue = defaultButton;
        defaultButton = newValue;
	 firePropertyChange(DEFAULT_BUTTON_PROPERTY, oldValue, defaultButton);
    }

    public Object getDefaultButton(){
	return defaultButton;
    }

    /**
     * Requests that the initial value be selected, which will set
     * focus to the initial value. This method
     * should be invoked after the window containing the option pane
     * is made visible.
     */
    public void selectDefaultButton() {
	WizardPaneUI ui = getUI();
	if (ui != null) {
	    ui.selectDefaultButton(this);
	}
    }

    /**
     * Sets the UI object which implements the L&F for this component.
     *
     * @param ui  the <code>OptionPaneUI</code> L&F object
     * @see UIDefaults#getUI
     * @beaninfo
     *       bound: true
     *      hidden: true
     * description: The UI object that implements the optionpane's LookAndFeel
     */
    public void setUI(WizardPaneUI ui) {
        if ((WizardPaneUI)this.ui != ui) {
            super.setUI(ui);
            invalidate();
        }
    }

    /**
     * Returns the UI object which implements the L&F for this component.
     *
     * @return the <code>WizardPaneUI</code> object
     */
    public WizardPaneUI getUI() {
        return (WizardPaneUI)ui;
    }

    /**
     * Returns the name of the UI class that implements the
     * L&F for this component.
     *
     * @return the string "OptionPaneUI"
     * @see JComponent#getUIClassID
     * @see UIDefaults#getUI
     */
    public String getUIClassID() {
        return "WizardPaneUI"; //uiClassID;
    }

    public JDialog createDialog(Component parentComponent, String title) {
        parent = parentComponent; 
        final JDialog dialog;

	//******
        Window window = getWindowForComponent( parentComponent);
	//= JOptionPane.getFrameForComponent(parentComponent);
	

        if (window instanceof Frame) {
            dialog = new JDialog((Frame)window, title, false);	
        } else {
            dialog = new JDialog((Dialog)window, title, true);
        }		

        Container contentPane = dialog.getContentPane();

        contentPane.setLayout(new BorderLayout());
        contentPane.add(this, BorderLayout.CENTER);
        dialog.pack();
        dialog.setLocationRelativeTo(parentComponent);
        dialog.addWindowListener(new WindowAdapter() {

		public void windowClosing(WindowEvent we) {
		    setValue(new Integer(CLOSED_OPTION));
                    customizedObject = null;
                    ((uw.rfpk.mda.nonmem.MDAFrame)parent).enablePrepareInput();
		}

		public void windowActivated(WindowEvent we) {

		    if (DEBUG) {
			System.out.println("windowActivated()");			
		    } // end of if ()
		    
//		    selectDefaultButton();
		}
	    });

	addPropertyChangeListener(new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent event) {

		    if(dialog.isVisible() && event.getSource() == JWizardPane.this &&
		       (event.getPropertyName().equals(VALUE_PROPERTY))){

			JWizardPane p = (JWizardPane) event.getSource();			
			if ((p.getValue().equals(new Integer(JWizardPane.CANCEL_OPTION)) || 
			    (p.getValue().equals(new Integer(JWizardPane.FINISH_OPTION))))){
			    dialog.setVisible(false);
			    dialog.dispose();
			} 
                        if (p.getValue().equals(new Integer(JWizardPane.CANCEL_OPTION)))
                        {
                            customizedObject = null; 
                            ((uw.rfpk.mda.nonmem.MDAFrame)parent).enablePrepareInput();
                        }
                        if (p.getValue().equals(new Integer(JWizardPane.FINISH_OPTION))) 
                            ((uw.rfpk.mda.nonmem.MDAFrame)parent).processInput(); 
		    }
		}
	    });
	 return dialog;
    }

    static Window getWindowForComponent(Component parentComponent) {
        if (parentComponent == null) 
	    return JOptionPane.getFrameForComponent(parentComponent);
	//return getRootFrame();

        if (parentComponent instanceof Frame || parentComponent instanceof Dialog)
            return (Window)parentComponent;
        return JWizardPane.getWindowForComponent(parentComponent.getParent());
    }
}

