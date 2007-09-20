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

import java.awt.Component;
import java.net.URL;
import java.awt.event.ActionListener;
import java.awt.Image;
import javax.swing.event.ChangeListener;
import java.lang.IllegalStateException;
import javax.swing.event.ChangeEvent;

/** This class defines step descriptor for the wizard.
 */
public abstract class StepDescriptor {

    /** The listener to changes of the panel's validity.
     */
    private ChangeListener listener;

    /** Get the component displayed in this panel.
     * @return the component displayed in this panel.
     */
    public abstract Component getComponent();

    /** Get the title displayed for this step.
     * @return the title displayed for this step.
     */
    public abstract String getStepTitle();

    /** Get String which is displayed in content.
     * @return the string which is displayed in content.
     */
    public abstract String getContentItem();

    /** Provides the wizard panel with the current data--either the default
     *  data or already-modified settings, if the user used the previous
     *  and/or next buttons.
     * @param wizard a JWizardPane object provided to the step.
     */
    public abstract void showingStep(JWizardPane wizard);

    /** Provides the wizard panel with the opportunity to update
     *  the settings with its current customized state.
     * @param wizard a JWizardPane object provided to the step.
     */
    public abstract boolean checkingStep(JWizardPane wizard);
    
    /** Provides the wizard panel with the opportunity to check the correctness of
     *  its current customized state.
     * @param wizard a JWizardPane object provided to the step.
     */
    public abstract void hidingStep(JWizardPane wizard);

    /** Test whether the panel is finished and it is safe to proceed to the next one.
     * @return ture if it is valid to proceed, false otherwisw.
     */
    public abstract boolean isValid();
    
    /** Fired change for wizards update.
     */
    public void fireStateChanged(){
	if (listener != null) {
	    listener.stateChanged(new ChangeEvent(this));
	} 
    }

    /** Add a listener to changes of the panel's validity.
     * @param l a ChangeListener to add.
     */
    public void addChangeListener(ChangeListener l){
	if (listener != null) throw new IllegalStateException();
	listener = l;
    }

    /** Remove a listener to changes of the panel's validity.
     * @param l a ChangeListener to remove.
     */
    public void removeChangeListener(ChangeListener l){
	listener = null;
    }

    /** Help action for this panel.
     *  If not null help button is displayed.
     * @return null.
     */
    public ActionListener getHelpAction(){
	return null;
    }
    
    /** Help ID for this panel.
     * If not null help ID is displayed.
     * @return null.
     */
    public String getHelpID(){
        return null;
    }
}
