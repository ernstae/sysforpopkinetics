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

import java.awt.Component;
import java.net.URL;
import java.awt.event.ActionListener;
import java.awt.Image;
import javax.swing.event.ChangeListener;
import java.lang.IllegalStateException;
import javax.swing.event.ChangeEvent;

public abstract class StepDescriptor {

    /** Add a listener to changes of the panel's validity.
     */
    private ChangeListener listener;

    /** Get the component displayed in this panel.
     */
    public abstract Component getComponent();

    /** Title displayed for this step.
     */
    public abstract String getStepTitle();

    /** String which is displayed in content.
     */
    public abstract String getContentItem();

    /** Provides the wizard panel with the current data--either the default
     *  data or already-modified settings, if the user used the previous 
     *  and/or next buttons. 
     */
    public abstract void showingStep(JWizardPane wizard);

    /** Provides the wizard panel with the opportunity to update 
     *  the settings with its current customized state.
     */
    public abstract void hidingStep(JWizardPane wizard);

    /** Test whether the panel is finished and it is safe to proceed to the next one.
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
     */
    public void addChangeListener(ChangeListener l){
	if (listener != null) throw new IllegalStateException();
	listener = l;
    }

    /**  Remove a listener to changes of the panel's validity.
     */
    public void removeChangeListener(ChangeListener l){
	listener = null;
    }

    /** Help action for this panel.
     *  If not null help button is displayed. 
     */
    public ActionListener getHelpAction(){
	return null;
    }
}
