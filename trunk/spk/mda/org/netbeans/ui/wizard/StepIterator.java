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

import java.lang.String;
import javax.swing.event.ChangeListener;

public interface StepIterator {

    /** Set steps */
    public void setSteps();

    /** Set if the back button was clicked.
     * @param b A boolean, true if the back button was clicked, false for otherwise.
     */
    public void setIsBack(boolean b);
    
    /** Initialize it. Curently move it to first WizardStep */
    public void reset();

    /** Get the current WizardStep.
     * @return the WizardStep
     */
    public WizardStep getCurrent();

    /** Moves to the final confirmation page 
     */
    public void lastStep();

    /** Can properly finish wizard run. It mean all setting are valid.
     * @return <code>true</code> if so
     */
    public boolean canFinish();

    /** Test whether there is a next panel.
     * @return <code>true</code> if so
     */
    public boolean hasNext();

    /** Test whether there is a previous panel.
     * @return <code>true</code> if so
     */
    public boolean hasPrevious();

    /** Move to the next panel.
     * I.e. increment its index, need not actually change any GUI itself.
     * @exception NoSuchElementException if the panel does not exist
     */
    public void nextStep();

    /** Move to the previous panel.
     * I.e. decrement its index, need not actually change any GUI itself.
     * @exception NoSuchElementException if the panel does not exist
     */
    public void previousStep();

    /** Add a listener to changes of the current panel.
     * The listener is notified when the possibility to move forward/backward changes.
     * @param l the listener to add
     */
    ////public void addChangeListener(ChangeListener l);

    /** Remove a listener to changes of the current panel.
     * @param l the listener to remove
     */
    ////public void removeChangeListener(ChangeListener l);
}
