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

import java.lang.String;
import javax.swing.event.ChangeListener;

/** This interface declares step iterator.
 */
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
     */
    public void nextStep();

    /** Move to the previous panel.
     * I.e. decrement its index, need not actually change any GUI itself.
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
