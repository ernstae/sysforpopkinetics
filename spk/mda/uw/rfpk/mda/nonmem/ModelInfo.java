/*
 * ModelInfo.java
 *
 * Created on December 8, 2003, 4:37 PM
 */

package uw.rfpk.mda.nonmem;

/**
 * This class defines an object that contains the information of a model
 * @author  jiaji Du
 */
public class ModelInfo {
    
    /** Creates a new instance of ModelInfo */
    public ModelInfo() {
    }
    
    /** name of the model */
    public String name = null;
    /** short description of the model */
    public String description = null;
    /** version code of the existing model */
    public String version = null;
    /** indication of the model being new or not */
    public boolean isNewModel = true;
    /** indication of the version being new or not */
    public boolean isNewVersion = true;
}
