/*
 * ModelInfo.java
 *
 * Created on December 8, 2003, 4:37 PM
 */

package uw.rfpk.mda.nonmem;

/**
 * This class defines an object that contains the information of an archive
 * @author  jiaji Du
 */
public class ArchiveInfo {
    
    /** Creates a new instance of Archive */
    public ArchiveInfo() {
    }
    
    /** id of the archive */
    public long id = 0L;    
    /** name of the archive */
    public String name = null;
    /** short description of the archive */
    public String description = null;
    /** version code of the existing archive */
    public String version = null;
    /** log message for the version */
    public String log = null;
    /** indication of the archive being new or not */
    public boolean isNewArchive = true;
    /** indication of the version being new or not */
    public boolean isNewVersion = true;
}
