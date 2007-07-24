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
package uw.rfpk.rcs;

import java.io.*;
import java.util.Vector;
import java.util.Calendar;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import javax.swing.JOptionPane;

/** This class wraps the rcs on Linux system to provide a revision control tool.
 *
 * @author  jiaji Du
 */
public class Archive {
    /** Create a new archive.
     * @param text text to put in the archive.
     * @param perlDir perl script directory path.
     * @param workingDir working directory path.
     * @param log log message of the version.
     * @param author author of the version.
     * @param filename filename of the temporary file.
     * @return the archive text.
     */
    public static String newArchive(String text, String perlDir, String workingDir, 
                                    String log, String author, String filename) 
        throws IOException, InterruptedException
    {
        return checkIn(null, text, perlDir, workingDir, log, author, filename);
    }
    
    /** Add a new version to the archive.
     * @param archive archive text.
     * @param text text to put in the archive.
     * @param perlDir perl script directory path.
     * @param workingDir working directory path.
     * @param log log message of the version.
     * @param author author of the version.
     * @param filename filename of the temporary file.
     * @return the archive text.
     */    
    public static String addRevision(String archive, String text, String perlDir, String workingDir,
                                     String log, String author, String filename)
        throws IOException, InterruptedException
    {
        checkOut(archive, perlDir, workingDir, filename, "0", true);
        return checkIn(archive, text, perlDir, workingDir, log, author, filename);
    }    
    
    /** Get a version text from the archive.
     * @param archive archive text.
     * @param perlDir perl script directory path.
     * @param workingDir working directory path.
     * @param filename filename of the temporary file.
     * @param version version to get except 0 for the last version.
     * @return the archive text.
     */
    public static String getRevision(String archive, String perlDir, String workingDir,
                                     String filename, String version)
        throws IOException, InterruptedException
    {
        return checkOut(archive, perlDir, workingDir, filename,  version, false);
    }
    
    /** Get the number of revisions.
     * @param archive archive text.
     * @return the number of revisions.
     */
    public static int getNumRevision(String archive)
    {
        return Integer.parseInt(archive.substring(archive.indexOf(".") + 1, archive.indexOf(";")));
    }
    
    /** Get the last revision date.
     * @param archive archive text.
     * @return the last revision date.
     */
    public static String getRevisionDate(String archive)
    {
        int i = archive.indexOf("date");
        return Calendar.getInstance().getTimeZone().getDisplayName(false, 0) + " " +
               archive.substring(i + 5, archive.indexOf(";", i)).trim();   
    }
    
    /** Get version list.
     * @param archive archive text.
     * @return the version list.
     */
    public static String[][] getVersionList(String archive)
    {
        String timeZone = Calendar.getInstance().getTimeZone().getDisplayName(false, 0) + " ";
        int n = getNumRevision(archive);
        int index = 0;
        String log;
        String[][] versionList = new String[n][4];
        for(int i = n; i > 0; i--)
        {
            versionList[i - 1][0] = String.valueOf(i);
            index = archive.indexOf("1." + i + "\ndate") + 9;            
            versionList[i - 1][2] = timeZone + archive.substring(index, archive.indexOf(";", index)).trim();
            index = archive.indexOf("author ", index) + 7;
            versionList[i - 1][1] = archive.substring(index, archive.indexOf(";", index)).trim();
        }
        for(int i = n; i > 0; i--)
        {
            index = archive.indexOf("@\n\n\n1." + i + "\nlog\n@", index);
            index = archive.indexOf("\nlog\n@", index) + 6;
            log = archive.substring(index, archive.indexOf("\n@", index));
            if(log.startsWith("@")) log = "None";
            versionList[i - 1][3] = log;
        }
        return versionList;
    }
    
    /** Get version log.
     * @param archive archive text.
     * @param version version specified.
     * @return version log
     */
    public static String getVersionLog(String archive, String version)
    {
        int index = archive.indexOf("@\n\n\n" + version + "\nlog\n@");
        index = archive.indexOf("\nlog\n@", index) + 6;
        String log = archive.substring(index, archive.indexOf("\n@", index));
        if(log.startsWith("@")) log = "None";
        return log;
    }
    
    private static String checkIn(String archive, String text, String perlDir, String workingDir,
                                  String log, String author, String filename)
        throws IOException, InterruptedException
    {
        Process process = null;        
        File archiveFile = null;
        String first = "0";
        if(archive == null)
            first = "1";
        File file = new File(workingDir + filename);
        saveFile(text, file);
        String[] command = {"perl", perlDir + "rcsci.pl", first, workingDir, log, author, filename};
        try
        {
            process = Runtime.getRuntime().exec(command);
            process.waitFor();
            archiveFile = new File(workingDir + filename + ",v");
            archive = openFile(archiveFile);
/*           
            String stdError = "";
            BufferedInputStream in = new BufferedInputStream(process.getErrorStream());
            while(true)
            {
                int i = in.read();
                if(i == -1)
                    break;
                stdError += (char)i;
            }
            in.close();
            if(stdError.length() != 0)
                JOptionPane.showMessageDialog(null, stdError);      
*/
        }
        finally
        {
            file.delete();
            if(archiveFile != null) archiveFile.delete();
            process.destroy();
        }
        return archive;
    }
    
    private static String checkOut(String archive, String perlDir, String workingDir,
                                   String filename, String version, boolean checkInLater)
        throws IOException, InterruptedException
    {
        String lock = checkInLater ? "-l" : "-u";
        Process process = null;
        String text = null;
        File file = null;
        File archiveFile = new File(workingDir + filename + ",v");
        saveFile(archive, archiveFile);
        String[] command = {"perl", perlDir + "/rcsco.pl", version, lock, workingDir, filename};
        try 
        {
            process = Runtime.getRuntime().exec(command);
            process.waitFor();
            file = new File(workingDir + filename);
            text = openFile(file).trim();
/*
            String stdError = "";
            BufferedInputStream in = new BufferedInputStream(process.getErrorStream());
            while(true)
            {
                int i = in.read();
                if(i == -1)
                    break;
                stdError += (char)i;
            }
            in.close();
            if(stdError.length() != 0)
                JOptionPane.showMessageDialog(null, stdError);
*/
        }
        finally
        {
            if(!checkInLater)
            {
                file.delete();
                archiveFile.delete();
            }
            process.destroy();
        }
        return text;
    }
    
    private static void saveFile(String text, File file)
    {
        FileOutputStream out = null;
        try
        {
            out = new FileOutputStream(file);
        }
        catch(FileNotFoundException e){}
        ByteBuffer buffer = ByteBuffer.allocate(text.length());
        FileChannel channel = out.getChannel();
        buffer.put(text.getBytes());
        buffer.flip();
        try
        {
            channel.write(buffer);
            out.close();
        }
        catch(IOException e )
        {
        }
    }
    
    private static String openFile(File file)
    {
        String text = null;
        try
	{
            StringBuffer buffer = new StringBuffer();
            BufferedReader in = new BufferedReader(new FileReader(file));
            String line;
            while((line = in.readLine()) != null)
                buffer.append(line).append("\n");
            in.close();
            text = buffer.toString();
        }
        catch(IOException e)
	{
        }
        return text;
    }
    
    /** Test code.
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        int i = 0;
        String text = "First version";
        String perlDir = "/usr/local/bin/";
        String workingDir = "/tmp/";
        String log = "log message 1";
        String author = "author-1";
        String filename = "my_file";
        try
        {
            String archive = newArchive(text, perlDir, workingDir, log, author, filename); 
            String version = "0";   // last version
            boolean checkInLater = true;
            text = checkOut(archive, perlDir, workingDir, filename, version, checkInLater);
            ok(text.equals("First version"), ++i, "newArchive and checkOut last version test");
            text += "\nSecond version";
            log = "log message 2";
            author = "author-2";
            archive = checkIn(archive, text, perlDir, workingDir, log, author, filename);
            version = "1.1";
            checkInLater = false;
            text = checkOut(archive, perlDir, workingDir, filename, version, checkInLater);
            ok(text.equals("First version"), ++i, "checkIn and checkOut version 1.1 test");
            version = "1.2";
            text = getRevision(archive, perlDir, workingDir, filename, version);
            ok(text.equals("First version\nSecond version"), ++i, "getRevision version 1.2 test");
            text += "\nThird version";
            log = "log message 3";
            author = "author-3";
            archive = addRevision(archive, text, perlDir, workingDir, log, author, filename);
            version = "1.3";
            text = getRevision(archive, perlDir, workingDir, filename, version);
            ok(text.equals("First version\nSecond version\nThird version"), ++i, "addRevision and getRevision test");
            int n = getNumRevision(archive);
            ok(n == 3, ++i, "getNumRevision test");
            String date = getRevisionDate(archive);
            System.out.println("The version " + version + " date = " + date + " , getRevisionDate test");
            String[][] versionList = getVersionList(archive);
            for(int j = 0; j < n; j++)
            {
                version = versionList[j][0];
                ok(version.equals(String.valueOf(j + 1)), ++i, "getVersionList test version " + (j + 1));
                author = versionList[j][1];
                ok(author.equals("author-" + String.valueOf(j + 1)), ++i, "getVersionList test version " + (j + 1));
                date = versionList[j][2];
                System.out.println("The version " + (j + 1) + " date = " + date + " , getVersionList test");
                log = versionList[j][3];
                ok(log.equals("log message " + (j + 1)), ++i,"getVersionList test version " + (j + 1)); 
            }
            log = getVersionLog(archive, "1.3");
            ok(log.equals("log message 3"), ++i, "getVersionLog test");
        }
        catch(IOException e){JOptionPane.showMessageDialog(null, e);}
        catch(InterruptedException e){JOptionPane.showMessageDialog(null, e);}
        System.out.println(okTests + " tests were ok out of a possible " + i);
    }
    
    private static void ok(boolean b, int i, String m)
    {
	String s = b ? "ok:\t" : "not ok:\t";
	s += i + " - " + m;
	System.out.println(s);
	if (b)
	    okTests++;
    }
    static int okTests = 0;
}
