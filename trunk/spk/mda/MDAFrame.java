package uw.rfpk.mda.nonmem;

import javax.swing.*;
import javax.swing.text.DefaultEditorKit;  
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Properties;
import java.awt.print.*;
import java.awt.font.*;
import java.security.*;
import javax.crypto.*;
import org.netbeans.ui.wizard.*;
import uw.rfpk.mda.nonmem.wizard.*; 

/**
 * This class creates a window that includes a menu bar and a text area.  The menu bar has two
 * menus, File menu and Operation menu.  The File menu includes Open, Close, Save, Save As,
 * Page Setup and Print menu items.  The Operation menu includes Transmit and Receive menu items.
 * @author Jiaji Du
 * @version 1.0 
 */                                 
public class MDAFrame extends JFrame
{
    /** This is a constructor creating the applications main window.
     * @param args A String array containing the session ID and secret number
     * @param title a String object as the title of the window.
     */
    public MDAFrame(String title, String[] args)
    {
        if(args.length != 0)
      	{
            sessionId = args[0];                      // Set the session ID

            // Generate the secret key of the session
            try
	    {
                byte[] bytes = new byte[16];
                for(int i = 0; i <16; i++)
	        {
                    bytes[i] = 0x0;
                    int m = 0x80;
                    for(int j = 0; j < 8; j++)
		    {
                        if(args[1].charAt(i * 8 + j) == '1')
                            bytes[i] = (byte)(bytes[i] | m);
                         m = m >> 1; 
                    }
                }

                KeyGenerator keygen = KeyGenerator.getInstance("Blowfish", "SunJCE");
                SecureRandom random = new SecureRandom(bytes);
                keygen.init(128, random);
                key = keygen.generateKey();
            }
            catch(GeneralSecurityException gse)
	    {
                System.err.println(gse);
                JOptionPane.showMessageDialog(null, "Error generating key",  // Display generating key 
                                                      "Security Error",      // error message
                                                      JOptionPane.ERROR_MESSAGE);	        
            }   
        }
        setTitle(title);                              // Set the window title
        setJMenuBar(menuBar);                         // Add the menu bar to the window

        JMenu fileMenu = new JMenu("File");           // Create File menu
        JMenu editMenu = new JMenu("Edit");           // Cretae Edit menu
        JMenu operationMenu = new JMenu("Operation"); // Create Operation menu

        // Create action items for the file menu
        openAction = new FileAction("Open", KeyStroke.getKeyStroke('O',Event.CTRL_MASK ));
        closeAction = new FileAction("Close");
        saveAction = new FileAction("Save", KeyStroke.getKeyStroke('S',Event.CTRL_MASK ));
        saveAsAction = new FileAction("Save As...");
        pageSetupAction = new FileAction("PageSetup");
        printAction = new FileAction("Print", KeyStroke.getKeyStroke('P',Event.CTRL_MASK ));
        exitAction = new FileAction("Exit", KeyStroke.getKeyStroke('E',Event.CTRL_MASK ));

        // Create action items for the operation menu
        createAction = new OperationAction("Create Input File", KeyStroke.getKeyStroke('C',Event.CTRL_MASK ));
        transmitAction = new OperationAction("Transmit File", KeyStroke.getKeyStroke('T',Event.CTRL_MASK ));
        receiveAction = new OperationAction("Receive File", KeyStroke.getKeyStroke('R',Event.CTRL_MASK ));

        // Construct the file pull down menu
        addMenuItem(fileMenu, openAction);
        addMenuItem(fileMenu, closeAction);
        fileMenu.addSeparator();
        addMenuItem(fileMenu, saveAction);
        addMenuItem(fileMenu, saveAsAction);
        fileMenu.addSeparator();
        addMenuItem(fileMenu, pageSetupAction);
        addMenuItem(fileMenu, printAction);
        fileMenu.addSeparator();
        addMenuItem(fileMenu, exitAction);

        menuBar.add(fileMenu);                        // Add the file menu
        menuBar.add(editMenu);
        menuBar.add(operationMenu);                   // Add the operation menu
        enableEvents(AWTEvent.WINDOW_EVENT_MASK);     // Enable window events

        // Add the test area
        textArea.setFont(textFont);
        JScrollPane scrollPane = new JScrollPane(textArea);
        getContentPane().add(scrollPane);
//        if(args.length != 0)
//            textArea.setText(sessionId + "\n" + args[1]);

        // Construct the operation pull down menu
        addMenuItem(operationMenu, createAction);
        addMenuItem(operationMenu, transmitAction);
        addMenuItem(operationMenu, receiveAction);
        
        JMenuItem cut = new JMenuItem("Cut");
        JMenuItem copy = new JMenuItem("Copy");
        JMenuItem paste = new JMenuItem("Paste");        
        cut.addActionListener(new DefaultEditorKit.CutAction());  
        copy.addActionListener(new DefaultEditorKit.CopyAction());  
        paste.addActionListener(new DefaultEditorKit.PasteAction()); 
        cut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U, ActionEvent.ALT_MASK));
        copy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.ALT_MASK));
        paste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.ALT_MASK));        
        editMenu.add(cut);
        editMenu.add(copy);
        editMenu.add(paste);
        
        if(sessionId == null)
	{
            transmitAction.setEnabled(false);
            receiveAction.setEnabled(false);
        }
    }

    /**
     * Handles window events.
     * @param e the WindowEvent object to handle.
     */
    protected void processWindowEvent(WindowEvent e) 
    {
        if (e.getID() == WindowEvent.WINDOW_CLOSING)
        {
            dispose();                    // Release resources
            System.exit(0);               // Exit the program
        }
        super.processWindowEvent(e);      // Pass on the event
    }
    
    // Helper method to add menu items.
    private JMenuItem addMenuItem(JMenu menu, Action action)
    {
        JMenuItem item = menu.add(action);           // Add the menu item
      
        KeyStroke keyStroke = (KeyStroke)action.getValue(action.ACCELERATOR_KEY);
        if(keyStroke != null )
	    item.setAccelerator(keyStroke);
        return item;
    }  

    // FileAction inner class
    class FileAction extends AbstractAction
    {
        // Constructor with one argument.
        FileAction(String name)
        { 
            super(name);
        }

        // Constructor with two arguments.
        FileAction(String name, KeyStroke keystroke)
        {
            this(name);
            if(keystroke != null )
                putValue(ACCELERATOR_KEY, keystroke);
        }
 
        // Event handler for FileAction.
        public void actionPerformed(ActionEvent e)
        {
            String name = (String)getValue(NAME);

            if(name.equals(saveAsAction.getValue(NAME)))     // Handle Save As
	    {
                int result = files.showSaveDialog(null);
                if(result == files.APPROVE_OPTION)
		{
                    file = files.getSelectedFile();
                    saveOperation();
                }
	    }
            else if(name.equals(saveAction.getValue(NAME)))  // Handle Save
	    {
		if(file != null)
		{
                    saveOperation();
		}
                else
		{
                    int result = files.showSaveDialog(null);
                    if(result == files.APPROVE_OPTION)
		    {
                        file = files.getSelectedFile();
                        saveOperation();
                    }
                }
	    }
            else if(name.equals(openAction.getValue(NAME)))  // Handle Open
	    {
                int result = files.showOpenDialog(null);
                if(result == files.APPROVE_OPTION)
		{
                    file = files.getSelectedFile();
                    try
	            {
                        BufferedReader in = new BufferedReader(new FileReader(file));
                        StringBuffer buffer = new StringBuffer();
                        boolean done = false;
                        while(!done)
                        {
                            // Read a line
                            String line = in.readLine();                            
                            if(line == null) 
                                done = true;
                            else
                                buffer.append(line).append("\n");
		        }
                        textArea.setText(buffer.toString());
                        in.close();
                    }
                    catch(IOException ioe )
		    {
                        System.err.println(ioe);
                        JOptionPane.showMessageDialog(null, "Error opening file",  // Display opening file 
                                                      "File Error",                // error message
                                                      JOptionPane.ERROR_MESSAGE);
                    }
		}
	    }
            else if(name.equals(closeAction.getValue(NAME)))  // Handle Close
	    {
                textArea.setText("");
                file = null;
	    }
            else if(name.equals(pageSetupAction.getValue(NAME)))// Handle Page Setup
	    {
                Printable printer = new Printer();

                // Get a PrinterJob object
                PrinterJob printJob = PrinterJob.getPrinterJob();

                // Get default PageFormat object
                PageFormat pageFormat = printJob.defaultPage();

                // Show page setup dialog
                pageFormat = printJob.pageDialog(pageFormat);
               
                // Print using the user page format settings
                printJob.setPrintable(printer, pageFormat);

                try
		{
                    printJob.print();
                }
                catch(PrinterException pe)
		{
                    System.out.println(pe);
                    JOptionPane.showMessageDialog(null, "Error printing",     // Display printing
                                                  "Printer Error",            // error message
                                                  JOptionPane.ERROR_MESSAGE);
                }
	    }
            else if(name.equals(printAction.getValue(NAME)))  // Handle Print
	    {
                Printable printer = new Printer();
			
		// Get a PrinterJob object
                PrinterJob printJob = PrinterJob.getPrinterJob();

                // Display print dialog,if user return OK, setPrintable and print
                if (printJob.printDialog()) 
                {
                    printJob.setPrintable(printer);
                    try
		    {
                        printJob.print();
                    }
                    catch(PrinterException pe)
		    {
                        System.out.println(pe);
                        JOptionPane.showMessageDialog(null, "Error printing",     // Display printing 
                                                      "Printer Error",            // error message
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                }      
	    }
            else if(name.equals(exitAction.getValue(NAME)))  // Handle Exit
	    {
                dispose();
	    }
        }
    }

    // OperationAction inner class.
    class OperationAction extends AbstractAction
    {
        // Constructor with one argument.
        OperationAction(String name)
        { 
            super(name);
        }

        // Constructor with two arguments.
        OperationAction(String name, KeyStroke keystroke)
        {
            this(name);
            if(keystroke != null )
                putValue(ACCELERATOR_KEY, keystroke);
        }

        // Event handler for OperationAction.
        public void actionPerformed(ActionEvent e)
        {
            String name = (String)getValue(NAME);
            if(name.equals(createAction.getValue(NAME)))  // Handle Create
	    {
                String[] names = {"Problem", "Data", "Input", "Pred", "Subroutines", "Aes", 
                                  "Aesinitial", "Model", "PK", "Theta", "Omega", "Des", 
                                  "Error", "Sigma", "Estimation", "Covariance", "TableEst", 
                                  "ScatterPlotEst", "Simulation", "TableSim", "ScatterPlotSim"};   
                MDAObject object = new MDAObject();
                for(int i = 0; i < 21; i++)
                    object.getRecords().setProperty(names[i], ""); 
                
                JWizardPane wp = new JWizardPane(new MDAIterator(), object); 
                wp.getContentPanel().setBackground(new Color(240, 245, 255));   
                wp.setContentImage((new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/wizard/nonmem-spk.gif"))).getImage()); 
                wp.createDialog(null , "MDA - Control File Generation Wizard").show();
             
                if(wp.getCustomizedObject() == null)
                    return;
                object = (MDAObject)wp.getCustomizedObject();
                Properties records = object.getRecords();
                String control = "";
                for(int i = 0; i < 21; i++)
                {
                    if(!records.getProperty(names[i]).equals("")) 
                        control = control + records.getProperty(names[i]) + "\n"; 
                } 
                
                if(JOptionPane.showConfirmDialog(null, 
                                                 "Do you want to save the NONMEM control file?",   
                                                 "Question Dialog",
                                                 JOptionPane.YES_NO_OPTION,
                                                 JOptionPane.QUESTION_MESSAGE) == 0)
                {
                    files.setDialogTitle("Save NONMEM Control File");
                    files.setSelectedFile(new File("control.txt")); 
                    int result = files.showSaveDialog(null);
                    if(result == files.APPROVE_OPTION)
		    {
                        file = files.getSelectedFile();
                        try
                        {
                            BufferedWriter out = new BufferedWriter(new FileWriter(file));
                            out.write(control);
                            out.close();
                        }
                        catch(IOException ioe )
                        {
                            System.err.println(ioe);
                            JOptionPane.showMessageDialog(null, "Error saving file",  // Display saving file
                                                          "File Error",               // error message
                                                          JOptionPane.ERROR_MESSAGE);
                        }
                    }   
                }
                
                if(JOptionPane.showConfirmDialog(null, 
                                                 "Do you want to save the SPK input file?",   
                                                 "Question Dialog",
                                                 JOptionPane.YES_NO_OPTION,
                                                 JOptionPane.QUESTION_MESSAGE) == 0)
                {
                    XMLWriter writer = new XMLWriter(object.getControl(), object.getData()); 
                    writer.setGeneral();
                    writer.setDriver();
                    writer.setModel();
                    writer.setData();
                    writer.setPresentation(); 
                    files.setDialogTitle("Save SPK Input File");
                    files.setSelectedFile(new File("input.txt"));                    
                    int result = files.showSaveDialog(null); 
                    if(result == files.APPROVE_OPTION)
		    {
                        file = files.getSelectedFile();
                        writer.save(file.getPath());
                    }
                    textArea.setText(writer.getDocument());
                }
            }
            if(name.equals(transmitAction.getValue(NAME)))  // Handle Transmit
	    {
                String fileName = JOptionPane.showInputDialog("Enter file name to transmit"); // Get filename 
                                                                                              // from the user
                if(fileName == null)
                    return;
                else if(fileName.equals(""))
		{
                    JOptionPane.showMessageDialog(null, "The file needs a name.",  // Display file name 
                                                  "File Name Error",               // error message
                                                  JOptionPane.ERROR_MESSAGE);
                    return;
                }
                try
		{
                    Network network = new Network("https://rose.rfpk.washington.edu:8443/spk/servlet/uw.rfpk.servlets.ReceiveFile",
                                                  sessionId, key);

                    String[] messages = new String[2];
                    messages[0] = fileName;
                    messages[1] = textArea.getText();
                    messages = network.talk(messages, 1);
                 
                    if(messages[0] != "")
                        JOptionPane.showMessageDialog(null, messages[0],                // Display the message 
                                                      "Message from the server",        
                                                      JOptionPane.INFORMATION_MESSAGE); 
                }
                catch(Exception ex)
	        {
                    JOptionPane.showMessageDialog(null, "Error transmitting " + ex,  // Display transmitting 
                                                  "Network Error",             // error message
                                                  JOptionPane.ERROR_MESSAGE);
                }
	    }
            else if(name.equals(receiveAction.getValue(NAME)))  // Handle Receive
	    {
                String fileName = JOptionPane.showInputDialog("Enter file name to receive");  // Get filename
                                                                                              // from the user
                if(fileName == null)
                    return;
                else if(fileName.equals(""))
		{
                    JOptionPane.showMessageDialog(null, "The file needs a name.",  // Display file name 
                                                  "File Name Error",               // error message
                                                  JOptionPane.ERROR_MESSAGE);
                    return;
                }
                try
		{
                    Network network = new Network("https://rose.rfpk.washington.edu:8443/spk/servlet/uw.rfpk.servlets.TransmitFile",
                                                  sessionId, key);

                    String[] messages = new String[1];
                    messages[0] = fileName;
                    messages = network.talk(messages, 2);
                    if(messages[0] != "")                                           // Disply the file content
                        textArea.setText(messages[0]);                               
                                                                                    
                    if(messages[1] != "")
                        JOptionPane.showMessageDialog(null, messages[1],            // display the message 
                                                      "Message from the server",   
                                                      JOptionPane.INFORMATION_MESSAGE);
                }
                catch(Exception ex)
	        {
                    JOptionPane.showMessageDialog(null, "Error receiving",    // Display receiving 
                                                  "Network Error",            // error message
                                                  JOptionPane.ERROR_MESSAGE);
                }
	    } 
        }
    }

    // This method performs save file operation.
    private void saveOperation()
    {
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(file));
            out.write(textArea.getText());
            out.close();
        }
        catch(IOException ioe )
        {
            System.err.println(ioe);
            JOptionPane.showMessageDialog(null, "Error saving file",  // Display saving file
                                          "File Error",               // error message
                                          JOptionPane.ERROR_MESSAGE);
        }
    }
 
    // This inner class implements Printable interface.
    class Printer implements Printable
    {
        // This method implements print() in Printable interface.
        public int print(Graphics gc, PageFormat pageFormat, int pageIndex)
        {
            if (pageIndex != 0) return NO_SUCH_PAGE;
            Graphics2D g2 = (Graphics2D)gc;
            String text = textArea.getText();

            // Get upper-left corner coordinates
            int lineInsetX  = (int)pageFormat.getImageableX();
            int lineInsetY  = (int)pageFormat.getImageableY();

            // Set font for the text
            g2.setFont(printFont);
            FontRenderContext frc = g2.getFontRenderContext();

            // Get LineMetrics object
            LineMetrics lineMetrics = textFont.getLineMetrics(text,frc);

            // Get text height, set color, set stroke
            int textHeight = (int)lineMetrics.getHeight();
            g2.setPaint(Color.black);
	    g2.setStroke(new BasicStroke());
                    
            // Draw text content line by line
            int j = 0;
            for (int i = 0; i < text.length(); i++) 
            {                        
                if (text.charAt(i) == '\n') 
                {
                    lineInsetY += textHeight;
                    g2.drawString(text.substring(j,i),lineInsetX,lineInsetY); 
                    j = i + 1; 
                }      
            }                 

            return PAGE_EXISTS;
        }
    }

    // Window menu bar
    private JMenuBar menuBar = new JMenuBar();      

    // File menu items
    private FileAction openAction, closeAction, saveAction, 
                       saveAsAction, pageSetupAction, printAction, exitAction;

    // Operation menu items
    private OperationAction createAction, transmitAction, receiveAction;

    // Text area
    private JTextArea textArea = new JTextArea();

    // File chooser
    private JFileChooser files = new JFileChooser();

    // Current file
    private File file = null;

    // Text font
    private Font textFont = new Font("Lucida Console",Font.PLAIN,14);

    // Print font
    private Font printFont = new Font("Lucida Console",Font.PLAIN,10);

    // Session ID
    private String sessionId = null;

    // Secret key
    private SecretKey key = null;
}


