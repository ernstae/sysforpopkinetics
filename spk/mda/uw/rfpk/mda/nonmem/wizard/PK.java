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
package uw.rfpk.mda.nonmem.wizard;

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import javax.swing.JComponent;
import javax.swing.text.DefaultEditorKit;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JOptionPane;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import java.awt.Color;
import java.util.Vector;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * This class defines a step to create the $PK record.
 * @author  Jiaji Du
 */
public class PK extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private MDAIterator iterator = null;
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private boolean isHighlighted = false;    
    private DefaultHighlighter highlighter = new DefaultHighlighter();
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter1 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,255,0));
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter2 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,0,0));
    
    /** Creates new form PK.
     * @param iter a MDAIterator object to initialize the field iterator.
     */
    public PK(MDAIterator iter) {
        iterator = iter; 
        initComponents();
        jButton1.addActionListener(new DefaultEditorKit.CutAction());
        jButton2.addActionListener(new DefaultEditorKit.CopyAction());
        jButton3.addActionListener(new DefaultEditorKit.PasteAction());
        jTextArea1.getDocument().addDocumentListener(new MyDocumentListener());
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();

        setLayout(new java.awt.BorderLayout());

        setBorder(new javax.swing.border.EmptyBorder(new java.awt.Insets(0, 12, 12, 12)));
        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter code for model parameters.");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 57);
        jPanel1.add(jTextPane1, gridBagConstraints);

        jButton1.setText("Cut");
        jButton1.setPreferredSize(new java.awt.Dimension(67, 25));
        jPanel1.add(jButton1, new java.awt.GridBagConstraints());

        jButton2.setText("Copy");
        jButton2.setPreferredSize(new java.awt.Dimension(67, 25));
        jPanel1.add(jButton2, new java.awt.GridBagConstraints());

        jButton3.setText("Paste");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton3, new java.awt.GridBagConstraints());

        add(jPanel1, java.awt.BorderLayout.NORTH);

    }//GEN-END:initComponents

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
    }//GEN-LAST:event_jButton3ActionPerformed
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    private class MyDocumentListener implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {
            isValid = true;
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());         
        }
        public void removeUpdate(DocumentEvent e) {
            if(jTextArea1.getText().equals(""))
            {
                isValid = false;
                wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
            }
        }
        public void changedUpdate(DocumentEvent e) {}
    }
    
    /**
     * This method is to return the StepDescriptor object.
     * @return a StepDescriptor object.
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{ 

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "Model Parameters";
  	}

	public String getStepTitle(){
	    return "Model Parameters";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("PK");
                if(text != null)
                {
                    jTextArea1.setText(text.substring(3).trim());
                    iterator.getReload().remove("PK");
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
                }
            }
            else
            {
                String value = ((MDAObject)wizard.getCustomizedObject()).getRecords().getProperty("PK");
                if(value.equals(""))
                    jTextArea1.setText(Utility.defaultPK(iterator.getAdvan(), iterator.getTrans()));
            }
            jTextArea1.requestFocusInWindow();
	}

	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }            
            MDAObject object = (MDAObject)wizard.getCustomizedObject();        
            String record = jTextArea1.getText().trim().replaceAll("\r", "").toUpperCase();
            String title = getStepTitle();
            if(!record.equals(""))
            {
                object.getRecords().setProperty("PK", "$PK " + "\n" + record);
                object.getSource().pk = "\n" + record + "\n";
                // Eliminate comments
                String code = Utility.eliminateComments(record); 
                // Find number of THETAs
                iterator.setNTheta(Utility.find(code, "THETA"));
                if(iterator.getNTheta() == 0)
                    JOptionPane.showMessageDialog(null, "The number of fixed effect parameters is 0.\n",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);                
                // Find number of ETAs
                iterator.setNEta(Utility.find(code.replaceAll("THETA", ""), "ETA"));
                if(iterator.getNEta() == 0)
                    JOptionPane.showMessageDialog(null, "The number of random effect parameters is 0.\n",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);                
                // Check NONMEM compatibility
                Vector names = Utility.checkMathFunction(code, title);
                // Check parenthesis mismatch
                Vector lines = Utility.checkParenthesis(record, title);
                // Highlight the incompatible function names and mismatched parenthesis lines
                if(isHighlighted)
                {                
                    highlighter.removeAllHighlights();
                    isHighlighted = false;
                }
                if(names.size() > 0 || lines.size() > 0)
                {
                    jTextArea1.setHighlighter(highlighter);
                    Element paragraph = jTextArea1.getDocument().getDefaultRootElement();          
                    String[] text = jTextArea1.getText().split("\n");
                    try
                    {
                        for(int i = 0; i < text.length; i++)
                        {
                            for(int j = 0; j < names.size(); j++)
                            {
                                int comment = text[i].indexOf(";");
                                if(comment != 0)
                                {
                                    String line = text[i];
                                    if(comment > 0)
                                        line = text[i].substring(0, comment);
                                    int pos = 0;
                                    int offset = paragraph.getElement(i).getStartOffset();
                                    String name = (String)names.get(j);
                                    while ((pos = text[i].indexOf(name, pos)) >= 0) 
                                    {                
                                        highlighter.addHighlight(pos + offset, pos + offset + name.length(), highlight_painter1);
                                        pos += name.length();
                                        isHighlighted = true;
                                    }
                                }
                            }
                        }
                        for(int i = 0; i < lines.size(); i++)
                        {
                            int n = ((Integer)lines.get(i)).intValue(); 
                            highlighter.addHighlight(paragraph.getElement(n).getStartOffset(),
                                                     paragraph.getElement(n).getEndOffset() - 1,
                                                     highlight_painter2); 
                            isHighlighted = true;                    
                        }
                    }
                    catch(BadLocationException e) 
                    {
                        JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);
                    }                    
                }                
            }
	}
        
	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $PK Record", 
                                 PK.class.getResource("/uw/rfpk/mda/nonmem/help/PK.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/PK.html");  
                }
            };
	}
        
        public String getHelpID() {
            return "PK";
        }
        
    }
}