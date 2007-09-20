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
import javax.swing.JOptionPane;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Vector;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * This class defines a step to create the $PRED record.
 * @author  Jiaji Du
 */
public class Pred extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private boolean isValid = false;
    private boolean isHighlighted = false;
    private JWizardPane wizardPane = null; 
    private MDAIterator iterator = null;
    private DefaultHighlighter highlighter = new DefaultHighlighter();
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter1 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,255,0));
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter2 =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255,0,0));
    
    /** Creates new form Pred.
     * @param iter a MDAIterator object to initialize the field iterator.
     */
    public Pred(MDAIterator iter) {
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
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();

        setLayout(new java.awt.BorderLayout());

        setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 12, 12, 12));
        jScrollPane1.setViewportView(jTextArea1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(238, 238, 238));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter model equations.              ");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jTextPane1, gridBagConstraints);

        jButton2.setText("Copy");
        jButton2.setPreferredSize(new java.awt.Dimension(67, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton2, gridBagConstraints);

        jButton3.setText("Paste");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton3, gridBagConstraints);

        jButton1.setText("Cut");
        jButton1.setPreferredSize(new java.awt.Dimension(67, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jButton1, gridBagConstraints);

        add(jPanel1, java.awt.BorderLayout.NORTH);

    }// </editor-fold>//GEN-END:initComponents

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
        
        private String record;
        private String code;

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "Model Equations";
  	}

	public String getStepTitle(){
	    return "Model Equations";
	}

	public void showingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }
            wizardPane = wizard;
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("PRED");
                if(text != null)
                {
                    text = text.substring(6, text.length() - 1);
 
//                    if(!iterator.getIsInd() && !iterator.getIsTwoStage() &&
//                       iterator.initTwoStage.contains("pred"))
//                    {
//                        text = Utility.replaceEtaByEps(text);
//                        text = Utility.addEtaToTheta(text);
//                        iterator.initTwoStage.remove("pred");
//                    }
                    
                    jTextArea1.setText(text);
                    iterator.getReload().remove("PRED");
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
                }
            }
            jTextArea1.requestFocusInWindow();
	}

        public boolean checkingStep(JWizardPane wizard){
            record = jTextArea1.getText().trim().replaceAll("\r", "").toUpperCase();
            // Correct IF conditions
            record = Utility.correctIFConditions(record);
            while(record.indexOf("\n\n") != -1)
                record = record.replaceAll("\n\n", "\n");
            String title = getStepTitle();
            if(!record.equals(""))
            {
                if(!Utility.checkCharacter(record, title)) return false;
                // Eliminate comments
                code = Utility.eliminateComments(record);
                // Find number of THETAs, ETAs and EPSs
                int nTheta = Utility.find(code, "THETA");
                int nEta = Utility.find(code, "ETA");
                int nEps = Utility.find(code, "EPS");
                if(nTheta == 0)
                {
                    JOptionPane.showMessageDialog(null, "The number of fixed effect parameters is 0.\n",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if(nEta == 0)
                {
                    if(iterator.analysis.equals("population"))
                    {
                        JOptionPane.showMessageDialog(null, "The number of random effect parameters is 0.\n",
                                                      "Input Error", JOptionPane.ERROR_MESSAGE);
                        return false;
                    }
                    else
                    {
                        JOptionPane.showMessageDialog(null, "The number of residual unkown variability parameters is 0.\n",
                                                      "Input Error", JOptionPane.ERROR_MESSAGE);
                        return false;
                    }
                }           
                if(iterator.analysis.equals("population") && nEps == 0)
                {
                    JOptionPane.showMessageDialog(null, "The number of residual unkown variability parameters is 0.\n",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return false;
                }

                // Check ENDIF syntax
                if(!Utility.checkENDIF(code, title)) return false;;
                // Check NONMEM compatibility
                Vector names = Utility.checkMathFunction(code, title);
                // Check parenthesis mismatch
                Vector lines = Utility.checkParenthesis(code, title);
                // Check expression left hand side
                Vector errors = Utility.checkLeftExpression(code, title);
                // Highlight the incompatible function names and mismatched parenthesis lines
                if(isHighlighted)
                {                
                    highlighter.removeAllHighlights();
                    isHighlighted = false;
                }
                if(names.size() > 0 || lines.size() > 0 || errors.size() > 0)
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
                        for(int i = 0; i < errors.size(); i++)
                        {
                            int n = ((Integer)errors.get(i)).intValue(); 
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
                    return false;
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "Code was missing.", "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            return true;
        }
            
	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack()) return;
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            object.getRecords().setProperty("Pred", "$PRED \n" + record);
            object.getSource().pred = "\n" + code.trim() + "\n";
            iterator.setNTheta(Utility.find(code, "THETA"));
            iterator.setNEta(Utility.find(code, "ETA"));
            iterator.setNEps(Utility.find(code, "EPS"));
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpBroker(){
            return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $PRED Record", 
                                 Pred.class.getResource("/uw/rfpk/mda/nonmem/help/Pred.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Pred.html");  
                }
            };
	}
        
        public String getHelpID() {
            return "Prepare_Input_Model_Equations";
        }
    }
}
