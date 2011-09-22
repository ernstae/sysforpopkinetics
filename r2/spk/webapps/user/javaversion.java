import java.applet.*;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.Font;

public class javaversion extends Applet
{
    public void paint(Graphics g)
    {
        String s = System.getProperty("java.version");
        g.setFont(new Font("SansSerif", Font.BOLD, 20));
        if(!s.startsWith("1.6"))
        {
            g.setColor(Color.red);
	    g.drawRect(0, 0, 
		       getSize().width - 1,
		       getSize().height - 1);
            g.drawString("The java version on your computer is " + s + ".", 5, 24);
            g.drawString("You need to upgrade it to version 1.6.", 5, 50);
        }
        else
        {
            g.setColor(Color.blue);
	    g.drawRect(0, 0, 
		       getSize().width - 1,
		       getSize().height - 1);
            g.drawString("The java version on your computer is " + s + ".", 5, 24);
            g.drawString("This version is correct for the MDA.", 5, 50);
        }
    }
}
