/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;
import tps.TpsDisplay;
import tps.TpsDisplayApplet;

public class TpsDisplayEventListener extends Applet implements ComponentListener {
    TpsDisplay tpsDisplay;
    TpsDisplayApplet tpsDisplayApplet;
    Dimension size;
    
    public TpsDisplayEventListener(TpsDisplay tw,TpsDisplayApplet ta) {
	tpsDisplay = tw;
	tpsDisplayApplet = ta;
    }

    public void componentHidden(ComponentEvent e) {
    }

    public void componentMoved(ComponentEvent e) {
	tpsDisplayApplet.refresh = true;
    }

    public void componentShown(ComponentEvent e) {
	tpsDisplayApplet.refresh = true;
    }

    public void componentResized(ComponentEvent e) {
	if ((tpsDisplayApplet != null) && (tpsDisplay != null)) {
	    size = tpsDisplay.size();
	    tpsDisplayApplet.setDimen(size.width,size.height);
	}
    }
}    
