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

public class TpsDisplayWindowListener extends Applet implements WindowListener {
    TpsDisplay tpsDisplay;
    TpsDisplayApplet tpsDisplayApplet;

    public TpsDisplayWindowListener(TpsDisplay tp,TpsDisplayApplet ta) {
	tpsDisplay = tp;
	tpsDisplayApplet = ta;
    }

    public void windowActivated(WindowEvent event) {
	tpsDisplayApplet.refresh = true;
    }

    public void windowDeactivated(WindowEvent event) {
	tpsDisplayApplet.refresh = true;
    }

    public void windowIconified(WindowEvent event) {
    }

    public void windowDeiconified(WindowEvent event) {
	tpsDisplayApplet.refresh = true;
    }

    public void windowOpened(WindowEvent event) {
	tpsDisplayApplet.refresh = true;
    }

    public void windowClosing(WindowEvent event) {
	tpsDisplay.dispose();
    }

    public void windowClosed(WindowEvent event) {
    }
}    
