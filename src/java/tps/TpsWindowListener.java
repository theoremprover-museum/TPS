/*
 * 
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;

public class TpsWindowListener extends Applet implements WindowListener {
    TpsWin tpsWin;

    public TpsWindowListener(TpsWin tp) {
	tpsWin = tp;
    }

    public void windowActivated(WindowEvent event) {
    }

    public void windowDeactivated(WindowEvent event) {
    }

    public void windowIconified(WindowEvent event) {
    }

    public void windowDeiconified(WindowEvent event) {
    }

    public void windowOpened(WindowEvent event) {
    }

    public void windowClosing(WindowEvent event) {
	tpsWin.endTpsWin();
    }

    public void windowClosed(WindowEvent event) {
    }
}    
