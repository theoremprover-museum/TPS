/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;

public class TpsPromptWindowListener extends Applet implements WindowListener {
    TpsPrompt tpsPrompt;

    public void setTpsPrompt(TpsPrompt tp) {
	tpsPrompt = tp;
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
	tpsPrompt.dispose();
    }

    public void windowClosed(WindowEvent event) {
    }
}    
