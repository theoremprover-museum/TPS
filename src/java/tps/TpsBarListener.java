/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;
import tps.TpsApplet;

public class TpsBarListener extends Applet implements AdjustmentListener {
    TpsApplet tpsApplet;
    
    public TpsBarListener(TpsApplet a) {
	tpsApplet = a;
    }

    public void adjustmentValueChanged(AdjustmentEvent e) {
	tpsApplet.bottom = e.getValue() + tpsApplet.screenY;
	tpsApplet.refresh = true;
	tpsApplet.repaint();
    }
}    
