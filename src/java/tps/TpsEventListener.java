/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;

public class TpsEventListener extends Applet implements ComponentListener {
    TpsWin tpsWin;
    TpsApplet tpsApplet;
    Dimension size;

    public TpsEventListener(TpsWin tp,TpsApplet ta) {
	tpsWin = tp;
	tpsApplet = ta;
    }

    public void componentHidden(ComponentEvent e) {
	tpsApplet.refresh = true;
    }

    public void componentMoved(ComponentEvent e) {
	tpsApplet.refresh = true;
    }

    public void componentShown(ComponentEvent e) {
	tpsApplet.refresh = true;
    }

    public void componentResized(ComponentEvent e) {
	if ((tpsApplet != null) && (tpsWin != null)) {
	    int w,h;
	    size = tpsApplet.size();
	    w = size.width - tpsWin.rightOffset; // a little room for the scrollbar
	    h = size.height - tpsWin.bottomOffset; // a little extra room at the bottom
	    int rm = (w / tpsApplet.xSize) - 2;
	    tpsApplet.tpsResize(w,h);
	    tpsWin.sendRightMargin(rm);
	    if (tpsWin.resp != null) {
		tpsWin.resp.setColumns(w / 8);
	    }
	}
    }
}    
