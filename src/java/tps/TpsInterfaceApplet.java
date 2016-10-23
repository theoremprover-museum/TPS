/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsWin;

public class TpsInterfaceApplet extends Applet {

    public void init() {
	Label label = new Label("");
	add(label);
	String fontshiftstr = getParameter("fontshift");
	int fontshift = (fontshiftstr != null) ? Integer.valueOf(fontshiftstr).intValue() : 0;
	String fontsize = getParameter("fontsize");
	String atstr = getParameter("server");
	String at = (atstr != null) ? atstr : "htps.math.cmu.edu";
	String portstr = getParameter("port");
	int p = (portstr != null) ? Integer.valueOf(portstr).intValue() : 2973;
	try {
	    TpsWin window = new TpsWin();
	    label.setText("Creating TPS Interface Window...");
	    window.TpsWinInit(5000,at,p,fontsize,fontshift);
	    window.TpsWinMenuInit(true);
	    window.resize(700,500);
	    window.show();
	    label.setText("");
	} catch (Exception ex) { // here we should try to create a 1-window interface
	    label.setText("Failed to create window: " + ex);
	}
    }
}
