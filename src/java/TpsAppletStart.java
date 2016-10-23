/*
 * 
 */

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsWin;

public class TpsAppletStart extends Applet {

    public void init() {
	Label label = new Label("");
	add(label);
	String fontshiftstr = getParameter("fontshift");
	int fontshift = (fontshiftstr != null) ? Integer.valueOf(fontshiftstr).intValue() : 0;
	String fontsize = getParameter("fontsize");
	String atstr = getParameter("server");
	String at = (atstr != null) ? atstr : "htps.math.cmu.edu";
	String portstr = getParameter("port");
	String known = getParameter("known");
	String popups = getParameter("popups");
	String etps = getParameter("etps");
	int p = (portstr != null) ? Integer.valueOf(portstr).intValue() : 29089;
	try {
	    TpsWin window = new TpsWin();
	    label.setText("Creating TPS Interface Window...");
	    if ((popups == null) && (known != null)) {
		window.popups = false;
	    }
	    if (etps != null) {
		window.etps = true;
	    }
	    window.TpsWinInit(5000,at,p,fontsize,fontshift);
	    window.TpsWinMenuInit(known != null);
	    if (known != null) {
		window.sendExpertStatus();
	    }
	    if ((popups == null) && (known != null)) {
		window.sendNoPopups();
	    }
	    window.resize(700,500);
	    window.show();
	    label.setText("");
	} catch (Exception ex) { // here we should try to create a 1-window interface
	    label.setText("Failed to create window: " + ex);
	}
    }
}
