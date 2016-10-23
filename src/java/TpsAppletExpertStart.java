/*
 *
 */

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsWin;

public class TpsAppletExpertStart extends Applet {

    TextField passwd;
    Label label;

    public void init() {
	label = new Label("Enter Remote Expert Password");
	add(label);
	passwd = new TextField("",15);
	passwd.setEchoCharacter('*');
	add(passwd);
	Button button = new Button("Enter");
	add(button);
    }

    public boolean action(Event evt,Object arg) {
	if (evt.target instanceof Button) {
	    String s = passwd.getText(); 
	    int sc = 0;
	    boolean wrong = false;
	    for (int i=0; i < s.length(); i++) {
		char c = s.charAt(i);
		System.out.println("char code " + i + ": " + c);
		if ((c < 97) || (c > 116)) {
		    System.out.println("bad");
		    wrong = true;
		} else {
		    sc = (sc * 20) + (c - 97);
		}
	    }
	    if ((!wrong) && (sc == 1212943844)) {
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
		    window.sendExpertStatus();
		    window.resize(700,500);
		    window.show();
		    label.setText("");
		} catch (Exception ex) { // here we should try to create a 1-window interface
		    label.setText("Failed to create window: " + ex);
		}
	    } else {
		label.setText("Sorry.  Wrong Password.");
	    }
	}
	return(true);
    }
}
