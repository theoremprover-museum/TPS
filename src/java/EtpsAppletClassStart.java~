/*
 * 
 */

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.net.*;
import java.io.*;
import tps.TpsWin;

public class EtpsAppletClassStart extends Applet {

    TextField userid, passwd;
    Label ulabel, label, plabel;
    Button button;
    InetAddress addr;
    Socket sock;
    DataOutputStream sendtps;
    DataInputStream fromtps;
    int fontshift;
    String fontsize, at, popups;
    int p;

    public void init() {
	label = new Label("Please Enter User Id and Password                            ");
	add(label);
	String fontshiftstr = getParameter("fontshift");
	fontshift = (fontshiftstr != null) ? Integer.valueOf(fontshiftstr).intValue() : 0;
	fontsize = getParameter("fontsize");
	String atstr = getParameter("server");
	at = (atstr != null) ? atstr : "htps.math.cmu.edu";
	String portstr = getParameter("port");
	popups = getParameter("popups");
	p = (portstr != null) ? Integer.valueOf(portstr).intValue() : 2973;
	try {
	    addr = InetAddress.getByName(at);
	    sock = new Socket(addr,p);
            fromtps = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
	    sendtps = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));
	    ulabel = new Label("User Id");
	    add(ulabel);
	    userid = new TextField("",15);
	    add(userid);
	    plabel = new Label("Password");
	    add(plabel);
	    passwd = new TextField("",15);
	    passwd.setEchoCharacter('*');
	    add(passwd);
	    button = new Button("Enter");
	    add(button);
        } catch (IOException ex) {
            label.setText("IOException - could not open socket to " + at + ":" + p);
        }
    }

    public boolean action(Event evt,Object arg) {
	if (evt.target instanceof Button) {
	    String user = userid.getText();
	    String pass = passwd.getText();
	    byte[] ubl = user.getBytes();
	    byte[] pbl = pass.getBytes();
	    try {
		sendtps.write(ubl);
		sendtps.write(0);
		sendtps.write(pbl);
		sendtps.write(0);
		sendtps.flush();
	    } catch (IOException ex) {
		label.setText("IO Exception - lost connection.");
	    }
	    try {
		int b1 = fromtps.read();
		int b2 = fromtps.read();
		int b3 = fromtps.read();
		int b4 = fromtps.read();
		int b5 = fromtps.read();
		sendtps.close();
		fromtps.close();
		if (b1 != 0) {
		    label.setText("Sorry - Unrecognized UserId or Password.");
		} else {
		    try {
			label.setText("Accepted! Creating TPS Interface Window...");
			p = (b2 << 24) + (b3 << 16) + (b4 << 8) + b5;
			TpsWin window = new TpsWin();
			if (popups != null) {
			    window.popups = true;
			} else {
			    window.popups = false;
			}
			window.etps = true;
			window.TpsWinInit(5000,at,p,fontsize,fontshift);
			window.TpsWinMenuInit(true);
			if (popups == null) {
			    window.sendNoPopups();
			}
			window.resize(700,500);
			window.show();
			label.setText("");
		    } catch (Exception ex) { // here we should try to create a 1-window interface
			label.setText("Failed to create window: " + ex);
		    }
		}
	    } catch (IOException ex) {
		label.setText("IO Problem - Quitting");
	    }
	}
	return(true);
    }
}
