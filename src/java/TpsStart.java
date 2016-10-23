/*
 * 
 */

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsWin;

public class TpsStart extends Applet {

    public static void main(String args[]) {
	int l = args.length;
	String host = "";
	int port = 0;
	String fontsize = "small";
	int fontshift = 0;
        TpsWin window = new TpsWin();
        window.inAnApplet = false;
	int defx = 700, defy = 500;
	int maxChars = 20000;
 
	if (l < 2) {
	    System.exit(0);
	} else {
	    host = args[0];
	    port = Integer.valueOf(args[1]).intValue();
	    System.out.println("host = " + host + "; port = " + port);
	    for (int i = 2; i < l; i++) {
		if (args[i].equals("-big")) {
		    fontsize = "big";
		} else if (args[i].equals("-x2")) {
		    fontshift = 1;
		} else if (args[i].equals("-x4")) {
		    fontshift = 2;
		} else if (args[i].equals("-nopopups")) {
		    window.popups = false;
		} else if (args[i].equals("-etps")) {
		    window.etps = true;
		} else if ((args[i].equals("-maxChars")) && (args[i+1] != null)) {
		    maxChars = Integer.valueOf(args[i+1]).intValue();
		    i++;
		} else if ((args[i].equals("-rightOffset")) && (args[i+1] != null)) {
		    window.rightOffset = Integer.valueOf(args[i+1]).intValue();
		    i++;
		} else if ((args[i].equals("-bottomOffset")) && (args[i+1] != null)) {
		    window.bottomOffset = Integer.valueOf(args[i+1]).intValue();
		    i++;
		} else if ((args[i].equals("-screenx")) && (args[i+1] != null)) {
		    defx = Integer.valueOf(args[i+1]).intValue();
		    i++;
		} else if ((args[i].equals("-screeny")) && (args[i+1] != null)) {
		    defy = Integer.valueOf(args[i+1]).intValue();
		    i++;
		}
	    }
	}
	System.out.println("initializing window");// delete me
	window.TpsWinInit(maxChars,host,port,fontsize,fontshift);
	System.out.println("initializing menus");// delete me
	window.TpsWinMenuInit(true);
	System.out.println("resizing");// delete me
	window.pack();
        window.resize(defx,defy);
        window.show();
    }
}
