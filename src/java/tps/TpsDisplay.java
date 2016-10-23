/*
 * For handling vpwindows, proofwindows & editor windows
 */

package tps;
 
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;
import tps.TpsDisplayApplet;
import tps.TpsDisplayThread;
import tps.TpsDisplayEventListener;
import tps.TpsDisplayWindowListener;
 
public class TpsDisplay extends Frame {
    TpsDisplayApplet tpsDisplayApplet;
    DataInputStream fromtps;
    Socket sock;
    int tpsServPort;
    int maxChars;
    TpsSmallFonts tpsSmallFonts;
    TpsBigFonts tpsBigFonts;
    boolean bigFonts;
    int fontshift;
    TpsDisplayThread tpsDisplayThread;
    TpsDisplayEventListener eventListener;
    TpsDisplayWindowListener winListener;

    public TpsDisplay(int mc,String title,Socket displaySock,int fontsh) {
	maxChars = mc;
	setTitle(title);
	sock = displaySock;
	fontshift = fontsh;
	try {
	    fromtps =  new DataInputStream(new BufferedInputStream(sock.getInputStream()));
	} catch (IOException ex) {
	    System.out.println("IOException: Failed to open Input Stream. " + ex);
	    dispose();
	}
    }

    public void TpsDisplaySmallFonts(TpsSmallFonts f) {
	tpsSmallFonts = f;
	bigFonts = false;
    }

    public void TpsDisplayBigFonts(TpsBigFonts f) {
	tpsBigFonts = f;
	bigFonts = true;
    }

    public void TpsDisplayStart() {
	if (bigFonts) {
	    tpsDisplayApplet = new TpsDisplayApplet(maxChars,tpsBigFonts,fontshift);
	} else {
	    tpsDisplayApplet = new TpsDisplayApplet(maxChars,tpsSmallFonts,fontshift);
	}
	tpsDisplayApplet.setDimen(size().width,size().height);
	add(tpsDisplayApplet);
	eventListener = new TpsDisplayEventListener(this,tpsDisplayApplet);
	winListener = new TpsDisplayWindowListener(this,tpsDisplayApplet);
	addComponentListener(eventListener);
	addWindowListener(winListener);
	tpsDisplayThread = new TpsDisplayThread(this,tpsDisplayApplet,fromtps);
	tpsDisplayThread.start();
    }
}
