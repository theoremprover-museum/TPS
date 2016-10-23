/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;

public class TpsKeyListener extends Applet implements KeyListener {
    TpsWin tpsWin;
    int pos = 0;

    public TpsKeyListener(TpsWin tp) {
	tpsWin = tp;
    }

    public void addString(String str) {
	int i = tpsWin.resp.getCaretPosition();
	String fst,snd;

	if (i > 0) {
	    tpsWin.resp.select(0,i);
	    fst = tpsWin.resp.getSelectedText();
	} else {
	    fst = "";
	}
	tpsWin.resp.selectAll();
	tpsWin.resp.setSelectionStart(i);
	snd = tpsWin.resp.getSelectedText();
	if (snd == null) {
	    snd = "";
	}
	tpsWin.resp.setText(fst + str + snd);
	tpsWin.resp.setCaretPosition(i+1);
    }

    public void keyPressed(KeyEvent e) {
	pos = tpsWin.resp.getCaretPosition();
    }

    public void keyReleased(KeyEvent e) {
	if (tpsWin.resp.getCaretPosition() == pos) { // if nothing happened, do it here
	    if (e.getKeyChar() == '"') {
		addString("\"");
	    } else if (e.getKeyChar() == '\'') {
		addString("'");
	    }
	}
    }

    public void keyTyped(KeyEvent e) {
    }
}    
