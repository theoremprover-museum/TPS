/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import java.io.*;
import tps.TpsPrompt;

public class TpsMenuActionListener extends Applet implements ActionListener {
    TpsWin tpsWin;
    String command;
    boolean exit = false;
    boolean gencommand = false;
    boolean textfield = false;
    DataOutputStream sendtps;


    public void tpsMenuActionInit(TpsWin tw,String c) {
	tpsWin = tw;
	command = c;
	if (command.equals("EXIT")) {
	    exit = true;
	}
    }

    public void tpsMenuActionGenCommand(DataOutputStream se) {
	sendtps = se;
	gencommand = true;
    }

    public void tpsLineActionCommand(TpsWin tw,DataOutputStream se) {
	tpsWin = tw;
	sendtps = se;
	textfield = true;
    }

    public void actionPerformed(ActionEvent e) {
	if (exit) {
	    System.out.println("Killing Tps");
	    tpsWin.sendTpsCommand("INTERRUPT"); // stop what Tps is doing, then exit
	    tpsWin.sendTpsCommand(command);
	    tpsWin.exitTpsWin();
	} else if (gencommand) {
	    TpsPrompt tpsPrompt = new TpsPrompt();
	    tpsPrompt.TpsDialogInit(sendtps,99,"Enter Command","COMMAND","","Any TPS Command");
	    tpsPrompt.pack();
	    tpsPrompt.show();
	} else if (textfield) {
	    command = tpsWin.resp.getText();
	    tpsWin.sendTpsCommand(command);
	    if (command.equals("EXIT") || command.equals("Exit") || command.equals("exit")) {
		tpsWin.exitTpsWin();
	    } else {
		tpsWin.resp.setText("");
	    }
	} else {
	    tpsWin.sendTpsCommand(command);
	}
    }
}
