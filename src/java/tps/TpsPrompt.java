/*
 *
 */

package tps;

import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;
import tps.TpsPromptWindowListener;

public class TpsPrompt extends Frame {
    DataOutputStream sendtps;
    String promptName;
    Button button, cbutton, yesbutton, nobutton, msgbutton;
    TextField resp;
    TextArea aresp;
    String def;
    Choice choice;
    Label msgl;
    TextArea helptxt;
    GridBagLayout gridbag;
    GridBagConstraints c;

    public TpsPrompt() {
	gridbag = new GridBagLayout();
	c = new GridBagConstraints();
	setLayout(gridbag);
    }

    private void PromptMsg(String msg) {
	c.fill = GridBagConstraints.BOTH;
	c.gridwidth = GridBagConstraints.REMAINDER;
	c.weightx = 0.0;
	c.weighty = 0.0;
	msgl = new Label(msg);
	msgl.setAlignment(Label.CENTER);
	gridbag.setConstraints(msgl,c);
	add(msgl);
    }

    private void PromptHelp(String help) {
	TpsPromptWindowListener winListener = new TpsPromptWindowListener();

	addWindowListener(winListener);
	winListener.setTpsPrompt(this);
	
	if (help.length() != 0) {
	    helptxt = new TextArea("HELP:\n" + help);
	    helptxt.setEditable(false);
	    c.gridheight = GridBagConstraints.REMAINDER;
	    c.weightx = 1.0;
	    c.weighty = 1.0;
	    gridbag.setConstraints(helptxt,c);
	    add(helptxt);
	}
    }
    
    public void TpsDialogInit(DataOutputStream se,int i,String msg,String name,String d,String help) {
	sendtps = se;
	def = new String(d);
	if ((i + 10) < d.length()) {
	    i = d.length() + 10;
	}
	setTitle(msg);
	PromptMsg(msg);
	Panel p2 = new Panel();
	button = new Button("OK");
	if (i <= 100) {
	    resp = new TextField(i);
	    resp.setText(d);
	    p2.add(resp);
	} else {
	    aresp = new TextArea(d);
	    aresp.setEditable(true);
	    p2.add(aresp);
	}
	p2.add(button);
	gridbag.setConstraints(p2,c);
	add(p2);
	PromptHelp(help);
	promptName = name;
    }

    public void TpsChoiceInit(DataOutputStream se,String msg,String name,String help) {
	sendtps = se;
	setTitle(msg);
	PromptMsg(msg);
	Panel p2 = new Panel();
	cbutton = new Button("OK");
	choice = new Choice();
	p2.add(choice);
	p2.add(cbutton);
	gridbag.setConstraints(p2,c);
	add(p2);
	PromptHelp(help);
	promptName = name;
    }

    public void TpsYesnoInit(boolean b,DataOutputStream se,String msg,String name,String help) {
	sendtps = se;
	setTitle(msg);
	PromptMsg(msg);
	Panel p2 = new Panel();
	yesbutton = new Button("Yes");
	nobutton = new Button("No");
	if (b) {
	    p2.add(yesbutton);
	    p2.add(nobutton);
	} else {
	    p2.add(nobutton);
	    p2.add(yesbutton);
	}
	gridbag.setConstraints(p2,c);
	add(p2);
	PromptHelp(help);
	promptName = name;
    }

    public void SimpleMsgPrompt(String msg) {
	setTitle(msg);
	PromptMsg(msg);
	msgbutton = new Button("Close");
	c.fill = GridBagConstraints.NONE;
	gridbag.setConstraints(msgbutton,c);
	add(msgbutton);
    }

    public void addItem(String ch) {
	choice.addItem(ch);
    }
    
    private void sendTpsLine(String ln) {
	byte[] bl = ln.getBytes();
	try {
	    sendtps.write(bl);
	    sendtps.write(0);
	    sendtps.flush();
	} catch (IOException ex) {
	    System.out.println("IO Exception sending TPS line " + ln);
	}
    }

    public boolean action(Event event, Object arg) {
        if ((event.target == button) | (event.target == resp)) {
	    sendTpsLine(promptName);
	    if (resp.getText().equals(def)) {
		sendTpsLine("");
	    } else {
		sendTpsLine(resp.getText());
	    }
	    dispose();
	} else if ((event.target == button) | (event.target == aresp)) {
	    sendTpsLine(promptName);
	    if (aresp.getText().equals(def)) {
		sendTpsLine("");
	    } else {
		sendTpsLine(aresp.getText());
	    }
	    dispose();
	} else if ((event.target == cbutton) || (event.target == choice)) {
	    sendTpsLine(promptName);
	    sendTpsLine(" " + (choice.getSelectedIndex() + 1));
	    dispose();
	} else if (event.target == yesbutton) {
	    sendTpsLine(promptName);
	    sendTpsLine("YES");
	    dispose();
	} else if (event.target == nobutton) {
	    sendTpsLine(promptName);
	    sendTpsLine("NO");
	    dispose();
	} else if (event.target == msgbutton) { // just a message, don't send TPS anything
	    dispose();
	}
        return true;
    }

}
