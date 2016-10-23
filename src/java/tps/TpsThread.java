/*
 *
 */

package tps;

import java.awt.*;
import java.net.*;
import java.io.*;
import tps.TpsWin;
import tps.TpsApplet;
import tps.TpsPrompt;
import tps.TpsDisplay;
import tps.TpsSmallFonts;
import tps.TpsBigFonts;
 
public class TpsThread extends Thread {
    TpsWin tpsWin;
    TpsApplet tpsApplet;
    Socket sock;
    DataInputStream fromtps;
    DataOutputStream sendtps;
    String tpsServHost;
    int fontshift;
    InetAddress addr;
    TpsSmallFonts tpsSmallFonts;
    TpsBigFonts tpsBigFonts;
 
    public TpsThread(TpsWin tw,TpsApplet a,InetAddress ad,Socket s,DataOutputStream se,String host,int fsh) {
	addr = ad;
	tpsWin = tw;
	if (a.bigFonts) {
	    tpsBigFonts = a.tpsBigFonts;
	} else {
	    tpsSmallFonts = a.tpsSmallFonts;
	}
        try {
            sock = s;
            fromtps = new DataInputStream(new BufferedInputStream(sock.getInputStream()));
            sendtps = se;
            tpsApplet = a;
            tpsServHost = host;
            fontshift = fsh;
        } catch (IOException io) {
            System.out.println("IOException");
        }
    }

 
    private void tpsPrintLine(String s) {
        byte[] bl = s.getBytes();
        for (int i = 0; i < bl.length ; i++) {
            tpsApplet.writeChar(bl[i]);
        }
        tpsApplet.writeChar((byte)13);
    }

    private boolean nontrivCharCode(int b) {
	return((b != 10) && (b != 12) && (b != 13) && (b != 32));
    }

    public void run() {
        int recmode = 0;
	// 0          normal font mode
	// 1          symbol font mode
	// 2          prompt msg begin
	// 3          prompt name begin
	// 4          prompt argtyp begin
	// 5          prompt options begin
	// 6          prompt default begin
	// 7          prompt help begin
	// 8          bring up prompt
	// 9          command execution completed
	// 10         top level string begin/end (so menus can be adjusted)
	// 11         display port begin
	// 12         display title begin
	// 13         display width begin (unit = characters)
	// 14         display height begin (unit = characters)
	// 15         bring up display with small fonts
	// 16         bring up display with big fonts
	// 19         change color of text
        int b;
        StringBuffer promptMsg = new StringBuffer();
	boolean promptMsgEmpty = true;
        StringBuffer promptName = new StringBuffer();
        StringBuffer promptArgtyp = new StringBuffer();
        StringBuffer promptDefault = new StringBuffer();
        StringBuffer promptHelp = new StringBuffer();
	boolean promptHelpEmpty = true;
        StringBuffer promptOptions = new StringBuffer();
        StringBuffer outport = new StringBuffer();
        StringBuffer displayTitle = new StringBuffer();
        StringBuffer displayWidth = new StringBuffer();
        StringBuffer displayHeight = new StringBuffer();
	int xSize,ySize;
	String toplevelString = "CMD-TOP";
        StringBuffer toplevel = new StringBuffer();
 
        System.out.println("Running TpsThread");
	if (tpsWin.etps) {
	    tpsApplet.tpsPrintLine("Welcome To Etps.");
	} else {
	    tpsApplet.tpsPrintLine("Welcome To Tps.");
	}
        try {
            while (true) {
                b = fromtps.read();
		//		System.out.println("read " + b + " in recmode " + recmode); // delete me
                if (b == 0) {
                    b = fromtps.read();
		    // System.out.println("changing mode " + b); // delete me
		    switch (b) {
		    case 0:
		    case 1:
		    case 2:
		    case 3:
		    case 4:
		    case 5:
		    case 6:
		    case 7:
			recmode = b;
			break;
		    case 8:
                                // create prompt
                        int l = promptOptions.length();
                        if (l > 0) {
                            TpsPrompt choice = new TpsPrompt();
                            choice.TpsChoiceInit(sendtps,promptMsg.toString(),promptName.toString(),promptHelp.toString());
                            StringBuffer option = new StringBuffer();
                            for (int i = 0; i < l; i++) {
                                char c = promptOptions.charAt(i);
                                if (((short)c == 10) || ((short)c == 12) ||
                                    ((short)c == 13)) {
				    if (option.toString().length() > 0) {
					choice.addItem(option.toString());
					option = new StringBuffer();
				    }
                                } else {
                                    option.append(c);
                                }
                            }
			    choice.pack();
                            choice.show();
                        } else if (promptArgtyp.toString().equals("YESNO")) {
                            TpsPrompt yesno = new TpsPrompt();
                            String pd = promptDefault.toString();
                            if (pd.equals("YES") || pd.equals("Yes") ||
                                pd.equals("yes") || pd.equals("T") ||
                                pd.equals("t")) {
                                yesno.TpsYesnoInit(true,sendtps,promptMsg.toString(),promptName.toString(),promptHelp.toString());
                            } else {
                                yesno.TpsYesnoInit(false,sendtps,promptMsg.toString(),promptName.toString(),promptHelp.toString());
                            }
			    yesno.pack();
                            yesno.show();
                        } else {
                            TpsPrompt dialog = new TpsPrompt();
                            dialog.TpsDialogInit(sendtps,40,promptMsg.toString(),promptName.toString(),promptDefault.toString(),promptHelp.toString());
			    dialog.pack();
                            dialog.show();
                        }
                        // reset prompt info
                        promptMsg = new StringBuffer();
			promptMsgEmpty = true;
                        promptName = new StringBuffer();
                        promptArgtyp = new StringBuffer();
                        promptDefault = new StringBuffer();
                        promptHelp = new StringBuffer();
			promptHelpEmpty = true;
                        promptOptions = new StringBuffer();
                        recmode = 0;
			break;
		    case 9:
                        //command completed
			tpsApplet.writeChar((byte)12);
                        recmode = 0;
			break;
		    case 10:
			if (recmode == 10) {
			    if (!toplevel.toString().equals(toplevelString)) {
				toplevelString = toplevel.toString();
				//			    System.out.println("Top Level: " + toplevelString); // delete me
				tpsWin.adjustMenus(toplevelString);
			    }
			    toplevel = new StringBuffer();
			    recmode = 0; // back to normal
			} else {
			    recmode = 10;
			}
			break;
		    case 11:
		    case 12:
		    case 13:
		    case 14:
			recmode = b;
			break;
		    case 15:
		    case 16:
			// start an output window
			String op = outport.toString();
			int p = (op != null) ? Integer.valueOf(op).intValue() : 0;
			if (p > 0) {
			    try {
				Socket displaySock = new Socket(addr,p);
				String dws = displayWidth.toString();
				String dhs = displayHeight.toString();
				int dw = Integer.valueOf(dws).intValue();
				int dh = Integer.valueOf(dhs).intValue();
				TpsDisplay ow = new TpsDisplay(7000,displayTitle.toString(),displaySock,fontshift);
				ow.show();
				if (b == 16) {
				    xSize = (12 << fontshift);
				    ySize = (20 << fontshift);
				    if (tpsBigFonts == null) {
					tpsBigFonts = new TpsBigFonts();
				    }
				    ow.TpsDisplayBigFonts(tpsBigFonts);
				} else {
				    xSize = (8 << fontshift);
				    ySize = (13 << fontshift);
				    if (tpsSmallFonts == null) {
					tpsSmallFonts = new TpsSmallFonts();
				    }
				    ow.TpsDisplaySmallFonts(tpsSmallFonts);
				}
				ow.setSize(new Dimension((dw * xSize) + 20,(dh * ySize) + 100));
				ow.TpsDisplayStart();
			    } catch (IOException ex) {
				System.out.println("IOException - could not open socket at port " + p + " for display.");
			    }
			} else {
			    System.out.println("Could not get port number");
			}
			recmode = 0;
			outport = new StringBuffer();
			displayTitle = new StringBuffer();
			displayWidth = new StringBuffer();
			displayHeight = new StringBuffer();
			break;
		    case 19:
			tpsApplet.colorCode = (byte)(fromtps.read());
			break;
		    }
                } else if ((b > 0) && (b <= 127)) {
		    //		    System.out.println(recmode + " : " + (char)(b)); // delete me
                    switch (recmode) {
                    case 0:
			tpsApplet.writeChar((byte)b); 
			break;
                    case 1:
			tpsApplet.writeSymbol((byte)b,(byte)(fromtps.read()));
			break;
                    case 2: 
			if ((!promptMsgEmpty) || (nontrivCharCode(b))) {
			    promptMsgEmpty = false;
			    promptMsg.append((char)(b));
			}
			break;
                    case 3: promptName.append((char)(b)); break;
                    case 4: promptArgtyp.append((char)(b)); break;
                    case 5: promptOptions.append((char)(b)); break;
                    case 6: promptDefault.append((char)(b)); break;
                    case 7:
			if ((!promptHelpEmpty) || (nontrivCharCode(b))) {
			    promptHelpEmpty = false;
			    promptHelp.append((char)(b));
			}
			break;
		    case 10: toplevel.append((char)(b)); break;
                    case 11: outport.append((char)(b)); break;
                    case 12: displayTitle.append((char)(b)); break;
                    case 13: displayWidth.append((char)(b)); break;
                    case 14: displayHeight.append((char)(b)); break;
                    }
                }
            }
        } catch (java.io.IOException io) {
            System.out.println("IO Error");
            tpsApplet.tpsPrintLine("Lost Connection.");
        }
    }
}
