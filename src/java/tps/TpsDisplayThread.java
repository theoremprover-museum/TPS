/*
 *
 */

package tps;
 
import java.net.*;
import java.io.*;
import tps.TpsDisplayApplet;
import tps.TpsDisplay;
 
public class TpsDisplayThread extends Thread {
    TpsDisplayApplet tpsDisplayApplet;
    TpsDisplay tpsDisplay;
    DataInputStream fromtps;
 
    public TpsDisplayThread(TpsDisplay d,TpsDisplayApplet a,DataInputStream fr) {
	tpsDisplay = d;
	tpsDisplayApplet = a;
	fromtps = fr;
    }

    public void run() {
        int recmode = 0;
	boolean con = true;
        // 0 - normal
        // 1 - symbol
	// 17 - clear
	// 18 - close
        int b;
        try {
            while (con) {
                b = fromtps.read();
                if (b == 0) {
                    b = fromtps.read();
		    //		    System.out.println("display changing mode " + b);
		    switch (b) {
		    case 0:
		    case 1:
			recmode = b;
			break;
		    case 17:
			tpsDisplayApplet.clearDisplay();
			recmode = 0;
			break;
		    case 18:
			System.out.println("Closing Display By Request");
			con = false;
			tpsDisplay.dispose();
			break;
		    case 19:
			tpsDisplayApplet.colorCode = (byte)(fromtps.read());
			break;
		    }
                } else if ((b > 0) && (b <= 127)) {
		    //		    System.out.println("display : " + recmode + " : " + (char)(b));
                    switch (recmode) {
                    case 0: tpsDisplayApplet.writeChar((byte)b); break;
                    case 1: tpsDisplayApplet.writeSymbol((byte)b,(byte)(fromtps.read())); break;
                    }
                }
            }
        } catch (java.io.IOException io) {
            System.out.println("Closing Display Due To Lost Connection");
	    tpsDisplay.dispose();
        } // and now this Thread dies
    }
}
