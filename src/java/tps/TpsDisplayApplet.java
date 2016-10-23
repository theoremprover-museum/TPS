/*
 *
 */

package tps;
 
import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsSmallFonts;
import tps.TpsBigFonts;
 
public class TpsDisplayApplet extends Applet {
    TpsSmallFonts tpsSmallFonts;
    TpsBigFonts tpsBigFonts;
    int maxChars;
    boolean bigFonts = false;
    int fontshift=0;
    int x=1,y=1; // position to place next symbol
    int xSize,ySize,bufferPos,width,height;
    byte[] bufferImj,bufferImi,bufferCol;
    short[] bufferX,bufferY;
    public boolean refresh = true;
    public byte colorCode;
 
    public TpsDisplayApplet(int mc,TpsSmallFonts f,int fontsh) {
	tpsSmallFonts = f;
	bigFonts = false;
	displayInit(mc,fontsh);
    }

    public TpsDisplayApplet(int mc,TpsBigFonts f,int fontsh) {
	tpsBigFonts = f;
	bigFonts = true;
	displayInit(mc,fontsh);
    }

    public void displayInit(int mc, int fontsh) {
        maxChars = mc;
	fontshift = fontsh;
        bufferPos = 0;
        bufferImj = new byte[maxChars];
        bufferImi = new byte[maxChars];
        bufferCol = new byte[maxChars];
        bufferX = new short[maxChars];
        bufferY = new short[maxChars];
	colorCode = 0;
 
        if (bigFonts) {
            xSize = (12 << fontshift);
            ySize = (20 << fontshift);
        } else {
            xSize = (8 << fontshift);
            ySize = (13 << fontshift);
        }
    }

    private void movePos() {
        x = x + xSize;
    }
 
    private void lnFeed() {
        x = 1;
        y = y + ySize;
    }

    public void clearDisplay() {
	for (int i = 0; i < bufferPos; i++) {
	    bufferImj[i] = -1;
	}
	bufferPos = 0;
	x = 1;
	y = 1;
	refresh = true;
	repaint();
    }
 
    public void writeChar(byte c) {
	if (bufferPos < maxChars) {
	    if ((c == 10) || (c == 13)) {
		lnFeed();
	    } else if (c == 12) {
		if (x > 1) {
		    lnFeed();
		}
		bufferImj[bufferPos] = 0;
		bufferImi[bufferPos] = 12;
		bufferCol[bufferPos] = 0;
		bufferX[bufferPos] = (short)x;
		bufferY[bufferPos] = (short)y;
		bufferPos++;
	    } else {
		if (bigFonts) {
		    if (tpsBigFonts.blankp(0,c)) {
			movePos();
		    } else {
			bufferImj[bufferPos] = 0;
			bufferImi[bufferPos] = c;
			bufferCol[bufferPos] = colorCode;
			bufferX[bufferPos] = (short)x;
			bufferY[bufferPos] = (short)y;
			bufferPos++;
			movePos();
			repaint();
		    }
		} else {
		    if (tpsSmallFonts.blankp(0,c)) {
			movePos();
		    } else {
			bufferImj[bufferPos] = 0;
			bufferImi[bufferPos] = c;
			bufferCol[bufferPos] = colorCode;
			bufferX[bufferPos] = (short)x;
			bufferY[bufferPos] = (short)y;
			bufferPos++;
			movePos();
			repaint();
		    }
		}
            }
        } else {
	    System.out.println("Out of memory for Display - increase the value in TpsThread.");
	}
    }
 
    public void writeSymbol(byte cj,byte ci) {
	if (bufferPos < maxChars) {
	    if (bigFonts) {
		if (tpsBigFonts.blankp(cj,ci)) {
		    movePos();
		} else {
		    bufferImj[bufferPos] = cj;
		    bufferImi[bufferPos] = ci;
		    bufferCol[bufferPos] = colorCode;
		    bufferX[bufferPos] = (short)x;
		    bufferY[bufferPos] = (short)y;
		    bufferPos++;
		    movePos();
		    repaint();
		}
	    } else {
		if (tpsSmallFonts.blankp(cj,ci)) {
		    movePos();
		} else {
		    bufferImj[bufferPos] = cj;
		    bufferImi[bufferPos] = ci;
		    bufferCol[bufferPos] = colorCode;
		    bufferX[bufferPos] = (short)x;
		    bufferY[bufferPos] = (short)y;
		    bufferPos++;
		    movePos();
		    repaint();
		}
	    }
        } else {
	    System.out.println("Out of memory for Display - increase the value in TpsThread.");
	}
    }
 
    private void paintPos(Graphics g, int i) {
	if (bufferImj[i] >= 0) {
	    if ((bufferImj[i] == 0) && (bufferImi[i] == 12)) {
		g.setColor(Color.blue);
		g.drawLine(0,bufferY[i],600,bufferY[i]);
	    } else {
		if (bigFonts) {
		    tpsBigFonts.drawTpsChar(g,bufferImj[i],bufferImi[i],bufferCol[i],bufferX[i],bufferY[i],fontshift);
		} else {
		    tpsSmallFonts.drawTpsChar(g,bufferImj[i],bufferImi[i],bufferCol[i],bufferX[i],bufferY[i],fontshift);
		}
	    }
	}
    }
 
    public void setDimen(int w,int h) {
	width = w;
	height = h;
	refresh = true;
	repaint();
    }

    public void paint(Graphics g) {
	if (refresh) {
	    g.setColor(Color.white);
	    g.fillRect(0,0,width,height);
	    refresh = false;
	}
	g.setColor(Color.black);
	for (int i = 0; i < bufferPos; i++) {
	    paintPos(g,i);
	}
    }
 
    public void update(Graphics g) {
        paint(g);
    }

    public Dimension minimumSize() {
        return new Dimension(600,200);
    }
 
    public Dimension preferredSize() {
        return minimumSize();
    }
}
 
