/*
 *
 */

package tps;
 
import java.awt.*;
import java.awt.event.*;
import java.applet.Applet;
import tps.TpsSmallFonts;
import tps.TpsBigFonts;
import tps.TpsWin;
 
public class TpsApplet extends Applet {
    TpsSmallFonts tpsSmallFonts;
    TpsBigFonts tpsBigFonts;
    int maxChars;
    public int bottom = 1;
    public boolean refresh = true;
    int relRMarg=1000, maxY=16384, midY=8192;
    public int bottomOffset=20;
    public int screenX=1000, screenY=1000;
    boolean bigFonts = false;
    int fontshift=0;
    int bufferPos=0;
    boolean fullBuffer = false;
    int x=1,y=1; // position to place next symbol
    public int xSize;
    int ySize;
    byte[] bufferImj,bufferImi,bufferCol;
    short[] bufferX,bufferY;
    public byte colorCode;
    TpsWin tpsWin;

    public void tpsInit(int mC, String fontsize,int f,TpsWin tw) {
        maxChars = mC;
        bufferPos = 0;
        bufferImj = new byte[maxChars];
        bufferImi = new byte[maxChars];
        bufferCol = new byte[maxChars];
        bufferX = new short[maxChars];
        bufferY = new short[maxChars];
	colorCode = 0;
	
	tpsWin = tw;
        fontshift = f;
        if (fontsize.equals("big")) {
            bigFonts = true;
        } else {
            bigFonts = false;
        }
	
        if (bigFonts) {
	    tpsBigFonts = new TpsBigFonts();
            xSize = (12 << fontshift);
            ySize = (20 << fontshift);
        } else {
	    tpsSmallFonts = new TpsSmallFonts();
            xSize = (8 << fontshift);
            ySize = (13 << fontshift);
        }
	bottom = y + ySize;
	tpsWin.vBar.setMaximum(0);
	tpsWin.vBar.setMinimum(0);
    }

    private void movePos() {
        x = x + xSize;
        if (x > relRMarg) {
            lnFeed();
        }
    }
    
    private void lnFeed() {
	boolean bottomY = (bottom > y);
        x = 1;
        y = y + ySize;
        if (y > maxY) {
            y = y - midY;
            scrollDown(midY);
        }
	if (bottomY) {
	    bottom = y + ySize;
	    refresh = true;
	}
    }
    
    private void scrollDown(int v) {
        refresh = true;
        if (fullBuffer) {
            for (int i = 0; i < maxChars; i++) {
                if (bufferY[i] < ((short)v)) {
                    bufferImj[i] = -1;
                } else {
                    bufferY[i] = (short)(bufferY[i] - v);
                }
            }
        } else {
            for (int i = 0; i < bufferPos; i++) {
                if (bufferY[i] < ((short)v)) {
                    bufferImj[i] = -1;
                } else {
                    bufferY[i] = (short)(bufferY[i] - v);
                }
            }
        }
    }
    
    private void scrollUp(int v) {
        refresh = true;
        if (fullBuffer) {
            for (int i = 0; i < maxChars; i++) {
		if ((bufferY[i] + (int)v) > maxY) {
		    bufferImj[i] = -1;
		} else {
		    bufferY[i] = (short)(bufferY[i] + v);
		}
            }
        } else {
            for (int i = 0; i < bufferPos; i++) {
		if ((bufferY[i] + (int)v) > maxY) {
		    bufferImj[i] = -1;
		} else {
		    bufferY[i] = (short)(bufferY[i] + v);
		}
            }
        }
    }
    
    private void incBuffer() {
        bufferPos++;
        if (bufferPos == maxChars) {
            bufferPos = 0;
            fullBuffer = true;
        }
    }

    public void tpsPrintLine(String s) {
        byte[] bl = s.getBytes();
        for (int i = 0; i < bl.length ; i++) {
            writeChar(bl[i]);
        }
        writeChar((byte)13);
    }

    public void writeChar(byte c) {
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
	    incBuffer();
	    repaint();
        } else {
	    if (blankp(0,c)) {
		movePos();
	    } else {
		bufferImj[bufferPos] = 0;
		bufferImi[bufferPos] = c;
		bufferCol[bufferPos] = colorCode;
		bufferX[bufferPos] = (short)x;
		bufferY[bufferPos] = (short)y;
		incBuffer();
		movePos();
		repaint();
	    }
        }
    }
    
    public void writeSymbol(byte cj,byte ci) {
	if (blankp(cj,ci)) {
	    movePos();
	} else {
	    bufferImj[bufferPos] = cj;
	    bufferImi[bufferPos] = ci;
	    bufferCol[bufferPos] = colorCode;
	    bufferX[bufferPos] = (short)x;
	    bufferY[bufferPos] = (short)y;
	    incBuffer();
	    movePos();
	    repaint();
	}
    }
    
    public boolean blankp(byte cj,byte ci) {
	if (bigFonts) {
	    return(tpsBigFonts.blankp(cj,ci));
	} else {
	    return(tpsSmallFonts.blankp(cj,ci));
	}
    }
    
    public boolean blankp(int cj,int ci) {
	if (bigFonts) {
	    return(tpsBigFonts.blankp(cj,ci));
	} else {
	    return(tpsSmallFonts.blankp(cj,ci));
	}
    }
    
    private boolean paintPos(Graphics g, int i) {
        if (bufferImj[i] >= 0) {
            int a = (int)(bufferX[i]);
            int b = (bufferY[i] - bottom) + screenY;
	    if ((b > 0) && ((b + ySize) <= screenY)) {
		if ((bufferImj[i] == 0) && (bufferImi[i] == 12)) {
		    g.setColor(Color.blue);
		    g.drawLine(0,b,screenX,b);
		} else {
		    if (bigFonts) {
			tpsBigFonts.drawTpsChar(g,bufferImj[i],bufferImi[i],bufferCol[i],a,b,fontshift);
		    } else {
			tpsSmallFonts.drawTpsChar(g,bufferImj[i],bufferImi[i],bufferCol[i],a,b,fontshift);
		    }
		}
	    }
	    return true;
        } else {
            return false;
        }
    }
    
    public void paint(Graphics g) {
        if (refresh) {
	    g.setColor(Color.white);
            g.fillRect(0,0,size().width,size().height);
            refresh = false;
        }
        int i = (bufferPos - 1);
        boolean done = false;
        while ((i >= 0) && (!done)) {
            if (paintPos(g,i)) {
                i--;
            } else {
                done = true;
            }
        }
        if (!done && fullBuffer) {
            i = maxChars-1;
            while ((i >= bufferPos) && (!done)) {
                if (paintPos(g,i)) {
                    i--;
                } else {
                    done = true;
                }
            }
        }
    }
    
    public void update(Graphics g) {
        paint(g);
	if ((y + ySize) > screenY) {
	    tpsWin.vBar.setMaximum(y + ySize);
	    tpsWin.vBar.setBlockIncrement(screenY);
	    tpsWin.vBar.setVisibleAmount(screenY);
	    tpsWin.vBar.setUnitIncrement(ySize);
	}
	if (bottom >= (y + ySize)) {
	    bottom = y + ySize;
	    tpsWin.vBar.setValue(bottom);
	}
    }
    
    public void tpsResize(int rm,int bm) {
	relRMarg = rm - xSize;
	screenX = rm;
	screenY = bm;
	if (x > relRMarg) {
	    lnFeed();
	}
	refresh = true;
    }
    
    public Dimension minimumSize() {
        return new Dimension(screenX,screenY);
    }
    
    public Dimension preferredSize() {
        return minimumSize();
    }
}
