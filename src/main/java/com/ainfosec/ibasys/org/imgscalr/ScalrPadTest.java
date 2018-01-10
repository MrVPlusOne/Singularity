/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorConvertOp;

public class ScalrPadTest
extends AbstractScalrTest {
    static int pad = 8;
    static Color alpha = new Color(255, 50, 255, 0);

    public void testPadEX() {
        try {
            Scalr.pad(src, -1, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.pad(src, 0, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
    }

    public void testPad() {
        ScalrPadTest.assertEquals(ScalrPadTest.load("time-square-pad-8.png"), Scalr.pad(src, pad, new BufferedImageOp[0]));
    }

    public void testPadColor() {
        ScalrPadTest.assertEquals(ScalrPadTest.load("time-square-pad-8-red.png"), Scalr.pad(src, pad, Color.RED, new BufferedImageOp[0]));
    }

    public void testPadAlpha() {
        ScalrPadTest.assertEquals(ScalrPadTest.load("time-square-pad-8-alpha.png"), Scalr.pad(src, pad, alpha, new BufferedImageOp[0]));
    }

    public void testPadAlphaOps() {
        ScalrPadTest.assertEquals(ScalrPadTest.load("time-square-pad-8-alpha-ops.png"), Scalr.pad(src, pad, alpha, Scalr.OP_GRAYSCALE));
    }
}

