/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorConvertOp;

public class ScalrResizeTest
extends AbstractScalrTest {
    public void testResizeEX() {
        try {
            Scalr.resize(src, -1, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, 240, -1, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, (Scalr.Method)null, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, (Scalr.Mode)null, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, (Scalr.Method)null, 240, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, (Scalr.Mode)null, 240, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, null, null, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
        try {
            Scalr.resize(src, null, null, 240, 240, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
    }

    public void testResizeSize() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-320.png"), Scalr.resize(src, 320, new BufferedImageOp[0]));
    }

    public void testResizeWH() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-640x480.png"), Scalr.resize(src, 640, 480, new BufferedImageOp[0]));
    }

    public void testResizeSizeSpeed() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-320-speed.png"), Scalr.resize(src, Scalr.Method.SPEED, 320, new BufferedImageOp[0]));
    }

    public void testResizeWHSpeed() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-640x480-speed.png"), Scalr.resize(src, Scalr.Method.SPEED, 640, 480, new BufferedImageOp[0]));
    }

    public void testResizeSizeExact() {
        System.setProperty("imgscalr.debug", "true");
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-320-fit-exact.png"), Scalr.resize(src, Scalr.Mode.FIT_EXACT, 320, new BufferedImageOp[0]));
    }

    public void testResizeWHExact() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-640x640-fit-exact.png"), Scalr.resize(src, Scalr.Mode.FIT_EXACT, 640, 640, new BufferedImageOp[0]));
    }

    public void testResizeSizeSpeedExact() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-320-speed-fit-exact.png"), Scalr.resize(src, Scalr.Method.SPEED, Scalr.Mode.FIT_EXACT, 320, new BufferedImageOp[0]));
    }

    public void testResizeWHSpeedExact() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-640x640-speed-fit-exact.png"), Scalr.resize(src, Scalr.Method.SPEED, Scalr.Mode.FIT_EXACT, 640, 640, new BufferedImageOp[0]));
    }

    public void testResizeWHSpeedExactOps() {
        ScalrResizeTest.assertEquals(ScalrResizeTest.load("time-square-resize-640x640-speed-fit-exact-ops.png"), Scalr.resize(src, Scalr.Method.SPEED, Scalr.Mode.FIT_EXACT, 640, 640, Scalr.OP_GRAYSCALE));
    }

    public void testResizeUltraQuality() {
        System.setProperty("imgscalr.debug", "true");
        BufferedImage i = new BufferedImage(32, 32, 1);
        Scalr.resize(i, Scalr.Method.ULTRA_QUALITY, 1, new BufferedImageOp[0]);
    }

    public void testResizeFitExact() {
        BufferedImage i = new BufferedImage(500, 500, 1);
        BufferedImage i2 = Scalr.resize(i, Scalr.Mode.FIT_EXACT, 500, 250, new BufferedImageOp[0]);
    }

    public void testResizeAutoVsFitBoth() {
        BufferedImage landscape = new BufferedImage(500, 250, 1);
        this.testResizeAutoVsBoth(landscape, 500, 250, 500, 250, 500, 250);
        this.testResizeAutoVsBoth(landscape, 500, 500, 500, 250, 500, 250);
        this.testResizeAutoVsBoth(landscape, 800, 300, 800, 400, 600, 300);
        this.testResizeAutoVsBoth(landscape, 800, 400, 800, 400, 800, 400);
        this.testResizeAutoVsBoth(landscape, 800, 500, 800, 400, 800, 400);
        this.testResizeAutoVsBoth(landscape, 250, 150, 250, 125, 250, 125);
        this.testResizeAutoVsBoth(landscape, 250, 125, 250, 125, 250, 125);
        this.testResizeAutoVsBoth(landscape, 250, 100, 250, 125, 200, 100);
        BufferedImage portrait = new BufferedImage(250, 500, 1);
        this.testResizeAutoVsBoth(portrait, 250, 500, 250, 500, 250, 500);
        this.testResizeAutoVsBoth(portrait, 500, 500, 250, 500, 250, 500);
        this.testResizeAutoVsBoth(portrait, 300, 800, 400, 800, 300, 600);
        this.testResizeAutoVsBoth(portrait, 400, 800, 400, 800, 400, 800);
        this.testResizeAutoVsBoth(portrait, 500, 800, 400, 800, 400, 800);
        this.testResizeAutoVsBoth(portrait, 150, 250, 125, 250, 125, 250);
        this.testResizeAutoVsBoth(portrait, 125, 250, 125, 250, 125, 250);
        this.testResizeAutoVsBoth(portrait, 100, 250, 125, 250, 100, 200);
        BufferedImage square = new BufferedImage(500, 500, 1);
        this.testResizeAutoVsBoth(square, 500, 500, 500, 500, 500, 500);
        this.testResizeAutoVsBoth(square, 800, 800, 800, 800, 800, 800);
        this.testResizeAutoVsBoth(square, 400, 400, 400, 400, 400, 400);
        this.testResizeAutoVsBoth(square, 800, 600, 800, 800, 600, 600);
    }

    private void testResizeAutoVsBoth(BufferedImage i, int targetWidth, int targetHeight, int autoWidth, int autoHeight, int fitBothWidth, int fitBothHeight) {
        BufferedImage auto = Scalr.resize(i, Scalr.Mode.AUTOMATIC, targetWidth, targetHeight, new BufferedImageOp[0]);
        BufferedImage fitBoth = Scalr.resize(i, Scalr.Mode.BEST_FIT_BOTH, targetWidth, targetHeight, new BufferedImageOp[0]);
    }
}

