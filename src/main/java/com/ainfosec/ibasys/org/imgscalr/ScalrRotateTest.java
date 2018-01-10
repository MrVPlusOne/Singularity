/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorConvertOp;

public class ScalrRotateTest
extends AbstractScalrTest {
    public void testRotateEX() {
        try {
            Scalr.rotate(src, null, new BufferedImageOp[0]);
        }
        catch (IllegalArgumentException illegalArgumentException) {
            // empty catch block
        }
    }

    public void testRotate90() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-90.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.CW_90, new BufferedImageOp[0]));
    }

    public void testRotate180() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-180.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.CW_180, new BufferedImageOp[0]));
    }

    public void testRotate270() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-270.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.CW_270, new BufferedImageOp[0]));
    }

    public void testRotateFlipH() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-horz.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.FLIP_HORZ, new BufferedImageOp[0]));
    }

    public void testRotateFlipV() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-vert.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.FLIP_VERT, new BufferedImageOp[0]));
    }

    public void testRotateFlipHOps() {
        ScalrRotateTest.assertEquals(ScalrRotateTest.load("time-square-rotate-horz-ops.png"), Scalr.rotate(ScalrRotateTest.load("time-square.png"), Scalr.Rotation.FLIP_HORZ, Scalr.OP_GRAYSCALE));
    }
}

