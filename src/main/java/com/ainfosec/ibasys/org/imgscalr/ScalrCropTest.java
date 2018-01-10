/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ColorConvertOp;

public class ScalrCropTest
extends AbstractScalrTest {
    public void testCropEX() {
        try {
            Scalr.crop(src, 3200, 2400, new BufferedImageOp[0]);
        }
        catch (Exception exception) {
            // empty catch block
        }
        try {
            Scalr.crop(src, -8, -10, 100, 100, new BufferedImageOp[0]);
        }
        catch (Exception exception) {
            // empty catch block
        }
        try {
            Scalr.crop(src, -100, -200, -4, -4, new BufferedImageOp[0]);
        }
        catch (Exception exception) {
            // empty catch block
        }
    }

    public void testCropWH() {
        ScalrCropTest.assertEquals(ScalrCropTest.load("time-square-crop-wh.png"), Scalr.crop(src, 320, 240, new BufferedImageOp[0]));
    }

    public void testCropXYWH() {
        ScalrCropTest.assertEquals(ScalrCropTest.load("time-square-crop-xywh.png"), Scalr.crop(src, 100, 100, 320, 240, new BufferedImageOp[0]));
    }

    public void testCropXYWHOps() {
        ScalrCropTest.assertEquals(ScalrCropTest.load("time-square-crop-xywh-ops.png"), Scalr.crop(src, 100, 100, 320, 240, Scalr.OP_GRAYSCALE));
    }
}

