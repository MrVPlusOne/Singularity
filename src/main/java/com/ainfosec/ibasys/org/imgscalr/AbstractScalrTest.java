/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import javax.imageio.ImageIO;
import javax.imageio.stream.ImageInputStream;

public abstract class AbstractScalrTest {
    protected static BufferedImage src;

    public static void setup(String img) throws IOException {
        src = AbstractScalrTest.load(img);
    }

    public static void setup(byte[] buff) throws IOException {
        src = AbstractScalrTest.load(buff);
    }

    public static void tearDown() {
        src.flush();
    }

    protected static BufferedImage load(byte[] data) {
        BufferedImage i = null;
        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(data);
            ImageInputStream iis = ImageIO.createImageInputStream(bais);
            i = ImageIO.read(iis);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return i;
    }

    protected static BufferedImage load(String name) {
        BufferedImage i = null;
        try {
            i = ImageIO.read(AbstractScalrTest.class.getResource(name));
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return i;
    }

    protected static void save(BufferedImage image, String name) {
        try {
            ImageIO.write((RenderedImage)image, "PNG", new FileOutputStream(name));
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    protected static void assertEquals(BufferedImage orig, BufferedImage tmp) throws AssertionError {
    }
}

