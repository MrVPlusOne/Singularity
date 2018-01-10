/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.AsyncScalr;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.io.PrintStream;

public class AsyncScalrSingleThreadTest
extends AbstractScalrTest {
    private static int ITERS = 100000;
    private static BufferedImage ORIG;

    public void test() throws InterruptedException {
        for (int i = 0; i < ITERS; ++i) {
            if (i % 100 == 0) {
                System.out.println("Scale Iteration " + i);
            }
            ScaleThread t = new ScaleThread();
            t.start();
            t.join();
        }
    }

    static {
        System.setProperty("imgscalr.async.threadCount", "1");
        ORIG = AsyncScalrSingleThreadTest.load("mr-t.jpg");
    }

    public class ScaleThread
    extends Thread {
        @Override
        public void run() {
            try {
                AsyncScalr.resize(ORIG, 125, new BufferedImageOp[0]).get();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

}

