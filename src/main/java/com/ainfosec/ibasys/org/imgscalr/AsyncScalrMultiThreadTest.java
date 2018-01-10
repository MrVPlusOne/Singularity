/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.AbstractScalrTest;
import com.ainfosec.ibasys.org.imgscalr.AsyncScalr;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.io.PrintStream;
import java.util.ArrayList;

public class AsyncScalrMultiThreadTest
extends AbstractScalrTest {
    private static int ITERS = 100000;
    private static BufferedImage ORIG;

    public void test() throws InterruptedException {
        int i;
        ArrayList<ScaleThread> threadList = new ArrayList<ScaleThread>(ITERS);
        for (i = 0; i < ITERS; ++i) {
            if (i % 100 == 0) {
                System.out.println("Scale Iteration " + i);
            }
            try {
                ScaleThread t = new ScaleThread();
                t.start();
                threadList.add(t);
                continue;
            }
            catch (OutOfMemoryError error) {
                System.out.println("Cannot create any more threads, last created was " + i);
                ITERS = i;
                break;
            }
        }
        for (i = 0; i < ITERS; ++i) {
            if (i % 100 == 0) {
                System.out.println("Thread Finished " + i);
            }
            ((Thread)threadList.get(i)).join();
        }
    }

    static {
        System.setProperty("imgscalr.async.threadCount", "1");
        ORIG = AsyncScalrMultiThreadTest.load("mr-t.jpg");
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

