/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import com.ainfosec.ibasys.client.AESUtil;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import com.ainfosec.ibasys.org.imgscalr.ScalrApplyTest;
import com.ainfosec.ibasys.server.LoginManager;
import com.ainfosec.ibasys.server.LoginSession;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.channels.SocketChannel;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ImageMatcher {
    static Map<String, ImageMatcher> threadstates;
    int length;
    byte[] aeskey;
    long starttime;
    byte[] data;
    public SocketChannel client;
    public String user;
    private int[] pcode;
    public String hkey;
    public boolean complete = false;
    private boolean loaded = false;
    static Map<Long, ImageMatcherWorker> checkers; //fixme: try multiple workers to corrupt states?

    static ImageMatcher init(String key) {
        if (threadstates == null) {
            threadstates = new Hashtable<String, ImageMatcher>();
        }
        ImageMatcher checkState = threadstates.get(key);
        return checkState;
    }

    public ImageMatcher(byte[] aeskey, byte[] data, long starttime) {
        this.aeskey = aeskey;
        this.data = data;
        this.starttime = starttime;
    }

    public static void startImageMatcherWorker(LoginSession session, ImageMatcher cstate, String key, byte[] data, byte[] iv, byte[] aeskey, long starttime, SocketChannel client, String user, int[] pcode) {
        try {
            ImageMatcher checkState = cstate == null ? threadstates.get(key) : cstate;
            if (checkState == null && session.size > 340000) {
                checkState = new ImageMatcher(aeskey, data, starttime);
                threadstates.put(key, checkState);
            }
            if (checkState == null && session.size <= 340000) {
                if (Scalr.DEBUG) {
                    Scalr.log(0, "Incorrect Image [width=%d, height=%d] to [x=%d, y=%d, width=%d, height=%d]...", session.size);
                }
                return;
            }
            if (checkState == null) {
                session.iscomplete = true;
                return;
            }
            ImageMatcherWorker checker = checkState.getChecker(checkState, data, iv, aeskey, starttime);
            checkState.client = client;
            checkState.hkey = key;
            checker.user = user;
            checkState.pcode = pcode;
            session.setData(null);
            session.databuff = null;
            session.start = 0;
            boolean foundworker = false;
            Thread t = new Thread(checker);
            t.start();
        }
        catch (IOException ex) {
            Logger.getLogger(ImageMatcher.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public ImageMatcherWorker getChecker(ImageMatcher state, byte[] data, byte[] iv, byte[] aeskey, long starttime) {
        return new ImageMatcherWorker(state, data, iv, aeskey, starttime);
    }

    class ImageMatcherWorker
    implements Runnable {
        public byte[] imagedata;
        ImageMatcher state;
        long starttime;
        byte[] iv;
        byte[] aeskey;
        public String user;
        String errorcode;
        StringBuffer error;
        boolean success;

        public ImageMatcherWorker(ImageMatcher state, byte[] data, byte[] iv, byte[] aeskey, long starttime) {
            this.errorcode = "";
            this.error = new StringBuffer();
            this.success = false;
            this.starttime = starttime;
            if (ImageMatcher.checkers == null) {
                ImageMatcher.checkers = new Hashtable<Long, ImageMatcherWorker>();
            }
            this.aeskey = aeskey;
            this.iv = iv;
            this.imagedata = Arrays.copyOf(data, data.length);
            this.state = state;
        }

        @Override
        public void run() {
            System.out.println("worker started:" + this.starttime);
            if (ImageMatcher.checkers.containsKey(this.starttime)) {
                System.out.println("worker not registering");
                return;
            }
            System.out.println("worker registering");
            ImageMatcher.checkers.put(this.starttime, this);
            System.out.println("worker running");
            this.state.loaded = true;
            try {
                byte[] decrypt = AESUtil.decrypt(this.aeskey, this.iv, this.imagedata);
                System.out.println("Loading passcode");
                ScalrApplyTest st = new ScalrApplyTest();
                ScalrApplyTest.setup(decrypt);
                BufferedImage testImage = st.testApply1();
                int w = testImage.getWidth();
                int h = testImage.getHeight();
                int[] imageDataBuff = testImage.getRGB(0, 0, w, h, null, 0, w);
                ByteBuffer byteBuffer = ByteBuffer.allocate(imageDataBuff.length * 4);
                IntBuffer intBuffer = byteBuffer.asIntBuffer();
                intBuffer.put(imageDataBuff);
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                baos.write(byteBuffer.array());
                baos.flush();
                byte[] imageInByte = baos.toByteArray();
                baos.close();
                System.out.println("Image Done");
                ScalrApplyTest.tearDown();
                byte[] pcodetest = new byte[128];
                int csize = imageDataBuff.length / 128;
                int ii = 0;
                for (int i = 0; i < csize * 128; i += csize) {
                    pcodetest[ii] = (byte)(imageDataBuff[i] % 2);
                    ++ii;
                }
                this.imagedata = pcodetest;
                System.out.println("worker ended, no error:" + this.starttime);
            }
            catch (Exception ex) {
                this.errorcode = "101";
                System.out.println("worker ended, error:" + this.starttime + " " + ex.getMessage());
                Logger.getLogger(ImageMatcher.class.getName()).log(Level.SEVERE, null, ex);
                this.error.append(ex.getMessage());
            }
            if (this.state.loaded) {
                this.success = true;
                for (int i = 0; i < this.imagedata.length && i < ImageMatcher.this.pcode.length; i += 4) {
                    boolean r;
                    int b = Math.abs(this.imagedata[i]);
                    int p = Math.abs(ImageMatcher.this.pcode[i]);
                    boolean bl = r = p % 2 == b % 2;
                    if (!r) {
                        this.success = false;
                        break;
                    }
                    this.imagedata[i] = (byte)(r ? 1 : 0);
                }
            } else {
                this.success = false;
            }
            System.out.println("user: " + this.user + " - status:" + this.success);
            LoginManager.response.add(this);
            this.state.complete = true;
        }
    }

}

