/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.client;

import com.ainfosec.ibasys.client.IBASysClientParser;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.security.NoSuchAlgorithmException;
import javax.crypto.NoSuchPaddingException;

public class IBASysClient {
    IBASysClientParser client;
    static String servername = "localhost";
    String valtosend = "";

    public static void main(String[] args) throws IOException, InterruptedException, NoSuchAlgorithmException, NoSuchPaddingException {
        IBASysClient.runclient(args);
    }

    public static int runclient(String[] args) throws IOException, InterruptedException, NoSuchAlgorithmException, NoSuchPaddingException {
        String username = null;
        String pcodeimagefilename = null;
        if (args.length > 1) {
            servername = args[1];
            username = args[2];
            pcodeimagefilename = args[3];
        }
        IBASysClient et = new IBASysClient("1");
        et.setup();
        IBASysClientParser.DataHolder dh = et.givenServerClient_whenServerEchosMessage_thenCorrect(pcodeimagefilename, username);
        et.lowLevelSend(dh);
        System.out.println("all sent");
        et.waitforResults();
        et.teardown();
        return et.client.responsesize;
    }

    private IBASysClient(String val) {
        this.valtosend = val;
    }

    public void setup() throws IOException, InterruptedException, NoSuchAlgorithmException, NoSuchPaddingException {
        this.client = IBASysClientParser.start();
        System.out.println("setup");
        this.client.makeAESKey();
    }

    public IBASysClientParser.DataHolder givenServerClient_whenServerEchosMessage_thenCorrect(String pcodeimgloc, String uname) throws IOException {
        IBASysClientParser.DataHolder sendMessage = this.client.sendMessage(pcodeimgloc, this.valtosend, uname);
        return sendMessage;
    }

    public void lowLevelSend(IBASysClientParser.DataHolder dh) throws IOException {
        this.client.lowLevelSend(dh);
    }

    public void teardown() throws IOException {
        IBASysClientParser.stop();
    }

    private void waitforResults() {
        this.client.lowLevelSendPart(null);
        System.out.println("done!");
    }
}

