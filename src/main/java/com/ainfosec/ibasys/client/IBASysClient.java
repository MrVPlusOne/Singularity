/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.client;

import benchmarks.IBASys;
import com.ainfosec.ibasys.client.IBASysClientParser;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
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

    public static int runMaliciousClient(String[] args) throws IOException, InterruptedException, GeneralSecurityException {
        String username = null;
        String pcodeimagefilename = null;
        if (args.length > 1) {
            servername = args[1];
            username = args[2];
            pcodeimagefilename = args[3];
        }
        IBASysClient et = new IBASysClient("1");
        et.setup();


        IBASysClientParser client = et.client;

//        IBASysClientParser.DataHolder dh1 = client.sendMessage(IBASys.getImageData(100000),"any","hansolo");
        IBASysClientParser.DataHolder dh1 = et.givenServerClient_whenServerEchosMessage_thenCorrect(pcodeimagefilename, username);

        byte[][] extraData= IBASys.getExtraData(10000, 32);
        byte[][] newHeaders = new byte[extraData.length][];
        for(int i =0; i< extraData.length; i++){
            newHeaders[i]=et.client.mkHeader("public.der", dh1, "hansolo" + i);
//            newHeaders[i]=et.client.mkHeader("public.der", dh1, "long12345678901234567890" + i);
        }

//        et.client.modifiedlLowLevelSend(dh1, extraData, 10);

        dh1.aesencrypteddata = Arrays.copyOf(dh1.aesencrypteddata, dh1.aesencrypteddata.length - 10000);
        et.client.modifiedlLowLevelSend(dh1, extraData, newHeaders);


        System.out.println("All data sent");

//        IBASysClientParser.DataHolder dh2 = et.givenServerClient_whenServerEchosMessage_thenCorrect(pcodeimagefilename, username+"_postfix");
//        dh2.aeskey = dh.aeskey;
//        et.client.lowLevelSend(dh1);
//        et.client.modifiedlLowLevelSend(dh1);
//        et.client.modifiedlLowLevelSend(dh1);

//        System.out.println("Malicious data sent");


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

