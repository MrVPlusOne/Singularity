/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.client;

import com.ainfosec.ibasys.client.AESUtil;
import com.ainfosec.ibasys.client.IBASysClient;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.WritableRaster;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.IntBuffer;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.KeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.imageio.ImageIO;

public class IBASysClientParser {
    public static final int AES_Key_Size = 128;
    public static final int AES_BIGKey_Size = 256;
    Cipher pkCipher;
    Cipher aesCipher;
    private static SocketChannel client;
    private static IBASysClientParser instance;
    byte[] aesKey;
    byte[] iv;
    boolean gotresponse = false;
    public int responsesize = 0;
    private WaitForResponse readthread;
    private Thread t;

    public void makeAESKey() throws NoSuchAlgorithmException {
        KeyGenerator kgen = KeyGenerator.getInstance("AES");
        kgen.init(128);
        SecretKey key = kgen.generateKey();
        this.aesKey = key.getEncoded();
    }

    public byte[] rsaEncryptAESKey(byte[] aesKey, File publicKeyFile, String uname) throws IOException, GeneralSecurityException {
        ByteArrayOutputStream baes = new ByteArrayOutputStream();
        baes.write(aesKey);
        byte[] unambytes = uname.getBytes();
        byte[] copyOf = Arrays.copyOf(unambytes, 128);
        baes.write(copyOf);
        byte[] aesKey2 = baes.toByteArray(); //aeskey2 = [aesKey|username.extendTo(128)]
        byte[] encodedKey = new byte[(int)publicKeyFile.length()];
        new FileInputStream(publicKeyFile).read(encodedKey);
        X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(encodedKey);
        KeyFactory kf = KeyFactory.getInstance("RSA");
        PublicKey pk = kf.generatePublic(publicKeySpec); //pk is fixed
        this.pkCipher.init(1, pk);
        ByteArrayOutputStream baos = new ByteArrayOutputStream(2000000);
        CipherOutputStream os = new CipherOutputStream(baos, this.pkCipher);
        os.write(aesKey2);
        os.flush();
        os.close();
        byte[] encrypedData = baos.toByteArray();
        return encrypedData;
    }

    public static IBASysClientParser start() throws NoSuchAlgorithmException, NoSuchPaddingException {
        if (instance == null) {
            instance = new IBASysClientParser();
        }
        return instance;
    }

    public static void stop() throws IOException {
        client.close();
        instance = null;
    }

    private IBASysClientParser() throws NoSuchAlgorithmException, NoSuchPaddingException {
        try {
            client = SocketChannel.open(new InetSocketAddress(IBASysClient.servername, 5453));
            this.pkCipher = Cipher.getInstance("RSA");
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String sendMessage(String msg) throws IOException {
        ByteBuffer buffer = ByteBuffer.wrap(this.extractBytes(msg));
        String response = null;
        try {
            int write = client.write(buffer);
            System.out.print("num wrote:" + write);
            buffer.clear();
            System.out.print("wait for it.");
            ByteBuffer bufferin = ByteBuffer.allocate(1000);
            int read = client.read(bufferin);
            System.out.print("got it." + read + ":");
            Charset charset = Charset.forName("us-ascii");
            CharsetDecoder decoder = charset.newDecoder();
            CharBuffer charBuffer = decoder.decode(bufferin);
            String result = charBuffer.toString();
            System.out.println(result);
            IntBuffer ibuf = bufferin.asIntBuffer();
            int[] iarr = new int[10];
            System.out.print("response=");
            for (int i = 0; i < iarr.length; ++i) {
                int get = ibuf.get(i);
                System.out.print("" + get + ",");
            }
            System.out.println("");
            bufferin.clear();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return response;
    }

    public byte[] extractBytesI(String ImageName) throws IOException {
        File imgPath = new File(ImageName);
        BufferedImage bufferedImage = ImageIO.read(imgPath);
        WritableRaster raster = bufferedImage.getRaster();
        DataBufferByte data = (DataBufferByte)raster.getDataBuffer();
        System.out.println(data.getSize());
        return data.getData();
    }

    public byte[] extractBytes(String ImageName) throws IOException {
        File fi = new File(ImageName);
        byte[] fileContent = Files.readAllBytes(fi.toPath());
        return fileContent;
    }

    DataHolder sendMessage(String imagesbowiejpg, String valtosend, String uname) throws IOException {
        byte[] extractBytes = this.extractBytes(imagesbowiejpg);
        return this.sendMessage(extractBytes, valtosend, uname);
    }

    DataHolder sendMessage(byte[] extractBytes, String valtosend, String uname) throws IOException {
        DataHolder datastate = new DataHolder();
        Object response = null;
        try {
            datastate.data = extractBytes;
            datastate.aeskey = this.aesKey;
            this.iv = this.makeAESIV();
            datastate.iv = this.iv;
            this.prepareHeaderBuffer("public.der", datastate, uname);
            byte[] fileContentFULL = null;
            if (datastate.aesencrypteddata[datastate.aesencrypteddata.length - 1] == 0) {
                System.exit(1);
            }
            int packetsnum = datastate.aesencrypteddata.length / 10000;
            int extra = datastate.aesencrypteddata.length / 10000;
            int fullsize = packetsnum * 10000 + 10000;
            fileContentFULL = Arrays.copyOf(datastate.aesencrypteddata, fullsize);
            datastate.aesencrypteddata = fileContentFULL;
        }
        catch (NoSuchAlgorithmException ex) {
            Logger.getLogger(IBASysClientParser.class.getName()).log(Level.SEVERE, null, ex);
        }
        catch (GeneralSecurityException ex) {
            Logger.getLogger(IBASysClientParser.class.getName()).log(Level.SEVERE, null, ex);
        }
        return datastate;
    }

    public boolean lowLevelSend(DataHolder datastate) {
        int start = 0;
        int increment = 10000;
        for (start = 0; start < datastate.aesencrypteddata.length; start += increment) {
            int end = start + increment;
            if (end > datastate.aesencrypteddata.length) {
                end = datastate.aesencrypteddata.length;
            }
            byte[] bufferpart = Arrays.copyOfRange(datastate.aesencrypteddata, start, end);
            byte s = bufferpart[0];
            byte e = bufferpart[increment - 1];
            ByteArrayOutputStream baes = new ByteArrayOutputStream();
            try {
                baes.write(datastate.header);
                baes.write(bufferpart);
                this.lowLevelSendPart(ByteBuffer.wrap(baes.toByteArray()));
                baes.close();
            }
            catch (IOException ex) {
                Logger.getLogger(IBASysClientParser.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return true;
    }

    public boolean lowLevelSendPart(ByteBuffer buffer) {
        if (this.gotresponse && buffer == null) {
            return false;
        }
        try {
            if (buffer != null) {
                int write = client.write(buffer);
                buffer.clear();
            }
            if (this.readthread == null) {
                this.readthread = new WaitForResponse();
                this.t = new Thread(this.readthread);
                this.t.start();
            }
            if (buffer == null) {
                try {
                    this.t.join();
                }
                catch (InterruptedException ex) {
                    Logger.getLogger(IBASysClientParser.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            ByteBuffer bufferin = ByteBuffer.allocate(1000);
            bufferin.flip();
            byte[] array = bufferin.array();
            Charset charset = Charset.forName("us-ascii");
            CharsetDecoder decoder = charset.newDecoder();
            CharBuffer charBuffer = decoder.decode(bufferin);
            String result = charBuffer.toString();
            if (result.contains("result")) {
                this.gotresponse = true;
            }
            IntBuffer ibuf = bufferin.asIntBuffer();
            int[] iarr = new int[10];
            bufferin.clear();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }

    public byte[] getHeader(String pkey, byte[] key, byte[] data, String uname) throws IOException, GeneralSecurityException {
        int ivSize = 16;
        byte[] iv = new byte[ivSize];
        SecureRandom random = new SecureRandom();
        random.nextBytes(iv);
        byte[] saveKey = this.rsaEncryptAESKey(key, new File(pkey), uname);
        byte[] encrypt = AESUtil.encrypt(key, iv, data);
        ByteArrayOutputStream bbuff = new ByteArrayOutputStream(saveKey.length + iv.length + encrypt.length);
        bbuff.write(saveKey);
        bbuff.write(ivSize);
        bbuff.write(encrypt);
        byte[] total = bbuff.toByteArray();
        return total;
    }

    private byte[] makeAESIV() {
        byte[] iv = new byte[16];
        SecureRandom random = new SecureRandom();
        random.nextBytes(iv);
        return iv;
    }

    public void prepareHeaderBuffer(String pkey, DataHolder datastate, String uname) throws IOException, GeneralSecurityException {
        ClassLoader classLoader = this.getClass().getClassLoader();
        File pkeyfile = new File(pkey);
        byte[] aeskeyrsaencrypt = this.rsaEncryptAESKey(datastate.aeskey, pkeyfile, uname);
        datastate.aesencrypteddata = AESUtil.encrypt(datastate.aeskey, datastate.iv, datastate.data);
        ByteArrayOutputStream bbuff = new ByteArrayOutputStream(aeskeyrsaencrypt.length + datastate.iv.length);
        bbuff.write(aeskeyrsaencrypt);
        bbuff.write(datastate.iv);
        datastate.header = bbuff.toByteArray();
    }

    public class DataHolder {
        public byte[] data;
        public byte[] aesencrypteddata;
        public byte[] header;
        public byte[] aeskey;
        public byte[] rsapubkey;
        int status;
        public byte[] iv;
    }

    public class WaitForResponse
    implements Runnable {
        @Override
        public void run() {
            while (!IBASysClientParser.this.gotresponse) {
                try {
                    ByteBuffer bufferin = ByteBuffer.allocate(1000);
                    client.socket().setSoTimeout(1000);
                    int read = client.read(bufferin);
                    bufferin.flip();
                    byte[] array = bufferin.array();
                    System.out.print("got it." + read + ":");
                    Charset charset = Charset.forName("us-ascii");
                    CharsetDecoder decoder = charset.newDecoder();
                    byte[] val = Arrays.copyOf(array, 6);
                    ByteBuffer chars = ByteBuffer.wrap(val);
                    CharBuffer charBuffer = decoder.decode(chars);
                    String result = charBuffer.toString();
                    System.out.println(result + array.length);
                    if (array.length > 6) {
                        byte[] data = Arrays.copyOfRange(array, 6, read);
                        byte[] key = Arrays.copyOfRange(array, 6, 38);
                        System.out.println("got key" + key.length);
                        System.out.println("Here's your token:");
                        byte[] decrypt = AESUtil.decrypt(IBASysClientParser.this.aesKey, new byte[16], key);
                        System.out.println(Arrays.toString(decrypt));
                        System.out.println("Warning: the server doesn't tell us if you failed authentication. It just sends a bad token if you failed.");
                        System.out.println("If this token is bad, you will not know it until you use it somewhere else and a death ray blows you up thru your computer.");
                        System.out.println("Have a nice day!");
                    }
                    if (result.contains("result") || result.contains("error")) {
                        IBASysClientParser.this.gotresponse = true;
                        IBASysClientParser.this.responsesize = read;
                    }
                    IntBuffer ibuf = bufferin.asIntBuffer();
                    int[] iarr = new int[10];
                    System.out.println("");
                    bufferin.clear();
                }
                catch (IOException ex) {
                    Logger.getLogger(IBASysClientParser.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

}

