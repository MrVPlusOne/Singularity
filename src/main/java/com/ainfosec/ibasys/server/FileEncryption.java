/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;
import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class FileEncryption {
    public static final int AES_Key_Size = 256;
    Cipher pkCipher = Cipher.getInstance("RSA");
    Cipher aesCipher = Cipher.getInstance("AES");
    byte[] aesKey;
    SecretKeySpec aeskeySpec;

    public FileEncryption() throws GeneralSecurityException {
    }

    public void makeKey() throws NoSuchAlgorithmException {
        KeyGenerator kgen = KeyGenerator.getInstance("AES");
        kgen.init(256);
        SecretKey key = kgen.generateKey();
        this.aesKey = key.getEncoded();
        this.aeskeySpec = new SecretKeySpec(this.aesKey, "AES");
    }

    public byte[] loadKey(byte[] aeskey, File privateKeyFile) throws GeneralSecurityException, IOException {
        byte[] encodedKey = new byte[(int)privateKeyFile.length()];
        new FileInputStream(privateKeyFile).read(encodedKey);
        PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(encodedKey);
        KeyFactory kf = KeyFactory.getInstance("RSA");
        PrivateKey pk = kf.generatePrivate(privateKeySpec);
        this.pkCipher.init(2, pk);
        byte[] aesKey = new byte[32];
        ByteArrayInputStream bis = new ByteArrayInputStream(aeskey);
        SecretKeySpec secretKey = new SecretKeySpec(encodedKey, "RSA");
        Cipher cipher = Cipher.getInstance("RSA");
        cipher.init(2, pk);
        byte[] outputBytes = cipher.doFinal(aeskey);
        byte[] shortkey = Arrays.copyOfRange(outputBytes, 0, 16);
        return shortkey;
    }

    public byte[] loadUser(byte[] aeskey, File privateKeyFile) throws GeneralSecurityException, IOException {
        byte[] encodedKey = new byte[(int)privateKeyFile.length()];
        new FileInputStream(privateKeyFile).read(encodedKey);
        PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(encodedKey);
        KeyFactory kf = KeyFactory.getInstance("RSA");
        PrivateKey pk = kf.generatePrivate(privateKeySpec);
        this.pkCipher.init(2, pk);
        byte[] aesKey = new byte[32];
        ByteArrayInputStream bis = new ByteArrayInputStream(aeskey);
        SecretKeySpec secretKey = new SecretKeySpec(encodedKey, "RSA");
        Cipher cipher = Cipher.getInstance("RSA");
        cipher.init(2, pk);
        byte[] outputBytes = cipher.doFinal(aeskey);
        byte[] shortkey = Arrays.copyOfRange(outputBytes, 16, 32);
        return shortkey;
    }

    public void saveKey(File out, File publicKeyFile) throws IOException, GeneralSecurityException {
        byte[] encodedKey = new byte[(int)publicKeyFile.length()];
        new FileInputStream(publicKeyFile).read(encodedKey);
        X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(encodedKey);
        KeyFactory kf = KeyFactory.getInstance("RSA");
        PublicKey pk = kf.generatePublic(publicKeySpec);
        this.pkCipher.init(1, pk);
        CipherOutputStream os = new CipherOutputStream(new FileOutputStream(out), this.pkCipher);
        os.write(this.aesKey);
        os.close();
    }

    public void encrypt(File in, File out) throws IOException, InvalidKeyException {
        this.aesCipher.init(1, this.aeskeySpec);
        FileInputStream is = new FileInputStream(in);
        CipherOutputStream os = new CipherOutputStream(new FileOutputStream(out), this.aesCipher);
        this.copy(is, os);
        os.close();
    }

    public void decrypt(File in, File out) throws IOException, InvalidKeyException {
        this.aesCipher.init(2, this.aeskeySpec);
        CipherInputStream is = new CipherInputStream(new FileInputStream(in), this.aesCipher);
        FileOutputStream os = new FileOutputStream(out);
        this.copy(is, os);
        is.close();
        os.close();
    }

    private void copy(InputStream is, OutputStream os) throws IOException {
        int i;
        byte[] b = new byte[1024];
        while ((i = is.read(b)) != -1) {
            os.write(b, 0, i);
        }
    }
}

