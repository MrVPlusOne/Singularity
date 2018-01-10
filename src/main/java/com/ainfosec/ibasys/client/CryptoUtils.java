/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.client;

import com.ainfosec.ibasys.client.CryptoException;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Reader;
import java.net.URLEncoder;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;

public class CryptoUtils {
    private static final String ALGORITHM = "AES";
    private static final String TRANSFORMATION = "AES";

    public static void encrypt(String key, String inputFile, File outputFile) throws CryptoException {
        CryptoUtils.enCrypto(key, inputFile, outputFile);
    }

    public static String decrypt(String key, File inputFile, String outputFile) throws CryptoException {
        return CryptoUtils.deCrypto(key, inputFile);
    }

    public static String strencrypt(String key, String inputFile) throws CryptoException {
        return CryptoUtils.strCrypto(1, key, inputFile);
    }

    public static String strdecrypt(String key, String inputFile) throws CryptoException {
        return CryptoUtils.strCrypto(2, key, inputFile);
    }

    private static byte[] byteCrypto(int mode, byte[] key, byte[] data) throws CryptoException {
        try {
            SecretKeySpec secretKey = new SecretKeySpec(key, "AES");
            Cipher cipher = Cipher.getInstance("AES");
            cipher.init(mode, secretKey);
            byte[] outputBytes = cipher.doFinal(data);
            return outputBytes;
        }
        catch (NoSuchPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (NoSuchAlgorithmException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (InvalidKeyException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (BadPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IllegalBlockSizeException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
    }

    private static String strCrypto(int mode, String key, String inputFile) throws CryptoException {
        try {
            SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "AES");
            Cipher cipher = Cipher.getInstance("AES");
            cipher.init(mode, secretKey);
            byte[] outputBytes = cipher.doFinal(inputFile.getBytes("ISO-8859-1"));
            String string = new String(outputBytes, "ISO-8859-1");
            String encode = URLEncoder.encode(string, "UTF-8");
            return encode;
        }
        catch (NoSuchPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (NoSuchAlgorithmException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (InvalidKeyException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (BadPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IllegalBlockSizeException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IOException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
    }

    private static void enCrypto(String key, String inputFile, File outputFile) throws CryptoException {
        try {
            SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "AES");
            Cipher cipher = Cipher.getInstance("AES");
            cipher.init(1, secretKey);
            ByteArrayInputStream inputStream = new ByteArrayInputStream(inputFile.getBytes("ISO-8859-1"));
            byte[] inputBytes = new byte[inputFile.length()];
            inputStream.read(inputBytes);
            byte[] outputBytes = cipher.doFinal(inputBytes);
            FileOutputStream outputStream = new FileOutputStream(outputFile);
            outputStream.write(outputBytes);
            inputStream.close();
            outputStream.close();
        }
        catch (NoSuchPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (NoSuchAlgorithmException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (InvalidKeyException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (BadPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IllegalBlockSizeException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IOException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
    }

    private static String deCrypto(String key, File inputFile) throws CryptoException {
        try {
            SecretKeySpec secretKey = new SecretKeySpec(key.getBytes(), "AES");
            Cipher cipher = Cipher.getInstance("AES");
            cipher.init(2, secretKey);
            FileInputStream inputStream = new FileInputStream(inputFile);
            byte[] inputBytes = new byte[(int)inputFile.length()];
            inputStream.read(inputBytes);
            byte[] outputBytes = cipher.doFinal(inputBytes);
            String outputFile = new String(outputBytes, "ISO-8859-1");
            inputStream.close();
            return outputFile;
        }
        catch (NoSuchPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (NoSuchAlgorithmException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (InvalidKeyException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (BadPaddingException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IllegalBlockSizeException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
        catch (IOException ex) {
            throw new CryptoException("Error encrypting/decrypting file", ex);
        }
    }

    public static String read(String fstr) throws IOException, InterruptedException {
        String readLine = "0";
        File f1 = new File("dirs/mydir2/" + fstr);
        if (f1.exists()) {
            FileReader fw = new FileReader(f1.getAbsoluteFile());
            BufferedReader bw = new BufferedReader(fw);
            readLine = bw.readLine();
            bw.close();
            System.out.println(f1.getName());
        }
        return readLine;
    }

    public static void write(String fileenc, String strencrypt) {
        try {
            Files.write(Paths.get(fileenc, new String[0]), strencrypt.getBytes(), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
        }
        catch (IOException iOException) {
            // empty catch block
        }
    }
}

