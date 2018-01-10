/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.client;

import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

public class AESUtil {
    public static byte[] encrypt(byte[] key, byte[] initVector, byte[] value) {
        try {
            IvParameterSpec iv = null;
            if (initVector != null) {
                iv = new IvParameterSpec(initVector);
            }
            SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
            if (iv != null) {
                cipher.init(1, (Key)skeySpec, iv);
            } else {
                cipher.init(1, skeySpec);
            }
            byte[] encrypted = cipher.doFinal(value);
            return encrypted;
        }
        catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }
    }

    public static byte[] decrypt(byte[] key, byte[] initVector, byte[] value) {
        try {
            IvParameterSpec iv = null;
            if (initVector != null) {
                iv = new IvParameterSpec(initVector);
            }
            SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
            if (iv != null) {
                cipher.init(2, (Key)skeySpec, iv);
            } else {
                cipher.init(2, skeySpec);
            }
            byte[] original = cipher.doFinal(value);
            return original;
        }
        catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }
    }
}

