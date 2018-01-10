/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.nio.channels.SocketChannel;

public class LoginSession
implements Comparable<LoginSession>,
Serializable {
    ByteArrayOutputStream databuff;
    String key;
    byte[] aeskey;
    long starttime;
    byte[] iv;
    SocketChannel client;
    String user;
    int[] pcode;
    int start;
    byte[] data;
    ByteArrayOutputStream tsafedatabuff = new ByteArrayOutputStream();
    boolean lockdata = false;
    boolean iscomplete = false;
    int size;

    public LoginSession(String key, byte[] aeskey, byte[] iv) {
        this.key = key;
        this.aeskey = aeskey;
        this.iv = iv;
        this.databuff = new ByteArrayOutputStream();
    }

    public void addData(byte[] data) throws IOException {
        if (this.databuff == null) {
            this.databuff = new ByteArrayOutputStream();
        }
        this.databuff.write(data);
    }

    public void setData(byte[] data) throws IOException {
        this.tsafedatabuff = new ByteArrayOutputStream();
        if (data != null) {
            this.data = data;
            this.tsafedatabuff.write(data);
        }
    }

    public byte[] getData() {
        byte[] toByteArray = this.databuff.toByteArray();
        return toByteArray;
    }

    @Override
    public int compareTo(LoginSession o) {
        long diff = o.starttime - this.starttime;
        if (diff > 0) {
            return 1;
        }
        if (diff == 0) {
            return 0;
        }
        return -1;
    }

    public String toString() {
        return this.databuff.toString();
    }

    public int hashCode() {
        return this.user.hashCode();
    }
}

