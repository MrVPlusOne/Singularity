/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import com.ainfosec.ibasys.server.FileEncryption;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.security.GeneralSecurityException;
import java.util.Arrays;

public class PacketParser {
    public byte[] iv;
    public byte[] data;
    public byte[] aesKey;
    public String user;

    public void parse(byte[] packet) throws GeneralSecurityException, IOException {
        if (packet.length == 10272) {
            byte[] key = Arrays.copyOfRange(packet, 0, 256);
            this.iv = Arrays.copyOfRange(packet, 256, 272);
            this.data = Arrays.copyOfRange(packet, 272, packet.length);
            FileEncryption fe = new FileEncryption();
            ClassLoader classLoader = this.getClass().getClassLoader();
            File pkeyfile = new File("private.der");
            this.aesKey = fe.loadKey(key, pkeyfile);
            byte[] userb = fe.loadUser(key, pkeyfile);
            this.user = new String(userb);
        } else {
            System.out.println("not 10k");
        }
    }
}

