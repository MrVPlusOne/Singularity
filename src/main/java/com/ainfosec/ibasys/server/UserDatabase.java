/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class UserDatabase {
    Map<String, int[]> pcodes = new HashMap<String, int[]>();
    Map<String, byte[]> tokencodes = new HashMap<String, byte[]>();

    public UserDatabase() throws IOException {
        this.loadUserFile("users.data");
    }

    public static void main(String[] args) throws IOException {
        UserDatabase userDatabase = new UserDatabase();
        int[] get = userDatabase.get("davidbowie");
        System.out.println("");
    }

    public int[] get(String key) {
        int[] pcode = this.pcodes.get(key);
        return pcode;
    }

    public byte[] gettoken(String key) {
        return this.tokencodes.get(key);
    }

    public void loadUserFile(String fname) throws IOException {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        InputStream is = classloader.getResourceAsStream("data/" + fname);
        BufferedReader br = new BufferedReader(new InputStreamReader(is, "UTF-8"));
        Throwable throwable = null;
        try {
            String line;
            while ((line = br.readLine()) != null) {
                int sIndexOf = line.indexOf(59);
                int lIndexOf = line.lastIndexOf(59);
                String user = line.substring(0, sIndexOf);
                String id = line.substring(sIndexOf + 1, lIndexOf);
                String sessionnum = line.substring(lIndexOf + 1, line.length());
                String[] split = id.split(",");
                byte[] unambytes = user.getBytes();
                byte[] fsizeuser = Arrays.copyOf(unambytes, 128);
                int[] pcode = new int[128];
                for (int i = 0; i < split.length; ++i) {
                    int b;
                    pcode[i] = b = Integer.parseInt(split[i]);
                }
                String[] splitsession = sessionnum.split(",");
                byte[] sessioncode = new byte[16];
                for (int i = 0; i < splitsession.length; ++i) {
                    byte b;
                    sessioncode[i] = b = Byte.parseByte(splitsession[i]);
                }
                String f = new String(fsizeuser);
                char[] toCharArray = user.toCharArray();
                this.pcodes.put(user, pcode);
                this.tokencodes.put(user, sessioncode);
            }
        }
        catch (Throwable line) {
            throwable = line;
            throw line;
        }
        finally {
            if (br != null) {
                if (throwable != null) {
                    try {
                        br.close();
                    }
                    catch (Throwable line) {
                        throwable.addSuppressed(line);
                    }
                } else {
                    br.close();
                }
            }
        }
    }
}

