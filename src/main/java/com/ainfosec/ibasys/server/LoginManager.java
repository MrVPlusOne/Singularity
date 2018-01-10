/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import com.ainfosec.ibasys.server.IBASysServer;
import com.ainfosec.ibasys.server.ImageMatcher;
import com.ainfosec.ibasys.server.LoginSession;
import com.ainfosec.ibasys.server.PacketParser;
import com.ainfosec.ibasys.server.SessionMonitor;
import com.ainfosec.ibasys.server.UserDatabase;
import patsyn.Debug;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Serializable;
import java.nio.channels.SocketChannel;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

public class LoginManager
implements Serializable {
    public static final int MINPCODE = 340000;
    public Map<String, LoginSession> sessions;
    public Map<Long, LoginSession> readysessions;
    public static ConcurrentLinkedQueue<ImageMatcher.ImageMatcherWorker> response;

    public LoginManager() {
        response = new ConcurrentLinkedQueue();
        this.sessions = new Hashtable<String, LoginSession>();
        this.readysessions = new Hashtable<Long, LoginSession>();
    }

    public LoginSession getSession(byte[] aesKey, byte[] iv) {
        String key = new String(aesKey);
        LoginSession session = this.sessions.get(key);
        if (session == null) {
            int matched = 0;
            int allmatch = 32;
            Set<String> keySet = this.sessions.keySet();
            Iterator<String> it = keySet.iterator();
            byte[] inbytes = key.getBytes();  //fixme: Possible timing side-channel
            while (it.hasNext()) {
                String next = it.next();
                byte[] bytes = next.getBytes();
                matched = 0;
                for (int i = 0; i < bytes.length; ++i) {
                    Debug.logLn("byte"+i+": "+bytes[i]);
                    if (inbytes[i] != bytes[i]) continue;
                    ++matched;
                }
                if (matched != allmatch) continue;
                session = this.sessions.get(next);
            }
        }
        if (session != null && session.iscomplete) {
            session = null;
        }
        if (session == null) {
            session = new LoginSession(key, aesKey, iv);
            this.sessions.put(key, session);
        }
        return session;
    }

    public void parsePacketPart(SocketChannel client, PacketParser parser, LoginSession session) {
        try {
            session.addData(parser.data);
            session.client = client;
            char[] basic = new char[16];
            char[] toCharArray = parser.user.toCharArray();
            int l = 0;
            for (int i = 0; i < toCharArray.length; ++i) {
                if (toCharArray[i] == '\u0000') continue;
                basic[i] = toCharArray[i]; //todo: if length(parser.user) > 16, it could break
                ++l;
            }
            char[] usernormal = Arrays.copyOf(basic, l);
            session.user = new String(usernormal);
            if (session.pcode == null) {
                session.pcode = this.getPcode(session.user);
            }
            byte[] data1 = session.getData();
            System.out.println("Processing user: " + session.user);
            byte e = data1[data1.length - 1];
            byte e1 = data1[data1.length - 2];
            byte e2 = data1[data1.length - 3];
            byte e3 = data1[data1.length - 4];
            byte e4 = data1[data1.length - 5];
            if (e == 0 && e1 == 0 && e2 == 0 && e3 == 0 && e4 == 0) {
                session.starttime = System.currentTimeMillis();
                this.readysessions.put(session.starttime, session);
                int totall = data1.length - session.start;
                for (int x = data1.length - 1; x >= 0 && data1[x] == 0; --x) {
                    --totall;
                }
                e = data1[totall - 1];
                data1 = Arrays.copyOfRange(data1, session.start, session.start + totall);
                byte[] first = session.tsafedatabuff.toByteArray();
                byte[] datanew = Arrays.copyOf(first, first.length + totall);
                System.arraycopy(data1, 0, datanew, first.length, totall);
                session.start += totall;
                session.client = client;
                session.setData(datanew);
                session.size = totall;
                SessionMonitor sm = new SessionMonitor(this.readysessions);
                System.out.println("lm.readysessions:addTaskForImageMatcherWorker: " + session.user);
                sm.addTaskForImageMatcherWorker();
            }
        }
        catch (IOException ex) {
            Logger.getLogger(LoginSession.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private int[] getPcode(String user) {
        String u = user.toString();
        int[] pcode = IBASysServer.db.get(u);
        return pcode;
    }
}

