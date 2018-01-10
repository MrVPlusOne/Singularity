/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import com.ainfosec.ibasys.server.ImageMatcher;
import com.ainfosec.ibasys.server.LoginSession;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.Serializable;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SessionMonitor
implements Serializable {
    List<LoginSession> pendingsessions;
    Map<Long, LoginSession> sessions;
    Map<String, ImageMatcher> threadstates;
    static long lasttime = 0;

    public SessionMonitor(Map<Long, LoginSession> sessions) {
        this.sessions = sessions;
        this.threadstates = new Hashtable<String, ImageMatcher>();
    }

    public void addTaskForImageMatcherWorker() {
        this.pendingsessions = new ArrayList<LoginSession>();
        System.out.println("begin processing sessions, pending sessions:" + this.sessions.keySet().size());
        Collection<LoginSession> values = this.sessions.values();
        for (LoginSession next : values) {
            if (next.starttime <= lasttime) continue;
            this.pendingsessions.add(next);
        }
        System.out.println("sessions selected");
        Collections.sort(this.pendingsessions);
        for (LoginSession next : this.pendingsessions) {
            if (next.iscomplete) continue;
            ImageMatcher cstate = ImageMatcher.init(next.key);
            System.out.println("lauch session:" + next.user + ":" + next.starttime);
            ImageMatcher.startImageMatcherWorker(next, cstate, next.key, next.tsafedatabuff.toByteArray(), next.iv, next.aeskey, next.starttime, next.client, next.user, next.pcode);
            LoginSession loginSession = this.sessions.remove(next.starttime);
        }
        System.out.println("finished selecting sessions");
        lasttime = System.currentTimeMillis();
    }
}

