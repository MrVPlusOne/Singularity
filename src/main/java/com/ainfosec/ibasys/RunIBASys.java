/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys;

import com.ainfosec.ibasys.client.IBASysClient;
import com.ainfosec.ibasys.server.IBASysServer;
import java.io.File;
import java.io.PrintStream;

public class RunIBASys {
    public static void main(String[] args) throws Exception {
        RunIBASys.clean("logs");
        if (args.length > 1) {
            if (args.length != 4) {
                System.out.println("client usage:java -jar IBASys.jar client %serveraddress% %username% %imagename%");
                System.exit(2);
            }
            if (args[0].startsWith("client")) {
                IBASysClient.main(args);
            }
        } else {
            System.out.println("Starting IBASys server");
            IBASysServer.init(null);
        }
    }

    public static void clean(String dir) {
        String[] entries;
        File dirf = new File(dir);
        dirf.mkdir();
        for (String s : entries = dirf.list()) {
            File currentFile = new File(dirf.getPath(), s);
            currentFile.delete();
        }
    }
}

