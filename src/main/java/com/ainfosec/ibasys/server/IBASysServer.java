/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.server;

import com.ainfosec.ibasys.client.AESUtil;
import com.ainfosec.ibasys.org.imgscalr.Scalr;
import com.ainfosec.ibasys.server.ImageMatcher;
import com.ainfosec.ibasys.server.LoginManager;
import com.ainfosec.ibasys.server.LoginSession;
import com.ainfosec.ibasys.server.PacketParser;
import com.ainfosec.ibasys.server.UserDatabase;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;

public class IBASysServer {
    public static UserDatabase db;
    public LoginManager lm;

    public IBASysServer() throws IOException {
        db = new UserDatabase();
        this.lm = new LoginManager();
    }

    private static byte[] makeAESIV() {
        byte[] iv = new byte[16];
        SecureRandom random = new SecureRandom();
        random.nextBytes(iv);
        return iv;
    }

    /*
     * Unable to fully structure code
     * Enabled aggressive block sorting
     * Enabled unnecessary exception pruning
     * Enabled aggressive exception aggregation
     * Lifted jumps to return sites
     */
    public static void init(String[] args) throws IOException {
        Selector selector = Selector.open();
        ServerSocketChannel serverSocket = ServerSocketChannel.open();
        serverSocket.bind(new InetSocketAddress(5453));
        serverSocket.configureBlocking(false);
        serverSocket.register(selector, 16);
        ByteBuffer buffer = ByteBuffer.allocate(10272);
        IBASysServer svr = new IBASysServer();

        while(true) {
            while(true) {
                ImageMatcher.ImageMatcherWorker result = LoginManager.response.poll();
                if (result != null) {
                    try {
                        ByteArrayOutputStream databuff = new ByteArrayOutputStream();
                        System.out.println("sending");
                        databuff.write("result".getBytes());

                        byte[] copyOf;
                        try {
                            byte[] akey = result.aeskey;
                            if (!result.success) {
                                System.out.println("prepare !success");
                                copyOf = svr.makeReturnAESKey();
                            } else {
                                copyOf = db.gettoken(result.user);
                            }

                            byte[] encrypted = AESUtil.encrypt(result.aeskey, new byte[16], copyOf);
                            System.out.println("send success");
                            databuff.write(encrypted);
                            databuff.write(result.user.getBytes());
                        } catch (Exception var14) {
                            result.error.append(var14.toString());
                        }

                        if (result.success) {
                            String eString = result.error.toString();
                            if (!eString.isEmpty()) {
                                copyOf = Arrays.copyOf(eString.getBytes(), 46);
                                System.out.println("send error");
                                databuff.write(copyOf);
                            }
                        }

                        ByteBuffer bbuf = ByteBuffer.allocate(databuff.size());
                        bbuf = ByteBuffer.wrap(databuff.toByteArray());
                        System.out.println("Sending result to user:" + result.user);
                        result.state.client.write(bbuf);
                        result.state.client.close();
                        bbuf.clear();
                        String k = new String(result.aeskey);
                        LoginSession get = (LoginSession)svr.lm.sessions.get(k);
                        get.iscomplete = true;
                        svr.lm.sessions.remove(k);
                        ImageMatcher.threadstates.remove(result.state.hkey);
                        System.out.println("sent");
                    } catch (IOException var15) {
                        System.out.println("queue checker loop exception:" + var15.getMessage());
                    }
                }

                selector.selectNow();
                Set selectedKeys = selector.selectedKeys();
                Iterator iter = selectedKeys.iterator();
                if (!iter.hasNext()) {
                    try {
//                        System.out.print(".");
                        Thread.sleep(1000L);
                    } catch (InterruptedException var16) {
                        Logger.getLogger(IBASysServer.class.getName()).log(Level.SEVERE, (String)null, var16);
                    }
                } else {
                    while(iter.hasNext()) {
                        System.out.println("selected");
                        SelectionKey key = (SelectionKey)iter.next();
                        iter.remove();
                        key.attachment();

                        try {
                            SocketChannel client;
                            if (key.isAcceptable()) {
                                client = serverSocket.accept();
                                System.out.println("selecting client");
                                client.configureBlocking(false);
                                client.register(selector, 1);
                            }

                            if (key.isReadable()) {
                                client = (SocketChannel)key.channel();
                                System.out.println("waiting to accept");
                                int read = client.read(buffer);
                                System.out.println("readfrombuff:" + read);
                                if (read == -1) {
                                    buffer = ByteBuffer.allocate(10272);
                                    key.cancel();
                                } else {
                                    LoginSession session = null;
                                    Object var12 = null;

                                    try {
                                        buffer.flip();
                                        byte[] array = buffer.array();
                                        PacketParser parser = new PacketParser();
                                        parser.parse(array);
                                        session = svr.lm.getSession(parser.aesKey, parser.iv);
                                        svr.lm.parsePacketPart(client, parser, session);
                                    } catch (Exception var17) {
                                        Logger.getLogger(IBASysServer.class.getName()).log(Level.SEVERE, (String)null, var17);
                                        Scalr.log(1, "ok", new Object[]{session});
                                    }
                                }
                            }
                        } catch (IOException var18) {
                            System.out.println("selector loop exception:" + var18.getMessage());
                            key.cancel();
                        }
                    }
                }
            }
        }
    }

    public static Process start() throws IOException, InterruptedException {
        String javaHome = System.getProperty("java.home");
        String javaBin = javaHome + File.separator + "bin" + File.separator + "java";
        String classpath = System.getProperty("java.class.path");
        String className = IBASysServer.class.getCanonicalName();
        ProcessBuilder builder = new ProcessBuilder(javaBin, "-cp", classpath, className);
        return builder.start();
    }

    public byte[] makeReturnAESKey() throws NoSuchAlgorithmException {
        KeyGenerator kgen = KeyGenerator.getInstance("AES");
        kgen.init(128);
        SecretKey key = kgen.generateKey();
        byte[] encoded = key.getEncoded();
        return encoded;
    }

    public static byte[] toBytes(byte[] input) {
        byte[] toReturn = new byte[input.length / 8];
        for (int entry = 0; entry < toReturn.length; ++entry) {
            for (int bit = 0; bit < 8; ++bit) {
                if (input[entry * 8 + bit] != 1) continue;
                byte[] arrby = toReturn;
                int n = entry;
                arrby[n] = (byte)(arrby[n] | 128 >> bit);
            }
        }
        return toReturn;
    }
}

