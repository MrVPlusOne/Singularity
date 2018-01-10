/*
 * Decompiled with CFR 0_123.
 */
package com.ainfosec.ibasys.org.imgscalr;

import com.ainfosec.ibasys.org.imgscalr.Scalr;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.ImagingOpException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

public class AsyncScalr {
    public static final String THREAD_COUNT_PROPERTY_NAME = "imgscalr.async.threadCount";
    public static final int THREAD_COUNT = Integer.getInteger("imgscalr.async.threadCount", 2);
    protected static ExecutorService service;

    public static ExecutorService getService() {
        return service;
    }

    public static /* varargs */ Future<BufferedImage> apply(final BufferedImage src, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.apply(src, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> crop(final BufferedImage src, final int width, final int height, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.crop(src, width, height, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> crop(final BufferedImage src, final int x, final int y, final int width, final int height, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.crop(src, x, y, width, height, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> pad(final BufferedImage src, final int padding, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.pad(src, padding, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> pad(final BufferedImage src, final int padding, final Color color, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.pad(src, padding, color, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final int targetSize, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, targetSize, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Method scalingMethod, final int targetSize, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, scalingMethod, targetSize, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Mode resizeMode, final int targetSize, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, resizeMode, targetSize, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Method scalingMethod, final Scalr.Mode resizeMode, final int targetSize, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, scalingMethod, resizeMode, targetSize, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final int targetWidth, final int targetHeight, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, targetWidth, targetHeight, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Method scalingMethod, final int targetWidth, final int targetHeight, final BufferedImageOp ... ops) {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, scalingMethod, targetWidth, targetHeight, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Mode resizeMode, final int targetWidth, final int targetHeight, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, resizeMode, targetWidth, targetHeight, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> resize(final BufferedImage src, final Scalr.Method scalingMethod, final Scalr.Mode resizeMode, final int targetWidth, final int targetHeight, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.resize(src, scalingMethod, resizeMode, targetWidth, targetHeight, ops);
            }
        });
    }

    public static /* varargs */ Future<BufferedImage> rotate(final BufferedImage src, final Scalr.Rotation rotation, final BufferedImageOp ... ops) throws IllegalArgumentException, ImagingOpException {
        AsyncScalr.checkService();
        return service.submit(new Callable<BufferedImage>(){

            @Override
            public BufferedImage call() throws Exception {
                return Scalr.rotate(src, rotation, ops);
            }
        });
    }

    protected static ExecutorService createService() {
        return AsyncScalr.createService(new DefaultThreadFactory());
    }

    protected static ExecutorService createService(ThreadFactory factory) throws IllegalArgumentException {
        if (factory == null) {
            throw new IllegalArgumentException("factory cannot be null");
        }
        return Executors.newFixedThreadPool(THREAD_COUNT, factory);
    }

    protected static void checkService() {
        if (service == null || service.isShutdown() || service.isTerminated()) {
            service = AsyncScalr.createService();
        }
    }

    static {
        if (THREAD_COUNT < 1) {
            throw new RuntimeException("System property 'imgscalr.async.threadCount' set THREAD_COUNT to " + THREAD_COUNT + ", but THREAD_COUNT must be > 0.");
        }
    }

    protected static class ServerThreadFactory
    extends DefaultThreadFactory {
        protected ServerThreadFactory() {
        }

        @Override
        public Thread newThread(Runnable r) {
            Thread thread = super.newThread(r);
            thread.setDaemon(true);
            thread.setPriority(1);
            return thread;
        }
    }

    protected static class DefaultThreadFactory
    implements ThreadFactory {
        protected static final AtomicInteger poolNumber = new AtomicInteger(1);
        protected final ThreadGroup group;
        protected final AtomicInteger threadNumber = new AtomicInteger(1);
        protected final String namePrefix;

        DefaultThreadFactory() {
            SecurityManager manager = System.getSecurityManager();
            this.group = manager == null ? Thread.currentThread().getThreadGroup() : manager.getThreadGroup();
            this.namePrefix = "pool-" + poolNumber.getAndIncrement() + "-thread-";
        }

        @Override
        public Thread newThread(Runnable r) {
            Thread thread = new Thread(this.group, r, this.namePrefix + this.threadNumber.getAndIncrement(), 0);
            thread.setDaemon(false);
            thread.setPriority(5);
            return thread;
        }
    }

}

