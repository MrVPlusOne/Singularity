/*
 * Decompiled with CFR 0_123.
 *
 * Could not load the following classes:
 *  com.bbn.MatrixRoutines
 *  org.apache.commons.math3.linear.Array2DRowRealMatrix
 *  org.apache.commons.math3.linear.ArrayRealVector
 *  org.apache.commons.math3.linear.RealMatrix
 *  org.apache.commons.math3.linear.RealVector
 */
package com.bbn;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Random;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.ArrayRealVector;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;

/*
 * Exception performing whole class analysis ignored.
 */
public class MatrixRoutines {
    public static final double EPSILON = 1.0E-8;

    public MatrixRoutines() {
    }

    public static RealVector makeVectorStochastic(RealVector v) {
        int n = v.getDimension();
        RealVector ones = new ArrayRealVector(n).mapAddToSelf(1.0);
        double sum = v.dotProduct(ones);
        if (sum == 0.0) {
            return new ArrayRealVector(n, 1.0 / (double)n);
        }
        return v.mapDivide(sum);
    }

    public static RealMatrix makeMatrixStochastic(RealMatrix M) {
        int n = M.getRowDimension();
        for (int i = 0; i < n; ++i) {
            RealVector v = M.getColumnVector(i);
            v = MatrixRoutines.makeVectorStochastic(v);
            M.setColumnVector(i, v);
        }
        return M;
    }

    public static RealVector addDampingFactor(RealVector v, double factor) {
        int n = v.getDimension();
        RealVector u = v.mapMultiply(factor);
        u.mapAddToSelf((1.0 - factor) / (double)n);
        return u;
    }

    public static RealMatrix addDampingFactor(RealMatrix M, double factor) {
        int n = M.getRowDimension();
        for (int i = 0; i < n; ++i) {
            RealVector v = M.getColumnVector(i);
            v = MatrixRoutines.addDampingFactor(v, factor);
            M.setColumnVector(i, v);
        }
        return M;
    }

    public static RealVector processVector(RealVector v, double factor) {
        v = MatrixRoutines.makeVectorStochastic(v);
        v = MatrixRoutines.addDampingFactor(v, factor);
        return v;
    }

    public static RealMatrix processMatrix(RealMatrix M, double factor) {
        M = MatrixRoutines.makeMatrixStochastic(M);
        M = MatrixRoutines.addDampingFactor(M, factor);
        return M;
    }

    public static RealMatrix concatenateColumn(RealMatrix M, RealVector u) {
        if (M.getRowDimension() == u.getDimension()) {
            M.setColumnVector(M.getColumnDimension() - 1, u);
            return M;
        }
        if (M.getRowDimension() == u.getDimension() - 1) {
            Array2DRowRealMatrix N = new Array2DRowRealMatrix(M.getRowDimension() + 1, M.getColumnDimension() + 1);
            N.setSubMatrix(M.getData(), 0, 0);
            N.setColumnVector(M.getColumnDimension(), u);
            return N;
        }
        return M;
    }

    public static RealVector estimateStationaryVector(RealMatrix M, double precision) {
        int m = M.getRowDimension();
        RealVector b = MatrixRoutines.randomStochasticVector(m);
        return MatrixRoutines.estimateStationaryVector(M, b, precision);
    }

    //todo: sum(@v) should be 1
    public static RealVector estimateStationaryVector(RealMatrix M, RealVector v, double precision) {
        int count = 0;
        int m = M.getRowDimension();
        RealVector b = v.unitVector();
        RealVector c = M.operate(b).unitVector();
        while (b.getNorm() > 1.0E-8 && Math.abs(b.getDistance(c) / b.getNorm()) > precision) {
            b = c;
            c = M.operate(b).unitVector();
            ++count;
        }
        return b;
    }

    public static RealVector loadVectorFromFile(File file) throws IOException {
        FileInputStream fis = new FileInputStream(file);
        DataInputStream dis = new DataInputStream(fis);
        int n = dis.readInt();
        ArrayRealVector v = new ArrayRealVector(n);
        for (int i = 0; i < n; ++i) {
            v.setEntry(i, dis.readDouble());
        }
        dis.close();
        fis.close();
        return v;
    }

    public static RealVector randomStochasticVector(int m) {
        Random rand = new Random();
        double[] x = new double[m];
        double sum = 0.0;
        for (int i = 0; i < m; ++i) {
            x[i] = rand.nextDouble();
            sum += x[i];
        }
        ArrayRealVector v = new ArrayRealVector(x);
        if (sum == 0.0) {
            return v;
        }
        v.mapMultiplyToSelf(1.0 / sum);
        return v;
    }
}

