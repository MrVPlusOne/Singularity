package singularity;

import java.util.Collection;

import org.apache.commons.math3.analysis.ParametricUnivariateFunction;
import org.apache.commons.math3.fitting.AbstractCurveFitter;
import org.apache.commons.math3.fitting.WeightedObservedPoint;
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresBuilder;
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresProblem;
import org.apache.commons.math3.linear.DiagonalMatrix;

public class ModifiedCurveFitter extends AbstractCurveFitter {
    /** Function to fit. */
    private final ParametricUnivariateFunction function;
    /** Initial guess for the parameters. */
    private final double[] initialGuess;
    /** Maximum number of iterations of the optimization algorithm. */
    private final int maxIter;

    /**
     * Contructor used by the factory methods.
     *
     * @param function Function to fit.
     * @param initialGuess Initial guess. Cannot be {@code null}. Its length must
     * be consistent with the number of parameters of the {@code function} to fit.
     * @param maxIter Maximum number of iterations of the optimization algorithm.
     */
    private ModifiedCurveFitter(ParametricUnivariateFunction function,
                              double[] initialGuess,
                              int maxIter) {
        this.function = function;
        this.initialGuess = initialGuess;
        this.maxIter = maxIter;
    }

    /**
     * Creates a curve fitter.
     * The maximum number of iterations of the optimization algorithm is set
     * to {@link Integer#MAX_VALUE}.
     *
     * @param f Function to fit.
     * @param start Initial guess for the parameters.  Cannot be {@code null}.
     * Its length must be consistent with the number of parameters of the
     * function to fit.
     * @return a curve fitter.
     *
     * @see #withStartPoint(double[])
     * @see #withMaxIterations(int)
     */
    public static ModifiedCurveFitter create(ParametricUnivariateFunction f,
                                           double[] start) {
        return new ModifiedCurveFitter(f, start, Integer.MAX_VALUE);
    }

    /**
     * Configure the start point (initial guess).
     * @param newStart new start point (initial guess)
     * @return a new instance.
     */
    public ModifiedCurveFitter withStartPoint(double[] newStart) {
        return new ModifiedCurveFitter(function,
                newStart.clone(),
                maxIter);
    }

    /**
     * Configure the maximum number of iterations.
     * @param newMaxIter maximum number of iterations
     * @return a new instance.
     */
    public ModifiedCurveFitter withMaxIterations(int newMaxIter) {
        return new ModifiedCurveFitter(function,
                initialGuess,
                newMaxIter);
    }

    /** {@inheritDoc} */
    @Override
    protected LeastSquaresProblem getProblem(Collection<WeightedObservedPoint> observations) {
        // Prepare least-squares problem.
        final int len = observations.size();
        final double[] target  = new double[len];
        final double[] weights = new double[len];

        int count = 0;
        for (WeightedObservedPoint obs : observations) {
            target[count]  = obs.getY();
            weights[count] = obs.getWeight();
            ++count;
        }

        final AbstractCurveFitter.TheoreticalValuesFunction model
                = new AbstractCurveFitter.TheoreticalValuesFunction(function,
                observations);

        // Create an optimizer for fitting the curve to the observed points.
        return new LeastSquaresBuilder().
                maxEvaluations(Integer.MAX_VALUE).
                maxIterations(maxIter).
                start(initialGuess).
                target(target).
                weight(new DiagonalMatrix(weights)).
                model(model.getModelFunction(), model.getModelFunctionJacobian()).
                checker((iteration, previous, current) -> iteration >= maxIter).
                build();
    }

}
