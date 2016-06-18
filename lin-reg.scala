/*
 * An implementation of regression (fit by gradient descent) in functional style.
 *
 * The main idea in this implementation is to lazily generate a stream of data
 * using gradient descent, and then select the first element of this stream
 * satisfying a convergence criterion.  The stream contains tuples (x, f(x)),
 * with each subsequent element of the stream calculated form the prior by
 * updating one gradient descent iteration.
 */
object RegressionApp {


type Point = Array[Double]
type Matrix = Array[Array[Double]]

/* One step of a gradient descent procedure. */
case class GradientDescentIteration(point: Point, value: Double)

/* A loss function to be minimized.  We need both the function itself to test
 * for convergence, and the functions gradient to perform the update.
 */
case class LossFunction(function: Point => Double, gradient: Point => Point)

/* An enumeration of supported loss functions. */
object LossFunctionName extends Enumeration {
    type LossFunctionName = Value
    val Gaussian, Binomial = Value
}

/*
 * Procedures for manipulating vectors and matricies.
 */
object LinAlg {

    def nRow(X: Matrix): Int = X.length
    def nCol(X: Matrix): Int = X(0).length

    def zeros(n: Int): Array[Double] = Array.fill[Double](n)(0) 

    def subtract(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x - y }
    def add(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x + y }
    def scalarMultiply(lambda: Double, v: Point): Point =
        v.map( (x: Double) => x * lambda )

    def dot(v1: Point, v2: Point): Double =
        v1.zip(v2).map { case (x: Double, y: Double) => x * y }.sum

    def transpose(X: Matrix): Matrix = Array.tabulate(nCol(X), nRow(X))((i, j) => X(j)(i))

    def matrixMultiply(X: Matrix, Y: Matrix): Matrix = {
        //var P = Array.tabulate(X.nRow, Y.nCol)( (x, y) => 0.0 )
        for (Xrow <- X) yield
            for (Ycol <- transpose(Y)) yield
                dot(Xrow, Ycol)
    }

    def matrixVectorMultiply(X: Matrix, y: Point): Point = {
        for(Xrow <- X) yield dot(Xrow, y)
    }

}


object Regression {

    import LossFunctionName._

    /* Fit a regression using gradient descent. */
    def fit(X: Matrix, y: Point, lf: LossFunctionName,
            tolerance: Double, learningRate: Double) = {
        val zeros = LinAlg.zeros(LinAlg.nCol(X))
        val lossFunction = makeLossFunction(X, y, lf)
        val descentStream = descend(learningRate, zeros, lossFunction)
        select(descentStream)(tolerance, _.value, _.point)
    }

    /*
     * Select the first value from a stream the differs in absolute value from
     * the previous value by less than a given tolerance.
     *
     * Two callbacks are used, one to cull from the stream what values to
     * compare within tolerance, and another to select from the stream when
     * within tolerance elements are found.
     */
    def select[A, B](s: Stream[A])(tolerance: Double,
                                   compareSelector: A => Double,
                                   valueSelector: A => B): B = {
        s match {
            case x #:: y #:: ms if math.abs(compareSelector(x) - compareSelector(y)) < tolerance
                => valueSelector(y)
            case x #:: y #:: ms => select(y #:: ms)(tolerance, compareSelector, valueSelector)
        }
    }

    /* Lazily generate a stream of gradient descent steps. */
    def descend(learningRate: Double, x0: Point, lf: LossFunction):
        Stream[GradientDescentIteration] = {

        GradientDescentIteration(x0, lf.function(x0)) #::
            descend(learningRate,
                    LinAlg.subtract(x0, LinAlg.scalarMultiply(learningRate, lf.gradient(x0))),
                    lf)

    }

    /* Construct a loss function of a given type given some data. */
    def makeLossFunction(X: Matrix, y: Point, lf: LossFunctionName) = lf match {
        case LossFunctionName.Gaussian => makeLossFunctionGaussian(X, y)
        case LossFunctionName.Binomial => makeLossFunctionBinomial(X, y)
    }

    /* Construct a gaussian loss function for linear regression.
     *   function: Sum of squared residuals.
     *   gradient: The transpose of the design matrix times the residuals.
     */
    def makeLossFunctionGaussian(X: Matrix, y: Point) = {
        def residuals(beta: Point): Point = {
            val predsResp = X.zip(y).map {
                case (row, resp) => (LinAlg.dot(row, beta), resp) }
            predsResp.map { case (pred, resp) => (pred - resp) }
        }
        def gradient(beta: Point): Point =
            LinAlg.matrixVectorMultiply(LinAlg.transpose(X), residuals(beta))
        def function(beta: Point): Double =
            residuals(beta).map( (x: Double) => x*x ).sum
        LossFunction(function, gradient)
    }

    /* Construct a binomial loss function for logistic regression.
     *   function: Binomial deviance.
     *   gradient: The transpose of the design matrix times the residuals.
     */
    def makeLossFunctionBinomial(X: Matrix, y: Point) = {
        def logit(x: Double): Double = 1 / (1 + math.exp(-x))
        def linPred(beta: Point): Point = LinAlg.matrixVectorMultiply(X, beta)
        def p(beta: Point): Point = linPred(beta).map(logit)
        def gradient(beta: Point): Point =
            LinAlg.matrixVectorMultiply(LinAlg.transpose(X), LinAlg.subtract(p(beta), y))
        def function(beta: Point): Double =
            LinAlg.dot(y, linPred(beta)) - 
                linPred(beta).map((x: Double) => math.log(1 + math.exp(x))).sum
        LossFunction(function, gradient)
    }

}


def main(args: Array[String]): Unit = {

    import scala.runtime.ScalaRunTime._
    def printArray[A](a: Array[A]): Unit = println(stringOf(a))

    def fitLinearRegression: Unit = {
        val X = Array(Array(1.0, 1.0, 1.0), Array(1.0, 2.0, 2.0), Array(1.0, 3.0, 2.0))
        val y = Array(2.0, 3.0, 4.0)

        val beta = Regression.fit(X, y, LossFunctionName.Gaussian, .001, .01)
        printArray(beta)
    }

    /* Each observation repeated with opposite observed response.  Estimated
     * parameters should be zero.
     */
    def fitLogisticRegression: Unit = {
        val X = Array(Array(1.0, 1.0, 1.0),
                      Array(1.0, 1.0, 1.0),
                      Array(1.0, 2.0, 2.0),
                      Array(1.0, 2.0, 2.0),
                      Array(1.0, 3.0, 2.0),
                      Array(1.0, 3.0, 2.0))
        val y = Array(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)

        val beta = Regression.fit(X, y, LossFunctionName.Binomial, .0001, .01)
        printArray(beta)
    }

    /* Simple example from wikipedia. */
    def fitLogisticExample: Unit = {
        val X = Array(Array(1, 0.50), Array(1, 0.75), Array(1, 1.00), Array(1, 1.25),
                      Array(1, 1.50), Array(1, 1.75), Array(1, 1.75), Array(1, 2.00),
                      Array(1, 2.25), Array(1, 2.50), Array(1, 2.75), Array(1, 3.00),
                      Array(1, 3.25), Array(1, 3.50), Array(1, 4.00), Array(1, 4.25),
                      Array(1, 4.50), Array(1, 4.75), Array(1, 5.00), Array(1, 5.50))
        val y = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0,
                      0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
        val beta = Regression.fit(X, y, LossFunctionName.Binomial, .00000001, .01)
        printArray(beta)
    }

    fitLinearRegression
    fitLogisticRegression
    fitLogisticExample

}

}
