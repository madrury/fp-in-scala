/*
 * An implementation of regression (fit by gradient descent) in functional style.
 */

type Point = Array[Double]
type Matrix = Array[Array[Double]]

case class GradientDescentIteration(
    point: Point,
    value: Double
)

case class LossFunction(
    function: Point => Double,
    gradient: Point => Point
)

object LossFunctionName extends Enumeration {
    type LossFunctionName = Value
    val Gaussian = Value
}

/*
 * Procedures for manipulating vectors and matricies.
 */
object LinAlg {

    def nRow(X: Matrix): Int = X.length
    def nCol(X: Matrix): Int = X(0).length

    def zeroPoint(n: Int): Array[Double] = Array.fill[Double](n)(0) 

    def subtract(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x - y }
    def add(v1: Point, v2: Point): Point =
        v1.zip(v2).map { case (x: Double, y: Double) => x + y }
    def scalarMult(lambda: Double, v: Point): Point =
        v.map( (x: Double) => x * lambda )

    def dot(v1: Point, v2: Point): Double =
        v1.zip(v2).map { case (x: Double, y: Double) => x * y }.sum

}

/*
 * Regression
 */
object Regression {
    import LossFunctionName._

    /* Fit a regression using gradient descent. */
    def fit(X: Matrix, y: Point, lf: LossFunctionName,
            tolerance: Double, learningRate: Double) = {
        val zeros = LinAlg.zeroPoint(LinAlg.nCol(X))
        select(descend(learningRate, zeros, descender(X, y, lf)))(
               tolerance, _.value, _.point)
    }

    /*
     * Select the first value from a stream the differs in absolute value from
     * the previous value by less than a given tolerance.
     *
     * Two callbacks are used to cull from the stream what values to compare within
     * tolerance, and what value to select when within tolerance elements are found.
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

    /* Generate a stream of gradient descent steps. */
    def descend(learningRate: Double, x0: Point, lf: LossFunction):
        Stream[GradientDescentIteration] = {

        GradientDescentIteration(x0, lf.function(x0)) #::
            descend(learningRate,
                    LinAlg.subtract(x0, LinAlg.scalarMult(learningRate,
                                                          lf.gradient(x0))),
                    lf)

    }

    /* Construct a loss function of a given type given some data. */
    def descender(X: Matrix, y: Point, lf: LossFunctionName) = lf match {
        case LossFunctionName.Gaussian => makeLossFunctionGaussian(X, y)
    }

    /* Construct a gaussian loss function for linear regression.
     *   function: sum of squared residuals.
     *   gradient: residuals.
     */
    def makeLossFunctionGaussian(X: Matrix, y: Point) = {
        def gradient(beta: Point): Point = {
            val predsResp = X.zip(y).map {
                case(row, resp) => (LinAlg.dot(row, beta), resp) }
            predsResp.map { case (preds, resp) => resp - preds }
        }
        def function(beta: Point): Double = {
            val residuals = gradient(beta)
            residuals.map( (x: Double) => x*x ).sum
        }
        LossFunction(function, gradient)
    }
}
