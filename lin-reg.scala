/*
 * An implementation of regression (fit by gradient descent) in functional style.
 */

type Vector = Array[Double]
type Matrix = Array[Array[Double]]

case class GradientDescentIteration(
    point: Vector,
    value: Double,
)

case class LossFunction(
    function: Vector => Double,
    gradient: Vector => Vector
)

object LossFunctionName {
    sealed trait EnumVal
    case object Gaussian extends EnumVal
    val lossFunctions = Seq(Gaussian)
}

/*
 * Procedures for manipulating vectors and matricies.
 */
object LinAlg = {

    def nRow(X: Matrix): Int = X.length
    def nCol(X: Matrix): Int = X(0).length

    def zeroVector(n: Int): Array[Double] = Array.fill[Double](n)(0) 

    def subtract(v1: Vector, v2: Vector) = v1.zip(v2).map(_ - _)
    def add(v1: Vector, v2: Vector) = v1.zip(v2).map(_ - _)
    def scalarMult(lambda: Double, v: Vector) = v.map(_ * lambda)

    def dot(v1: Vector, v2: Vector) = v1.zip(v2).map(_ * _).sum 

}

/*
 * Regression
 */
object Regression {

    /* Fit a regression using gradient descent. */
    def fit(X: Matrix, y: Vector, lf: LossFunctionName,
            tolerance: Double, learningRate: Double) = {
        val zeros = LinAlg.zeroVector(LinAlg.nCol(X))
        select(descend(learningRate, zeros, descender(X, y, lf)),
               tolerance, _.value, _.gradient)
    }

    /*
     * Select the first value from a stream the differs in absolute value from
     * the previous value by less than a given tolerance.
     *
     * Two callbacks are used to cull from the stream what values to compare within
     * tolerance, and what value to select when within tolerance elements are found.
     */
    def select[A, B](s: Stream[A])(tolerance: Double,
                                   compareSelector: A => Double, valueSelector: A => B) = {
        m match {
            case x #:: y #:: ms if abs(compareSelector(x) - compareSelector(y)) < tolerance
                => valueSelector(y)
            case _ => select(y #:: ms)(tolerance, compareSelector, valueSelector)
        }
    }

    /* Generate a stream of gradient descent steps. */
    def descend(learningRate: Double, x0: Vector, lf: LossFunction):
        Stream[GradientDescentIteration] = {

        GradientDescentIteration(x0, lf.function(x0)) #::
            descend(learningRate,
                    LinAlg.subtract(x0, LinAlg.scalarMult(learningRate,
                                                          lf.gradient(x0))),
                    lf)

    }

    /* Construct a loss function of a given type given some data. */
    def descender(X: Matrix, y: Vector, lf: LossFunctionName) = lf match {
        case LossFunctionName.Gaussian => makeLossFunctionGaussian(X, y)
    }

    /* Construct a gaussian loss function for linear regression.
     *   function: sum of squared residuals.
     *   gradient: residuals.
     */
    def makeLossFunctionGaussian(X: Matrix, y: Vector) = {
        def gradient(beta: Vector): Vector = {
            val predsResp = X.zip(y).map((row, resp) => (LinAlg.dot(row, beta), resp))
            predsResp.map(_ - _)
        }
        def function(beta: Vector): Double = {
            val residuals = gradient(beta)
            residuals.map(_ * _).sum
        }
        LossFunction(function, gradient)
    }
}
