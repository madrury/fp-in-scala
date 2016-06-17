/*
 * An implementation of regression (fit by gradient descent) in functional style.
 */

type Point = Array[Double]

type Vector = Array[Double]
type Matrix = Array[Array[Double]]

case class GradientDescentIteration(
    point: Point
    value: Double,
)

case class LossFunction(
    function: Point => Double,
    gradient: Point => Vector
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

}

/*
 * Regression
 */
object Regression {

    def fit(X: Matrix, y: Vector, lf: LossFunctionName,
            tolerance: Double, learningRate: Double) = {

        val zeros = LinAlg.zeroVector(LinAlg.nCol(X))
        select(descend(learningRate, zeros, descender(X, y, lf)),
               tolerance, _.value, _.gradient)

    }

    /*
     * Select the first value from a stream the differs in absolute value from
     * the previous value by less than a given tolerance.
     */
    def select[A, B](s: Stream[A])(tolerance: Double,
                                   compareSelector: A => Double, valueSelector: A => B) = {
        m match {
            case x #:: y #:: ms if abs(compareSelector(x) - compareSelector(y)) < tolerance
                => valueSelector(y)
            case _ => select(y #:: ms)(tolerance, compareSelector, valueSelector)
        }
    }

    def descend(learningRate: Double, x0: Point, lf: LossFunction):
        Stream[GradientDescentIteration] = {

        GradientDescentIteration(x0, lf.function(x0)) #::
            descend(learningRate,
                    LinAlg.subtract(x0, LinAlg.scalarMult(learningRate,
                                                          lf.gradient(x0))),
                    lf)

    }



}
