/*
 * An implementation of regression (fit by gradient descent) in functional style.
 */

type Point = Array[Double]
type Vector = Array[Double]

case class GradientDescentIteration(
    point: Point
    value: Double,
)

case class LossFunction(
    function: Point => Double,
    gradient: Point => Vector
)

object LossFunctions {
    sealed trait EnumVal
    case object Gaussian extends EnumVal
    val lossFunctions = Seq(Gaussian)
}

object Regression {

}
