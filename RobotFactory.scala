case class Wires(howMany: Int)
case class BrainCore(power: Int) {
  def spin(wires: Wires): String =  {
    val ws = wires.howMany
    "// TODO remember to implement this before we go live to production!"
  }
}
case class CyberBrain(wires: Wires, core: BrainCore) {
  def use: String = core.spin(wires)
}
case class LowerBody(numberOfLegs: Int)
case class UpperBody(numberOfArms: Int)
case class Body(lower: LowerBody, upper: UpperBody)
case class Robot(brain: CyberBrain, body: Body)


object Forgery {

  def forgeBrain(wires: Int, power: Int) =
    if(power <= wires * 2) CyberBrain(Wires(wires), BrainCore(power)) else null

  def forgeLowerBody(legs: Int) = if(legs < 1 || legs > 100) null else LowerBody(legs)

  def forgeUpperBody(arms: Int) = if(arms < 1) null else UpperBody(arms)

  def forgeBody(lower: LowerBody, upper: UpperBody) = if(lower.numberOfLegs > upper.numberOfArms * 2) null else Body(lower, upper)

}

object Factory {

  def apply(corePower: Int, wires: Int, numberOfLegs: Int, numberOfArms: Int) = {

    import Forgery._

    new Robot(forgeBrain(wires, corePower), forgeBody(forgeLowerBody(numberOfLegs), forgeUpperBody(numberOfArms)))

  }


}

object TestRun extends App {

  def testRun(robot: Robot) {
    println(s"We've created: $robot")
    println(s"Robot thinks that... ${robot.brain.use}")
  }

  val robot1 = Factory(corePower = 10, wires = 5, numberOfLegs = 5, numberOfArms = 3)
  testRun(robot1)
  val robot2 = Factory(corePower = 10, wires = 2, numberOfLegs = 500, numberOfArms = 3)
  testRun(robot2)

}
