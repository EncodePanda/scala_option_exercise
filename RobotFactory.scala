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
    if(power <= wires * 2) Some(CyberBrain(Wires(wires), BrainCore(power))) else None

  def forgeLowerBody(legs: Int) = if(legs < 1 || legs > 100) None else Some(LowerBody(legs))

  def forgeUpperBody(arms: Int) = if(arms < 1) None else Some(UpperBody(arms))

  def forgeBody(lower: LowerBody, upper: UpperBody) = if(lower.numberOfLegs > upper.numberOfArms * 2) None else Some(Body(lower, upper))

}

object Factory {

  def apply(corePower: Int, wires: Int, numberOfLegs: Int, numberOfArms: Int) = {

    import Forgery._

    for {
      brain <- forgeBrain(wires, corePower)
      lower <- forgeLowerBody(numberOfLegs)
      upper <- forgeUpperBody(numberOfArms)
      body <- forgeBody(lower, upper)
    } yield(Robot(brain, body))
  }
}

object TestRun extends App {

  def testRun(maybeRobot: Option[Robot]) = maybeRobot match {
    case Some(robot) =>
      println(s"We've created: $robot")
      println(s"Robot thinks that... ${robot.brain.use}")
    case None =>
      println(s"Robot was not constructed, some invalid parameters...")
  }

  val robot1 = Factory(corePower = 10, wires = 5, numberOfLegs = 5, numberOfArms = 3)
  testRun(robot1)
  val robot2 = Factory(corePower = 10, wires = 2, numberOfLegs = 500, numberOfArms = 3)
  testRun(robot2)

}
