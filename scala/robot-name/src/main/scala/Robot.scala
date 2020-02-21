import scala.util.Random
import scala.collection.mutable.{Set => mSet}

object RobotFactory {
    val process : mSet[Int] = mSet()
    val letters = 'A' to 'Z'
    val maxNr = 1000
    val rest : mSet[Int] = mSet() ++ (0 until 676000 ).toSet

    def apply(int: Int): RobotName = {
        val l1 = int / (RobotFactory.maxNr*RobotFactory.letters.size)
        val l2 = (int - l1 * (RobotFactory.maxNr*RobotFactory.letters.size)) / RobotFactory.maxNr
        val nr = int - l1 * (RobotFactory.maxNr*RobotFactory.letters.size) - l2 * RobotFactory.maxNr
        new RobotName(l1,l2,nr)
  }
}

case class RobotName (l1n:Int, l2n:Int, nr:Int) {    
    def id = (nr%RobotFactory.maxNr) + RobotFactory.maxNr * l2n  +  RobotFactory.maxNr*RobotFactory.letters.size * l1n
    override def toString = "%s%s%03d".format(RobotFactory.letters(l1n),  RobotFactory.letters(l2n), nr)
}
        
class Robot() {
    val r = new Random()
    private var _name = ""
    def name = _name

    if (_name == "") reset() 

    def getRandomRobotName = RobotName(r.nextInt(RobotFactory.letters.size), r.nextInt(RobotFactory.letters.size), r.nextInt(RobotFactory.maxNr))

    def reset() = {
        var robot = getRandomRobotName
            
        while(RobotFactory.process.contains(robot.id) && RobotFactory.rest.size >0){
            if(RobotFactory.process.size < 10000){
                 robot = getRandomRobotName
            }
            else{
                robot = RobotFactory(RobotFactory.rest.head)
            }
        }
        RobotFactory.process.add(robot.id)
        RobotFactory.rest.remove(robot.id)
        _name = robot.toString
    }
}