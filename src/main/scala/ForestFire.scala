import scala.util.Random
import java.io.PrintWriter
import java.io.File

trait Cell

case object Empty extends Cell{
  override def toString: String = "\"E\""
}
case object Tree extends Cell{
  override def toString: String = "\"T\""
}
case object Burning extends Cell{
  override def toString: String = "\"F\""
}
case object Burn extends Cell{
  override def toString: String = "\"B\""
}
case object Water extends Cell{
  override def toString: String = "\"W\""
}

class ForestFire(width:Int = 15, height:Int = 15, treeDensity:Double = 0.8, waterDensity:Double = 0.2, fireStartChance:Double = 0.05, treePropagationChance:Double = 0.1, firePropagationChance:Double = 0.5) {
  val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))
  val rnd = new Random()
  def T =  (fireStartChance + firePropagationChance) / treePropagationChance

  var grid: Array[Array[Cell]] = Array.fill(height, width) {Empty}
  for (y <- 0 until height; x <- 0 until width) {
    if (rnd.nextDouble() < treeDensity) grid(y)(x) = Tree
    else if (hasWaterNeighbor(x,y) && rnd.nextDouble() < waterDensity * 6)  grid(y)(x) = Water
    else if ( rnd.nextDouble() < waterDensity) grid(y)(x) = Water
    else grid(y)(x) = Empty
  }

  for (y <- 0 until height; x <- 0 until width) {
    if (grid(y)(x) == Tree && rnd.nextDouble() < fireStartChance)
      grid(y)(x) = Burning
  }

  def printGrid(): Unit = {
    for (row <- grid) {
      println(row.map {
        case Empty   => "⬛"
        case Tree    => "🌲"
        case Burning => "🔥"
        case Water   => "💧"
        case Burn    => "🪵"
      }.mkString)
    }
    println("\n")

  }

  def step(): Unit = {
    val newGrid = grid.map(_.clone())
    for (y <- 0 until height; x <- 0 until width) {
      grid(y)(x) match {
        case Tree =>
          val rdm = rnd.nextDouble()
          if ( hasWaterNeighbor(x,y)){
            if (hasBurningNeighbor(x, y) && rdm < firePropagationChance/1.5) newGrid(y)(x) = Burning
          } else {
            if (hasBurningNeighbor(x, y) && rdm < firePropagationChance) newGrid(y)(x) = Burning

          }
        case Burning =>
          newGrid(y)(x) = Burn
        case Empty =>
          val rdm = rnd.nextDouble()
          if ( hasTreeNeighbor(x,y) && rdm < treePropagationChance) newGrid(y)(x) = Tree
          else if (rdm < treePropagationChance/1.5) newGrid(y)(x) = Tree
        case Burn =>
          val rdm = rnd.nextDouble()
          if (hasTreeNeighbor(x, y) && rdm < treePropagationChance) newGrid(y)(x) = Tree
          else if (rdm < treePropagationChance / 1.5) newGrid(y)(x) = Tree
        case _ =>
      }
    }
    grid = newGrid
  }
  def hasWaterNeighbor(x:Int,y:Int) : Boolean = {
    directions.exists { case (dx, dy) =>
      val nx = x + dx
      val ny = y + dy
      nx >= 0 && nx < width && ny >= 0 && ny < height && grid(ny)(nx) == Water
    }
  }

  def hasTreeNeighbor(x: Int, y: Int): Boolean = {

    directions.exists { case (dx, dy) =>
      val nx = x + dx
      val ny = y + dy
      nx >= 0 && nx < width && ny >= 0 && ny < height && grid(ny)(nx) == Tree
    }
  }
  def hasBurningNeighbor(x: Int, y: Int): Boolean = {
    directions.exists { case (dx, dy) =>
      val nx = x + dx
      val ny = y + dy
      nx >= 0 && nx < width && ny >= 0 && ny < height && grid(ny)(nx) == Burning
    }
  }

  def burningRatio: Double = {
    val total = grid.flatten.length.toDouble
    val burning = grid.flatten.count((x)=>x == Burn || x== Burning)
    burning / total
  }

  override def toString: String = {
    grid.map { row =>
      row.mkString("[", ",", "]")
    }.mkString("{\"burningRatio\":" + burningRatio +",\"step\":[\n  ", ",\n", "\n]}")
  }

}


object Main extends App {
    def simuleOne(width:Int = 40, height:Int = 40, treeDensity:Double = 0.8, waterDensity:Double = 0.2, fireStartChance:Double = 0.05, treePropagationChance:Double = 0.1, firePropagationChance:Double = 0.5, nbrStep:Int = 100, animation:Boolean = false):String   = {
      var steps = List[String]()
      var ratioBurningSimulation = 0.0
      val simulation = new ForestFire(width, height, treeDensity, waterDensity, fireStartChance, treePropagationChance, firePropagationChance)
      if(animation){
        for (x <- 0 until nbrStep) {
          steps = simulation.toString :: steps
          ratioBurningSimulation += simulation.burningRatio
          simulation.step()
        }
      }else{
        for (x <- 0 until nbrStep) {
          steps = simulation.burningRatio.toString :: steps
          ratioBurningSimulation += simulation.burningRatio
          simulation.step()
        }
      }

      ratioBurningSimulation /= nbrStep

      "{" +
        "\"treePropagationChance\": " + treePropagationChance + "," +
        "\"firePropagationChance\": " + firePropagationChance + "," +
        "\"ratioBurningSimulation\": " + ratioBurningSimulation + "," +
        (if (animation) "\"simulation\": "  else "\"burningRatios\":") + steps.reverse.mkString("[\n", ",", "\n]")  +
        "}"
    }

  def simuleWithVisual (width:Int = 40, height:Int = 40, treeDensity:Double = 0.8, waterDensity:Double = 0.2, fireStartChance:Double = 0.05, nbrStep:Int = 100) ={
    var simulations = List[String]()
    var idx = 1
    for (treePropagationChance <- 0 until 100 by 10;firePropagationChance <- 0 until 100 by 10) {
      println(idx)
      idx += 1
      simulations = simuleOne(width, height, treeDensity, waterDensity, fireStartChance, treePropagationChance.toDouble/500, firePropagationChance.toDouble/100,nbrStep,true) :: simulations
    }
    val jsonString = "{" +
      "\"width\":" + width + "," +
      "\"height\":" + height + "," +
      "\"treeDensity\": " + treeDensity + "," +
      "\"waterDensity\": " + waterDensity + "," +
      "\"fireStartChance\":" + fireStartChance + "," +
      "\"simulations\":" +
      simulations.reverse.mkString("[\n", ",\n", "\n]}")


    val writer = new PrintWriter(new File("visu_simulation.json"))
    writer.write(jsonString)
    writer.close()
  }
  def simuleWithoutVisual (width:Int = 40, height:Int = 40, treeDensity:Double = 0.8, waterDensity:Double = 0.2, fireStartChance:Double = 0.05, nbrStep:Int = 100) ={
    var simulations = List[String]()
    var idx = 1
    for (treePropagationChance <- 0 until 100 by 1;firePropagationChance <- 0 until 100 by 1) {
      println(idx)
      idx += 1
      simulations = simuleOne(width, height, treeDensity, waterDensity, fireStartChance, treePropagationChance.toDouble/500, firePropagationChance.toDouble/100,nbrStep,false) :: simulations
    }
    val jsonString = "{" +
      "\"width\":" + width + "," +
      "\"height\":" + height + "," +
      "\"treeDensity\": " + treeDensity + "," +
      "\"waterDensity\": " + waterDensity + "," +
      "\"fireStartChance\":" + fireStartChance + "," +
      "\"simulations\":" +
      simulations.reverse.mkString("[\n", ",\n", "\n]}")


    val writer = new PrintWriter(new File("simulation.json"))
    writer.write(jsonString)
    writer.close()
  }

  val width = 40
  val height = 40
  val treeDensity = 0.8
  val waterDensity = 0.1
  val fireStartChance = 0.05
  val nbrStep = 400

  simuleWithVisual(width, height, treeDensity, waterDensity, fireStartChance,nbrStep)
  simuleWithoutVisual(width, height, treeDensity, waterDensity, fireStartChance,nbrStep)


}
