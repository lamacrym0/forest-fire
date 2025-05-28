import scala.util.Random
import java.io.PrintWriter
import java.io.File

trait Cell

case object Empty extends Cell{
  override def toString: String = "Empty"
}

case object Tree extends Cell{
  override def toString: String = "Tree"
}

case object Burning extends Cell{
  override def toString: String = "Burning"
}

case object Water extends Cell{
  override def toString: String = "Water"
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
      }.mkString)
    }
    println("\n")

  }

  def step(): Unit = {
    val newGrid = grid.map(_.clone())
    for (y <- 0 until height; x <- 0 until width) {
      grid(y)(x) match {
        case Tree =>
          if (hasBurningNeighbor(x, y) && rnd.nextDouble() < firePropagationChance) newGrid(y)(x) = Burning else if (rnd.nextDouble() < fireStartChance) newGrid(y)(x) = Burning
        case Burning =>
          newGrid(y)(x) = Empty
        case Empty =>
          if (rnd.nextDouble() < treePropagationChance) newGrid(y)(x) = Tree
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
  def hasBurningNeighbor(x: Int, y: Int): Boolean = {
    directions.exists { case (dx, dy) =>
      val nx = x + dx
      val ny = y + dy
      nx >= 0 && nx < width && ny >= 0 && ny < height && grid(ny)(nx) == Burning
    }
  }

  def burningRatio: Double = {
    val total = grid.flatten.length.toDouble
    val burning = grid.flatten.count(_ == Burning)
    burning / total
  }

  override def toString: String = {
    grid.map { row =>
      row.map {
        case Empty => "\"Empty\""
        case Tree => "\"Tree\""
        case Burning => "\"Burning\""
        case Water => "\"Water\""
      }.mkString("[", ",", "]")
    }.mkString("[\n  ", ",\n  ", "\n]")
  }

}

object Main extends App {
  val width = 15
  val height = 15
  val treeDensity = 0.8
  val waterDensity = 0.2
  val fireStartChance = 0.05
  val treePropagationChance = 0.1
  val firePropagationChance = 0.5
  var simulations = List[String]()

  val simulation = new ForestFire(width, height, treeDensity, waterDensity, fireStartChance, treePropagationChance, firePropagationChance)

  for (_ <- 0 until 50) {
    simulations = simulation.toString :: simulations
    simulation.step()
  }

  val jsonString = "{" +
    "\"width\":" + width + "," +
    "\"height\":" + height + "," +
    "\"treeDensity\": " + treeDensity + "," +
    "\"waterDensity\": " + waterDensity + "," +
    "\"fireStartChance\":" + fireStartChance + "," +
    "\"treePropagationChance\": " + treePropagationChance + "," +
    "\"firePropagationChance\": " + firePropagationChance + "," +
    "\"simulations\": " +
    simulations.reverse.mkString("[\n", ",\n", "\n]") + "}"

  val writer = new PrintWriter(new File("simulation.json"))
  writer.write(jsonString)
  writer.close()
}
