import java.io.{FileWriter, BufferedWriter, PrintWriter}
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.Iterator
import scala.io.Source

/**
  * Created by ola on 1/17/16.
  */
object Sampler {

  def main(args: Array[String]) = {
    args.length match {
      case n if n == 3 => pickSamples(args(0), args(1).toInt, args(2).toInt == 1)
      case _ => printUsage
    }
  }

  def printUsage(): Unit = {
    println("Usage: target/scala-2.11/reservoir_2.11-1.0.jar <file-path> <sample-size> <has-header-row 0=no, 1=yes>")
    System.exit(1)
  }

  def pickSamples(fileName: String, sampleSize: Int, hasHeader: Boolean): Unit = {
    val reservoir = preLoadReservoir(Seq[String](), Source.fromFile(fileName).getLines, sampleSize, hasHeader)
    val file = Source.fromFile(fileName).getLines

    val header = if hasHeader Seq[String](file.next()) else Seq[String]()
    val samples = sample(reservoir, file, sampleSize, 0, new Random())

    outputData(header ++ samples, "samples.csv")
  }

  @tailrec
  def preLoadReservoir(reservoir: Seq[String], file: Iterator[String], sampleSize: Int, hasHeader: Boolean): Seq[String] = {
    if (hasHeader && reservoir.length == 0) file.next()
    if (file.isEmpty || reservoir.length > sampleSize - 1) {
      return reservoir
    }
    preLoadReservoir(reservoir :+ file.next(), file, sampleSize, hasHeader)
  }

  @tailrec
  def sample(reservoir: Seq[String], file: Iterator[String], sampleSize: Int, currentLine: Int, random: Random): Seq[String] = {
    if (file.isEmpty) return reservoir

    val sampleIndex = random.nextInt(sampleSize + currentLine - 1)
    sampleIndex match {
      case i if i < sampleSize => {
        val line = file.next()
        sample(reservoir.updated(sampleIndex, line), file, sampleSize, currentLine + 1, random)
      }
      case _ => {
        file.next()
        sample(reservoir, file, sampleSize, currentLine + 1, random)
      }
    }
  }

  def outputData(data: Seq[String], outFile: String) = {
    val output = data.mkString("\n")
    //println(output)
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    writer.write(output)
    writer.close()
  }
}