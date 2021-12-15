package works.worace.dagnabbit

import cats.effect.IO
import cats.effect.implicits._
import scala.collection.mutable.Queue

trait Target extends Product {
  def isBuilt: IO[Boolean]
}

case class FileTarget(path: String) extends Target {
  def isBuilt: IO[Boolean] = IO(new java.io.File(path).exists)
}


trait Task extends Product {
  def target: Target
  def run: IO[Unit]
  def depends: Vector[Task]

  def execute: IO[Unit] = {
    target.isBuilt.flatMap {
      case true => IO.unit
      case false => ???
    }
  }

  def productArity: Int = depends.size
  def productElement(n: Int): Task = depends(n)
  def descTargets: Vector[Target] = depends.flatMap(_.descTargets) :+ target
}

object Dag {
  val TARGET_CHECK_PARALLELISM: Int = 5
  def targetState(t: Task): IO[Map[Target, Boolean]] =
    t.descTargets.distinct.parTraverseN(TARGET_CHECK_PARALLELISM)(t => t.isBuilt.map(status => (t, status)))
      .map(_.toMap)

  // Q: How to prevent multiple tasks resolving the same target?
  def isAcyclic(root: Task): Boolean = {
    var visited = Set(root)
    val queue = Queue(root)
    var foundRepeat = false

    while (queue.nonEmpty && !foundRepeat) {
      val current = queue.dequeue()
      if (visited.contains(current)) {
        foundRepeat = true
      } else {
        queue.enqueue(current.depends:_*)
      }
    }
    foundRepeat
  }

  def runTranches(root: Task): IO[Vector[Vector[Task]]] = {
    for {
      tState <- targetState(root)
    } yield {
      ???
    }
  }
}


// A --> B ---> C
//  \------>D--/
// Order:
//      /-- D -- A
// C --> -- B --/




//
