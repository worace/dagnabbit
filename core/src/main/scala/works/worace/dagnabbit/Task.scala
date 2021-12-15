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

object Config {
  val TARGET_CHECK_PARALLELISM: Int = 5
}


trait Task {
  def target: Target
  def run: IO[Unit]
  def depends: Vector[Task]

  def execute: IO[Unit] = {
    target.isBuilt.flatMap {
      case true => IO.unit
      case false => ???
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case t: Task => t.target.equals(target)
      case _ => false
    }
  }

  def descTargets: Vector[Target] = depends.flatMap(_.descTargets) :+ target

  def isAcyclic: Boolean = {
    isAcyclic(Set())
  }
  private def isAcyclic(visited: Set[Task]): Boolean = {
    if (visited.contains(this)) {
      false
    } else {
      depends.forall(_.isAcyclic(visited + this))
    }
  }

  def remainingTasks(targetStates: Map[Target, Boolean]): Vector[Task] = {
    if (targetStates(target)) {
      Vector()
    } else {
      Vector(this) ++ depends.flatMap(_.remainingTasks(targetStates))
    }
  }

  def remainingTasksIO: IO[Vector[Task]] =
    targetStates.map(s => remainingTasks(s))

  def targetStates: IO[Map[Target, Boolean]] =
    descTargets.distinct.parTraverseN(Config.TARGET_CHECK_PARALLELISM)(t => t.isBuilt.map(status => (t, status)))
      .map(_.toMap)

  def allTasks: Vector[Task] = {
    assert(isAcyclic)
    Vector(this) ++ depends.flatMap(_.allTasks)
  }
}

object Dag {
  def targetState(t: Task): IO[Map[Target, Boolean]] =
    t.descTargets.distinct.parTraverseN(Config.TARGET_CHECK_PARALLELISM)(t => t.isBuilt.map(status => (t, status)))
      .map(_.toMap)

  // Q: How to prevent multiple tasks resolving the same target?

  // A --> B ---> C
  //  \------>D--/
  def runTranches(root: Task): IO[Vector[Vector[Task]]] = {
    // 0 - verify acyclic
    // 1. get list of remaining tasks
    // 2. build edge sets:
    //    Map[Task, Task]
    //    forward:    backward:
    //    (A, B)      (B, A)
    //    (A, D)      (D, A)
    //    (A, E)      (E, A)
    //    (B, C)      (C, B)
    //    (D, C)      (C, D)
    //
    // 3. Identify leaf targets
    // 4. From leaf targets, construct backwards graph of
    //    Dependency -> Deferred[Boolean]
    //    Parent -------/
    // flatmap over tasks:
    //   task.depSignals.traverse(_.get).flatmap(_.run >> <own deferred>.complete)

    // for {
    //   _ <- run
    //   _ <- reCheckTarget // should be complete now if we succeeded, else throw
    //   _ <- signalConsumers
    // } yield ()

    assert(root.isAcyclic)
    for {
      tState <- targetState(root)
    } yield {
      ???
    }
  }
}


// A --> B ---> C
//  \------>D--/
//   \---> E
// Order:
//      /-- D -- A
// C --> -- B --/




//
