package works.worace.dagnabbit

import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._
import scala.collection.mutable.Queue
import cats.effect.kernel.Deferred
import scala.concurrent.duration._

trait Target extends Product {
  def isBuilt: IO[Boolean]
  def assertBuilt: IO[Unit] = isBuilt.map {
    case true => ()
    case false => throw new RuntimeException("Task was run but its target is still not built.")
  }
}

case class FileTarget(path: String) extends Target {
  def isBuilt: IO[Boolean] = IO(new java.io.File(path).exists)
}

object Config {
  val TARGET_CHECK_PARALLELISM: Int = 5
  val TASK_PARALLELISM: Int = 5
}


trait Task {
  def target: Target
  def run: IO[Unit]

  // Big problem: Can this be something like...
  // HList[Task]...so people could match on the various subtypes to get access to their actual methods
  // Luigi...this can be either:
  // Task
  // List[Task]
  // Map[String, Task] (give names to deps)
  // and then...you can refer to them dynamically since there's no type checking..
  // Luigi allows things like...
  // self.requires["dep-a"].output().path
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

case class RunnableTask(
  task: Task,
  depSignals: Vector[Dag.DoneSignal],
  doneSignal: Dag.DoneSignal
) {
  // TODO - this needs to either re-check its own target, or be pre-pruned to not include
  // tasks that are already done
  def run: IO[Unit] = {
    depSignals.traverse(_.get).flatMap { _ =>
      task.target.isBuilt.flatMap {
        case true => doneSignal.complete(task).map { wasAbleToSet =>
          if (!wasAbleToSet)
            throw new RuntimeException("Logic Error - completion signal for task was already finished. This should only happen once.")
          println(s"Target ${task.target} is already built. Skipping run.")
        }
        case false => {
          for {
            _ <- IO.println(s"Running Task to build: ${task.target}")
            _ <- task.run
            _ <- IO.println(s"Finished building: ${task.target}")
            _ <- task.target.assertBuilt
            _ <- doneSignal.complete(task)
          } yield ()
        }
      }
    }
  }
}

object Dag {
  def targetState(t: Task): IO[Map[Target, Boolean]] =
    t.descTargets.distinct.parTraverseN(Config.TARGET_CHECK_PARALLELISM)(t => t.isBuilt.map(status => (t, status)))
      .map(_.toMap)

  // Q: How to prevent multiple tasks resolving the same target?

  def forwardDeps(current: Task, graph: TaskGraph = Map()): TaskGraph = {
    val soFar: TaskGraph = graph + (current -> current.depends)
    current.depends.foldLeft(soFar){ case (graph, dep) => graph ++ forwardDeps(dep, graph) }
  }

  def doneSignalMap(graph: TaskGraph): IO[Map[Task, DoneSignal]] = {
    graph.keys.toVector.traverse(task => Deferred[IO, Task].map(d => (task, d)))
      .map(_.toMap)
  }

  def stitchGraph(graph: TaskGraph, signals: Map[Task, DoneSignal]): Vector[RunnableTask] = {
    graph.keys.toVector.map { task =>
      val depSignals = task.depends.map(dep => signals(dep))
      val ownSignal = signals(task)
      RunnableTask(task, depSignals, ownSignal)
    }
  }

  type TaskGraph = Map[Task, Vector[Task]]
  type DoneSignal = Deferred[IO, Task]
  // A --> B ---> C
  //  \------>D--/
  //   \---> E
  def runDag(root: Task): IO[Unit] = {
    // 0 - verify acyclic
    // 1. get list of remaining tasks
    // 2. build edge sets:
    //    forward: Map[Task, Vector[Task]]
    //    backward: Map[Task, Vector[Task]]
    //    forward:    backward:
    //    (A, B)      (B, A)
    //    (A, D)      (D, A)
    //    (A, E)      (E, A)
    //    (B, C)      (C, B)
    //    (D, C)      (C, D)
    //
    // 3. Identify leaf targets
    //    - C + E
    //    -
    // 4. From leaf targets, construct backwards graph of
    //    Dependency -> Deferred[Boolean]
    //    Parent -------/
    // flatmap over tasks:
    //   task.depSignals.traverse(_.get).flatmap(_.run >> <own deferred>.complete)
    // Turn into flat collection of...
    // (Task, deps (0 or more deferreds...), forward signal: 1x deferred)

    // for {
    //   _ <- run
    //   _ <- reCheckTarget // should be complete now if we succeeded, else throw
    //   _ <- signalConsumers
    // } yield ()

    assert(root.isAcyclic, s"DAG starting from ${root} has cycles")
    val graph = forwardDeps(root)
    for {
      tState <- targetState(root) // todo - use this for pruning the graph
      signals <- doneSignalMap(graph)
      stitched = stitchGraph(graph, signals)
      _ <- stitched.parTraverseN(Config.TASK_PARALLELISM)(_.run)
    } yield {
      println("*******")
      println("tstate:")
      println(tState)
      println("**********")
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
