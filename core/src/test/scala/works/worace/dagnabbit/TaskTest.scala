package works.worace.dagnabbit

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import java.io.File

case class FileTask(path: String, depends: Vector[Task] = Vector()) extends Task {
  def target = FileTarget(path)
  def run = IO(new File(path).createNewFile())
}

case class DoneTarget(name: String) extends Target {
  def isBuilt: IO[Boolean] = IO(true)
}

case class PendingTarget(name: String) extends Target {
  def isBuilt: IO[Boolean] = IO(false)
}

case class NoOpTask(target: Target, depends: Vector[Task] = Vector()) extends Task {
  def run: IO[Unit] = IO.unit
}

class TaskTest extends CatsEffectSuite {
  test("is acyclic") {
    assert(FileTask("pizza.txt").isAcyclic)
    assert(FileTask("pizza.txt", Vector(FileTask("pie.txt"))).isAcyclic)
    val root = FileTask("pizza.txt")
    root.copy(depends=Vector(root))
    assert(!root.copy(depends=Vector(root)).isAcyclic)
  }

  test("remaining tasks") {
    val done = NoOpTask(DoneTarget("a"))
    done.remainingTasksIO.map { t =>
      assertEquals(t, Vector())
    }
    val pending = NoOpTask(PendingTarget("a"))
    pending.remainingTasksIO.map { t =>
      assertEquals(t, Vector(pending))
    }

    val graph = NoOpTask(
      PendingTarget("a"),
      Vector(
        NoOpTask(
          DoneTarget("b"),
          Vector(NoOpTask(PendingTarget("d")))
        ),
        NoOpTask(PendingTarget("c"))
      )
    )

    graph.remainingTasksIO.map { t =>
      val targets = t.map(_.target)
      assertEquals(targets, Vector(PendingTarget("a"), PendingTarget("c")))
    }
  }
}
