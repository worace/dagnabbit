package works.worace.dagnabbit

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import java.io.File
import java.nio.file.Files


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
  def randStr: String = scala.util.Random.alphanumeric.take(20).mkString
  def tempPath: String = s"/tmp/${randStr}.txt"

  test("is acyclic") {
    assert(FileTask("pizza.txt").isAcyclic)
    assert(FileTask("pizza.txt", Vector(FileTask("pie.txt"))).isAcyclic)
    val root = FileTask("pizza.txt")
    root.copy(depends=Vector(root))
    assert(!root.copy(depends=Vector(root)).isAcyclic)


    val c = FileTask("c.txt")
    val b = FileTask("b.txt",Vector(c))
    val d = FileTask("d.txt", Vector(c))
    val e = FileTask("e.txt")
    val a = FileTask("a.txt", Vector(b, d, e))
    assertEquals(a.isAcyclic, true)
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

  test("constructing graphs") {
    val task = FileTask(
      path = "/tmp/a.txt",
      depends = Vector(
        FileTask(
          path = "/tmp/b.txt"
        )
      )
    )
    val exp = Map(
      task -> Vector(FileTask("/tmp/b.txt").asInstanceOf[Task]),
      FileTask("/tmp/b.txt").asInstanceOf[Task] -> Vector()
    )
    assertEquals(Dag.forwardDeps(task), exp)
  }

  val tempDir = FunFixture[File](
    setup = { test =>
      Files.createTempDirectory("dagnabbit-tests").toFile
    },
    teardown = { _ => () }
  )

  def assertFile(dir: File, name: String): Unit = {
    val f = new File(dir.getAbsolutePath() + "/" + name)
    println("check file:")
    println(f)
    assert(f.exists())
    assert(!f.isDirectory())
  }

  tempDir.test("running a dag") { dir =>
    println("run dag in dir")
    println(dir)
    def tempFile(fname: String): String = s"${dir.getAbsolutePath()}/$fname"
    // A --> B ---> C
    //  \------>D--/
    //   \---> E
    val c = FileTask(path=tempFile("c.txt"))
    val b = FileTask(tempFile("b.txt"),Vector(c))
    val d = FileTask(tempFile("d.txt"), Vector(c))
    val e = FileTask(tempFile("e.txt"))
    val a = FileTask(tempFile("a.txt"), Vector(b, d, e))

    assertEquals(a.isAcyclic, true)

    Dag.runDag(a).map { _ =>
      println("dir: ")
      println(dir)
      println("done running dag...")
      assertFile(dir, "a.txt")
      assertFile(dir, "b.txt")
      assertFile(dir, "c.txt")
      assertFile(dir, "d.txt")
      assertFile(dir, "e.txt")
    }
  }
}
