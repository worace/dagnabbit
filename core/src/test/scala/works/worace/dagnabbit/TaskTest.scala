package works.worace.dagnabbit

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import java.io.File

case class FileTask(depends: Vector[Task], path: String) extends Task {
  def target = FileTarget(path)
  def run = IO(new File(path).createNewFile())
}

class TaskTest extends CatsEffectSuite {
  test("is acyclic") {
  }
}
