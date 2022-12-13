package nl.grons.advent

package object y2022 {

  implicit class IntTimes(val i: Int) extends AnyVal {
    def times(task: => Unit): Unit = {
      (1 to i).foreach { _ => task }
    }
  }

}
