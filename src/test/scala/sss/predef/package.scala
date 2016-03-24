package sss

package object predef {
  /**
    * trace time used to compute thunk. taken from
    * <http://stackoverflow.com/a/15437838>.
    *
    * TODO: make this pure. returning a Writer monad would be one way
    */
  def time[T](str: String)(thunk: => T): T = {
    print(str + "... ")
    val t1 = System.currentTimeMillis
    val x = thunk
    val t2 = System.currentTimeMillis
    println((t2 - t1) + " msecs")
    x
  }
}
