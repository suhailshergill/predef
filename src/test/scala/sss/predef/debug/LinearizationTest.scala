package sss.predef.debug

import sss.tags._
import org.scalatest._

// AsInstanceOf gives false positives in the face of scala reflection
@SuppressWarnings(Array("org.brianmckenna.wartremover.warts.AsInstanceOf"))
class LinearizationTest extends FreeSpec {
  import Linearization._

  /**
    * example taken from
    * <http://www.scala-lang.org/files/archive/spec/2.11/05-classes-and-objects.html#class-linearization>
    */
  object Example {
    trait AbsIterator extends AnyRef
    trait RichIterator extends AbsIterator
    trait StringIterator extends AbsIterator
    trait Iter extends StringIterator with RichIterator
  }
  import Example._

  "Linearization of a class should" - {
    val linearization = Get[Iter, String]
    "be serializable" in {
      val linString: String = linearization.mkString(", ")
      info(s"{ $linString }", None)
    }

    "contain the linearization of direct superclass as suffix" in {
      assert(linearization.endsWith(Get[StringIterator, String]))
    }

    "not contain the linearization of mixin as suffix" in {
      assert(!linearization.endsWith(Get[RichIterator, String]))
    }

    "start with most specific" in {
      assert(linearization.headOption.
        exists(_.endsWith("LinearizationTest.Example.Iter")))
    }

    "end with least specific" in {
      assert(linearization.lastOption.exists(_ == "scala.Any"))
    }
  }
}
