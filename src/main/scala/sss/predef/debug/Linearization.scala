package sss.predef.debug

/**
  * Rudimentary utility to query the linearization of a class
  * <http://www.scala-lang.org/files/archive/spec/2.11/05-classes-and-objects.html#class-linearization>
  */
object Linearization {
  import scala.reflect.runtime.universe._

  trait Get[Output] {
    def get[T: TypeTag]: List[Output]
  }
  object Get {
    def apply[T: TypeTag, O: Get]: List[O] = implicitly[Get[O]].get

    implicit object GetSymbol extends Get[Symbol] {
      def get[T: TypeTag]: List[Symbol] = {
        val tt = typeTag[T]
        val tpe = tt.tpe
        tpe.baseClasses
      }
    }
    implicit object GetString extends Get[String] {
      def get[T: TypeTag]: List[String] = Get[T, Symbol].map(_.fullName)
    }
  }
}
