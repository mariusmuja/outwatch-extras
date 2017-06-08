package demo


trait Bar {

  type BaseType

  implicit class Converter(t: BaseType) {
    def convert: String = t.toString
  }

  val value: BaseType


}




object Foo extends Bar {
  class Model2(str: String)

  override type BaseType = Model2
  override val value: BaseType = null
}

object Test {

  val t = Foo.value.convert
  //  val s = MainComponent.effects.full
}



