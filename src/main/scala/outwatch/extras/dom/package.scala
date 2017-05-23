package outwatch.extras

import outwatch.dom.{Attributes, Handlers, Tags}

/**
  * Created by marius on 23/05/17.
  */
package object dom {
  object all extends Tags with Attributes

  object <^ {
    object < extends Tags
    object ^ extends Attributes
  }

  object Handlers extends Handlers
}
