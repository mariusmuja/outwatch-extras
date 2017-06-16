package outwatch

import rxscalajs.Observable

/**
  * Created by marius on 16/06/17.
  */
package object extras {

  type Handler[Action] = Observable[Action] with Sink[Action]

}
