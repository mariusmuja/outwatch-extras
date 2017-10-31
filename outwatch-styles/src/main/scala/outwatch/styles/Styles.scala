package outwatch.styles

import rxscalajs.Subject
import rxscalajs.subscription.Subscription

import scalacss.defaults.Exports.StyleSheet

/**
  * Created by marius on 11/06/17.
  */
trait Styles[S] {
  private val styles = Subject[S]()

  def publish(s: S): Unit = styles.next(s)

  def subscribe(f: S => Unit): Subscription = styles.subscribe(f)

  trait Publish { self: S => publish(self) }
}

object Styles extends Styles[StyleSheet.Inline]
