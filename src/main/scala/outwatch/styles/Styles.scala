package outwatch.styles

import rxscalajs.Subject
import rxscalajs.subscription.Subscription

import scalacss.defaults.Exports.StyleSheet

/**
  * Created by marius on 11/06/17.
  */
object Styles {
  private val styles = Subject[StyleSheet.Inline]()

  def publish(ss: StyleSheet.Inline): Unit = styles.next(ss)

  def subscribe(f: StyleSheet.Inline => Unit): Subscription = styles.subscribe(f)

  trait Publish { self: StyleSheet.Inline =>
    publish(self)
  }

}
