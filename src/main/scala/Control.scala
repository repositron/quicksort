/**
 * Created by ljw on 23/05/14.
 */
package org.ljw.util

// from stackoverflow
object Control {
  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }
}