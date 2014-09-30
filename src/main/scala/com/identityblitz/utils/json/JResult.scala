package com.identityblitz.utils.json

import java.util.NoSuchElementException

/**
 *
 */

sealed trait JResult[+T] {

  def fold[B](ef: Seq[String] => B)(sf: T => B): B = this match {
    case JSuccess(s) => sf(s)
    case JError(e) => ef(e)
  }

  def get: T

  def getOpt: Option[T] = this match {
    case JSuccess(s) => Option(s)
    case _ => None
  }

  def map[B](f: T => B): JResult[B] = this match {
    case JSuccess(v) => JSuccess(f(v))
    case j: JError => j
  }

  def and[B, R](s: JResult[B])(implicit bld: (JResult[T], JResult[B]) => JResult[R]) = bld.apply(this, s)

  def $[R <: Applicative](implicit v: JResult[T] => R) = v(this)

}

trait Applicative

case class Applicative2[A1, A2](t: JResult[(A1, A2)]) extends Applicative {
  def lift[T](f: (A1, A2) => T): JResult[T] = t map f.tupled
}
case class Applicative3[A1, A2, A3](t: JResult[(A1, A2, A3)]) extends Applicative {
  def lift[T](f: (A1, A2, A3) => T): JResult[T] = t map f.tupled
}
case class Applicative4[A1, A2, A3, A4](t: JResult[(A1, A2, A3, A4)]) extends Applicative {
  def lift[T](f: (A1, A2, A3, A4) => T): JResult[T] = t map f.tupled
}

import scala.language.implicitConversions

object JsonTools {
  implicit def jBuilder1[A1, A2](a1: JResult[A1], a2: JResult[A2]) = (a1, a2) match {
    case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1, v2))
    case (JError(v), JSuccess(_)) => JError(v)
    case (JSuccess(_), JError(v)) => JError(v)
    case (JError(v1), JError(v2)) => JError(v1 ++ v2)
  }

  implicit def jBuilder2[A1, A2, A3](a: JResult[(A1, A2)], b: JResult[A3]) = (a, b) match {
    case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v2))
    case (JError(v), JSuccess(_)) => JError(v)
    case (JSuccess(_), JError(v)) => JError(v)
    case (JError(v1), JError(v2)) => JError(v1 ++ v2)
  }

  implicit def jBuilder3[A1, A2, A3, A4](a: JResult[(A1, A2, A3)], b: JResult[A4]) = (a, b) match {
    case (JSuccess(v1), JSuccess(v2)) => JSuccess((v1._1, v1._2, v1._3, v2))
    case (JError(v), JSuccess(_)) => JError(v)
    case (JSuccess(_), JError(v)) => JError(v)
    case (JError(v1), JError(v2)) => JError(v1 ++ v2)
  }

  implicit def applicative2[A1, A2](t: JResult[(A1, A2)]) = Applicative2(t)
  implicit def applicative3[A1, A2, A3](t: JResult[(A1, A2, A3)]) = Applicative3(t)
  implicit def applicative4[A1, A2, A3, A4](t: JResult[(A1, A2, A3, A4)]) = Applicative4(t)
}

import JsonTools._
import scala.language.postfixOps

class Test(x: String, y: Int, z: Boolean) {
  override def toString: String = "Test(x = " + x + ", y = " + y + ", z = " + z + ")"
}

object Test {
  def apply(x: String, y: Int, z: Boolean) = new Test(x, y, z)

  implicit def jreader = new JReader[Test] {
    override def read(v: JVal): JResult[Test] =
      ((v \ "id").read[String] and
        (v \ "num").read[Int] and
        (v \ "act").read[Boolean] $).lift(Test.apply)

  }

}









case class JSuccess[V](value: V) extends JResult[V] {

  def get: V = value

  override def toString: String = "JSuccess(" + value + ")"

}

case class JError(errors: Seq[String]) extends JResult[Nothing] {

  def get = throw new NoSuchElementException("JError.get")

  override def toString: String = "JError(" + errors + ")"

}

object JError {

  def apply(error: String) = new JError(Seq(error))

}

