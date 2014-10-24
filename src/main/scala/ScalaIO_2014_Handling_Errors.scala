package com.normation


/*
*************************************************************************************
* Copyright 2014 Normation SAS
*************************************************************************************
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*************************************************************************************
*/


/*
 * This code was demonstrated at Scala.io 2014. It intents to list all the main
 * way of handling errors in Scala.
 * In short: use scalaz.\/
 *   https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/Either.scala
 *
 *   Read these very nice information:
 *   http://typelevel.org/blog/2014/02/21/error-handling.html
 *   http://stackoverflow.com/questions/12307965/method-parameters-validation-in-scala-with-for-comprehension-and-monads/12309023#12309023
 *
 * And add you custom syntactic sugar on top of that.
 *
 * In all the example, we follow the two same method, one that
 * get an user by id from somewhere (and can fail doing thant),
 * and one saving the modified user after that.
 *
 * The returned type of these method is evolving with the error
 * ADT processed in the example
 *
 * So, you can simply look for these name:
 * def getUser(id: UserId) : XXXXXX = ???
 * def saveUser(user: User): XXXXXX = ???
 *
 */


/*
 * Use Option to handle error - that's a bad idea.
 * Option semantic is missing/present element, not
 * error management.
 */
object use_option {

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String)

  def getUser(id: UserId): Option[User] = ???
  def saveUser(user: User): Option[User] = ???

  val u = for {
    user    <- getUser(UserId("fanf42"))
    updated =  user.copy(name = user.name.capitalize)
    saved   <- saveUser(updated)
  } yield {
    saved
  }

  u match {
    case Some(User(_, name)) => println(name + " saved !")
    case None                => println("None ? Errr ? Dunno what happened")
  }
}

/*
 * Use scala.Either.
 * Bad idea. It is no right biased, so that leads to
 * far to much boilerplate to keep a nice flow in
 * the code, even in simpl case like that one.
 */
object use_either {

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String)

  case class StrError(value: String)

  def getUser(id: UserId): Either[StrError, User] = ???
  def saveUser(user: User): Either[StrError, User] = ???

  val u = for {
    user    <- getUser(UserId("fanf42")).right
    updated <- Right(user.copy(name = user.name.capitalize)).right
    saved   <- saveUser(user).right
  } yield {
    saved
  }

  u match {
    case Left(StrError(msg))  => println("Oh no, error: " + msg)
    case Right(User(_, name)) => println(name + " is saved!")
  }

}


/*
 * Box from Liftweb could have been cool.
 * But the semantic of Empty is ambiguous,
 * and you REALLY don't want to have any ambiguity here.
 */
object use_box {
  import net.liftweb.common._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String)

  def getUser(id: UserId) : Box[User] = ???
  def saveUser(user: User): Box[User] = ???

  val id = UserId("fanf")
  val u = {
    for {
      user    <- getUser(id)
      updated =  user.copy(name = user.name.capitalize)
      saved   <- saveUser(updated)
    } yield {
      saved
    }
  }

  u match {
    case Full(User(_, name)) => println(name + " is saved!")
    //look, I don't process Empty!
    case eb: EmptyBox        => println("Oh no, error!")
  }

}



/*
 * Still, Box is nice due to all the syntactic sugar it has
 */
object use_box_with_sugar {
  import net.liftweb.common._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String)

  def getUser(id: UserId) : Box[User] = ???
  def saveUser(user: User): Box[User] = ???

  val id = UserId("fanf")
  val u = {
    for {
      user    <- getUser(id)       ?~! s"Can't get user ${id.value}"
      updated =  user.copy(name = user.name.capitalize)
      saved   <- saveUser(updated) ?~! s"Can't save user ${id.value}"
    } yield {
      saved
    }
  }

  u match {
    case Full(User(_, name)) => println(name + " is saved!")
    case eb: EmptyBox =>
      val failure = eb ?~! s"Error when saving ${id.value}"
      println(failure.messageChain)
      failure.rootExceptionCause.foreach { ex =>
        println("Exception was: " + ex.getMessage)
      }
  }

}


/*
 * This is a version where, by convention, Emtpy is
 * forbidden, and so missing User is a None
 */
object use_box__corrected {
  import net.liftweb.common._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String)

  def getUser(id: UserId) : Box[Option[User]] = ???
  def saveUser(user: User): Box[User] = ???


  val id = UserId("fanf")
  val correctUser = {
    for {
      user    <- getUser(id)       ?~! s"Can't get user ${id.value}"
      updated =  user match {
                   case None    => User(id, id.value.capitalize)
                   case Some(u) => u.copy(name = u.name.capitalize)
                 }
      saved   <- saveUser(updated) ?~! s"Can't save user ${id.value}"
    } yield {
      saved
    }
  }

  correctUser match {
    case eb: EmptyBox =>
      val failure = eb ?~! s"There was an error with persistance of user with id ${id.value}"
      println(failure.messageChain)
      failure.rootExceptionCause.foreach { ex =>
        println("Exception was: ", ex)
      }
    case Full(User(_, name)) =>  println(name + " is saved!")
  }
}


/*
 * So, this is Scalaz.validation
 */
object use_scalaz_validation {

  import scalaz._
  import Scalaz._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String, age: Int)

  case class StrError(value: String)


  def checkAge(user: User) : Validation[StrError, User] = {
    if(user.age < 18)      StrError("Too Young!").fail
    else if(user.age > 65) StrError("Too Old!").fail
    else                   user.success
  }

  def checkName(user: User) : Validation[StrError, User] = {
    val r = """\w""".r
    if(r.pattern.matcher(user.name).matches) user.success
    else                                     StrError("Too Young!").fail
  }

  def validate(user: User) = {
    checkAge(user).toValidationNel |@| checkName(user).toValidationNel
  }


  def main(args: Array[String]): Unit = {

    println(validate(User(UserId("foo"), "foo", 42)))

  }
}

/*
 * And this is scalaz.\/ (disjunction)
 */
object use_scalaz_disjunction {

  import scalaz._
  import Scalaz._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String, age: Int)

  case class StrError(value: String)

  def getUser(id: UserId) : \/[StrError, User] = ???
  def saveUser(user: User): \/[StrError, User] = ???

  val u = for {
    user    <- getUser(UserId("fanf42"))
    updated =  user.copy(name = user.name.capitalize)
    saved   <- saveUser(updated)
  } yield {
    saved
  }

  u match {
    case -\/(StrError(msg)) => println("Oh no, an error!")
    case \/-(User(_, name, _)) => println(name + " is saved!")
  }
}


/*
 * Finally, this is adding nice syntactic sugar to scalaz.\/
 */
object use_scalaz_disjunction_with_sugar {

  import scalaz._
  import Scalaz._

  case class UserId(value: String) extends AnyVal
  case class User(id: UserId, name: String, age: Int)

  /**
   * We want a general error type that:
   * - can store a message and optionally an exception
   * - is never empty (that's why chaining use NonEmptyList)
   * - can stack errors
   */
  final case class Fail(message: String, cause: Option[\/[Throwable, Fail]] = None) {

    def info(s: String) = Fail(s, Some(\/-(this)))

    def withEx(ex: Throwable) = this.copy(cause = Some(-\/(ex)))

    def messages(): NonEmptyList[String] = cause match {
      case None               => NonEmptyList(message)
      case Some(-\/(exp))     => message <::message <:: NonEmptyList(exp.getMessage)
      case Some(\/-(parent))  => message <::message <:: parent.messages
    }

    def userMessage() : String = messages.list.mkString("", " <- ", "")

    def getRootException(): Option[Throwable] = cause flatMap { _ match {
      case -\/(exp)     => Some(exp)
      case \/-(parent)  => parent.getRootException
    } }
  }

  /*
   * A nice name for our error ADT
   */
  type Expect[T] = \/[Fail, T]

  /*
   * More syntactic sugar.
   * The name is actually a joke, if you doubt it.
   */
  implicit class WhyILostMyExpectation[T](h: Expect[T]) {
    def onError(s: String): Expect[T] = h.leftMap( _.info(s))

    //you can actually use any name you want, like that nice
    //utf-8 one, very informative:
    def ♥(s: String): Expect[T] = h.leftMap( _.info(s))
  }

  /*
   * Look, my returned type are pretty and I more or less understand
   * that perhaps I won't get that damn user:
   */
  def getUser(id: UserId) : Expect[User] = ???
  def saveUser(user: User): Expect[User] = ???

  val u = for {
    user    <- getUser(UserId("fanf42")) onError("Can't get user!")
    updated =  user.copy(name = user.name.capitalize)
    saved   <- saveUser(updated)         ♥("Can't save user!")
  } yield {
    saved
  }

  u match {
    case -\/(f:Fail) =>
      //look, syntactic sugar that could prints:
      // "Can't get user!" <- "The connection to the database dies"
      println(f.userMessage)
      //and, here, find back the root cause
      f.getRootException.foreach(ex =>
        println("Root cause exception is: " + ex.getMessage)
        //for example: "Root cause exception is: JDBCSomethingGoesWildException"
      )
    case \/-(User(_, name, _)) => println(name + " is saved!")
  }
}

//that's it !
