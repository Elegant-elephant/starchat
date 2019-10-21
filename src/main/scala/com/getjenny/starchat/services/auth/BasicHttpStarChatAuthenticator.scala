package com.getjenny.starchat.services.auth

import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.server.directives.Credentials
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services._
import com.roundeights.hasher.Implicits._
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class BasicHttpStarChatAuthenticator(userService: AbstractUserService) extends AbstractStarChatAuthenticator {
  val config: Config = ConfigFactory.load()
  val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  val admin: String = config.getString("starchat.basic_http_es.admin")
  val password: String = config.getString("starchat.basic_http_es.password")
  val salt: String = config.getString("starchat.basic_http_es.salt")

  def secret(password: String, salt: String): String = {
    password + "#" + salt
  }

  def hashedSecret(password: String, salt: String): String = {
    secret(password, salt).sha512
  }

  class Hasher(salt: String) {
    def hasher(password: String): String = {
      secret(password, salt).sha512
    }
  }

  def fetchUser(id: String): Future[User] = Future {
    userService.read(UserId(id=id))
  }

  def authenticator(credentials: Credentials): Future[Option[User]] = {
    credentials match {
      case p@Credentials.Provided(id) =>
        fetchUser(id) map { user =>
          val hasher = new Hasher(user.salt)
          if (p.verify(secret = user.password, hasher = hasher.hasher)) {
            Some(user)
          } else {
            val message = "Authentication failed for the user: " + "user.id with password(" + user.password + ")"
            log.error(message)
            None
          }
        }
      case _ => Future.successful(None)
    }
  }

  def hasPermissions(user: User, index: String, permissions: Set[Permissions.Value]): Future[Boolean] = {
    user.id match {
      case `admin` => //admin can do everything
        val userPermissions = user.permissions.getOrElse("admin", Set.empty[Permissions.Value])
        Future.successful(userPermissions.contains(Permissions.admin))
      case _ =>
        val userPermissions = user.permissions.getOrElse(index, Set.empty[Permissions.Value])
        Future.successful((userPermissions & permissions).nonEmpty)
    }
  }
}
