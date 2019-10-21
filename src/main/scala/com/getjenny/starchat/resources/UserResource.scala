package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 20/12/16.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.{AbstractUserService, UserService}
import javax.naming.AuthenticationException

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

trait UserResource extends StarChatResource {

  private[this] val userService: AbstractUserService = UserService.service

  def postUserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("user") {
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.admin)) {
              entity(as[User]) { userEntity =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(userService.create(userEntity)) {
                  case Success(t) => completeResponse(StatusCodes.Created, StatusCodes.BadRequest, Some(t))
                  case Failure(e) => handleFailure(e)
                }
              }
            }
          }
        }
      }
    }
  }

  def putUserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("user") {
      pathEnd {
        put {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.admin)) {
              entity(as[UserUpdate]) { userEntity =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(userService.update(userEntity)) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                  case Failure(e) => handleFailure(e)
                }
              }
            }
          }
        }
      }
    }
  }

  def delUserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("user" ~ Slash ~ "delete") {
      post {
        authenticateBasicAsync(realm = authRealm,
          authenticator = authenticator.authenticator) { user =>
          authorizeAsync(_ =>
            authenticator.hasPermissions(user, "admin", Permissions.admin)) {
            entity(as[UserId]) { userEntity =>
              val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
              onCompleteWithBreakerFuture(breaker)(userService.delete(userEntity)) {
                case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                case Failure(e) => handleFailure(e)
              }
            }
          }
        }
      }
    }
  }

  def getUserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("user" ~ Slash ~ "get") {
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.admin)) {
              entity(as[UserId]) { userEntity =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(userService.read(userEntity)) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                  case Failure(e) => handleFailure(e)
                }
              }
            }
          }
        }
      }
    }
  }

  def genUserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("user" ~ Slash ~ "generate") {
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.admin)) {
              entity(as[UserUpdate]) { userEntity =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(userService.genUser(userEntity, authenticator)) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                  case Failure(e) => handleFailure(e)
                }
              }
            }
          }
        }
      }
    }
  }

  private[this] def handleFailure(e: Throwable): Route = {
    e match {
      case authException: AuthenticationException =>
        completeResponse(StatusCodes.Unauthorized, authException.getMessage)
      case NonFatal(nonFatalE) =>
        completeResponse(StatusCodes.Unauthorized, nonFatalE.getMessage)
      case _: Exception =>
        completeResponse(StatusCodes.BadRequest, e.getMessage)
    }
  }

}

