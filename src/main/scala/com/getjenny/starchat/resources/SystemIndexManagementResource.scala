package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 14/11/16.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.SystemIndexManagementService

import scala.util.{Failure, Success}

trait SystemIndexManagementResource extends StarChatResource {

  private[this] val systemIndexManagementService: SystemIndexManagementService.type = SystemIndexManagementService

  def systemGetIndexesRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("system_indices") {
      pathEnd {
        get {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.read)) {
              val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
              onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.indices) {
                case Success(t) =>
                  completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                    t
                  })
                case Failure(e) => completeResponse(StatusCodes.BadRequest,
                  Option {
                    ReturnMessageData(code = 100, message = e.getMessage)
                  })
              }
            }
          }
        }
      }
    }
  }

  def systemIndexManagementRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix("system_index_management") {
      path(Segment) { operation: String =>
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.write)) {
              parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                operation match {
                  case "refresh" =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.refresh(indexSuffix)) {
                      case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                      case Failure(e) => completeResponse(StatusCodes.BadRequest,
                        Option {
                          IndexManagementResponse(message = e.getMessage)
                        })
                    }
                  case "create" =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.create(indexSuffix)) {
                      case Success(t) => completeResponse(StatusCodes.Created, StatusCodes.BadRequest, Option {
                        t
                      })
                      case Failure(e) => completeResponse(StatusCodes.BadRequest,
                        Option {
                          IndexManagementResponse(message = e.getMessage)
                        })
                    }
                  case _ => completeResponse(StatusCodes.BadRequest,
                    Option {
                      IndexManagementResponse(message = "index(system) Operation not supported: " + operation)
                    })
                }
              }
            }
          }
        }
      } ~
        pathEnd {
          get {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.read)) {
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.check()) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                    t
                  })
                  case Failure(e) => completeResponse(StatusCodes.BadRequest,
                    Option {
                      IndexManagementResponse(message = e.getMessage)
                    })
                }
              }
            }
          } ~
            delete {
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, "admin", Permissions.write)) {
                  parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.remove(indexSuffix)) {
                      case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                      case Failure(e) => completeResponse(StatusCodes.BadRequest,
                        Option {
                          IndexManagementResponse(message = e.getMessage)
                        })
                    }
                  }
                }
              }
            } ~
            put {
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, "admin", Permissions.write)) {
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(systemIndexManagementService.update()) {
                    case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                      t
                    })
                    case Failure(e) => completeResponse(StatusCodes.BadRequest,
                      Option {
                        IndexManagementResponse(message = e.getMessage)
                      })
                  }
                }
              }
            }
        }
    }
  }
}


