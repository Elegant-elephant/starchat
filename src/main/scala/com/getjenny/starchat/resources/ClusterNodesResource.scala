package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities.{Permissions, ReturnMessageData}
import com.getjenny.starchat.routing.{StarChatCircuitBreaker, StarChatResource}
import com.getjenny.starchat.services.ClusterNodesService

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 29/01/19.
  */

trait ClusterNodesResource extends StarChatResource {
  private[this] val clusterNodesService: ClusterNodesService.type = ClusterNodesService
  private[this] val routeName: String = """cluster_node"""

  def clusterNodesRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(routeName) {
      pathEnd {
        get {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.read)) {
              extractRequest { request =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(clusterNodesService.aliveNodes) {
                  case Success(t) =>
                    completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                  case Failure(e) =>
                    log.error(logTemplate(user.id, "", "clusterNodesRoutes", request.method, request.uri), e)
                    completeResponse(StatusCodes.BadRequest,
                      Option {
                        ReturnMessageData(code = 100, message = e.getMessage)
                      })
                }
              }
            }
          }
        }
      }
    } ~
      pathPrefix(routeName) {
        get {
          path(Segment) { uuid: String =>
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.read)) {
                extractMethod { method =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(clusterNodesService.isAlive(uuid)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(s"Node($uuid) route=clusterNodesRoutes method=$method: ", e.getMessage)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 100, message = e.getMessage)
                        })
                  }
                }
              }
            }
          }
        } ~
          post {
            authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.read)) {
                extractMethod { method =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(clusterNodesService.alive()) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(s"Node(${clusterNodesService.uuid}) route=clusterNodesRoutes method=$method: ", e.getMessage)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 101, message = e.getMessage)
                        })
                  }
                }
              }
            }
          } ~
          delete {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.read)) {
                extractMethod { method =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(clusterNodesService.cleanDeadNodes) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(s"Node(${clusterNodesService.uuid}) route=clusterNodesRoutes method=$method: ", e.getMessage)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 102, message = e.getMessage)
                        })
                  }
                }
              }
            }
          }
      }
  }
}
