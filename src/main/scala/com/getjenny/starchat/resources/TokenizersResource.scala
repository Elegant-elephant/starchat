package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 03/04/17.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.TermService

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait TokenizersResource extends StarChatResource {
  private[this] val termService: TermService.type = TermService

  def esTokenizersRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "tokenizers") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                entity(as[TokenizerQueryRequest]) { request_data =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(termService.esTokenizer(indexName, request_data)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "esTokenizersRoutes", request.method,
                        request.uri, s"data=$request_data"), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 100, message = e.getMessage)
                        })
                  }
                }
              }
            }
          }
        } ~ {
          get {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                val analyzers_description: Map[String, String] =
                  TokenizersDescription.analyzersMap.map { case (name, description) =>
                    (name, description._2)
                  }
                val result: Option[Map[String, String]] = Option(analyzers_description)
                completeResponse(StatusCodes.OK, StatusCodes.BadRequest, result)
              }
            }
          }
        }
      }
    }
  }
}
