package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 21/04/17.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.SpellcheckService

import scala.util.{Failure, Success}

trait SpellcheckResource extends StarChatResource {

  private[this] val spellcheckService: SpellcheckService.type = SpellcheckService

  def spellcheckRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "spellcheck") { indexName =>
      pathPrefix("terms") {
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  entity(as[SpellcheckTermsRequest]) { spellCheckRequest =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(spellcheckService.termsSuggester(indexName, spellCheckRequest)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "spellcheckRoutes", request.method, request.uri), e)
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
        }
      }
    }
  }
}
