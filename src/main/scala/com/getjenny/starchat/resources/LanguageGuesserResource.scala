package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 19/12/16.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.LanguageGuesserService

import scala.util.{Failure, Success}

trait LanguageGuesserResource extends StarChatResource {

  private[this] val languageGuesserService: LanguageGuesserService.type = LanguageGuesserService

  def languageGuesserRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "language_guesser") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                entity(as[LanguageGuesserRequestIn]) { request_data =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(languageGuesserService.guessLanguage(indexName, request_data)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "languageGuesserRoutes", request.method, request.uri), e)
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
        path(Segment) { language: String =>
          get {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(languageGuesserService.getLanguages(indexName, language)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "languageGuesserRoutes", request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 101, message = e.getMessage)
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
