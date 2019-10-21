package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 21/04/17.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.ManausTermsExtractionService

import scala.util.{Failure, Success}

trait TermsExtractionResource extends StarChatResource {

  private[this] val spellcheckService: ManausTermsExtractionService.type = ManausTermsExtractionService

  def freqExtractionRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "extraction") { indexName =>
      pathPrefix("frequencies") {
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  entity(as[TermsExtractionRequest]) { extractionRequest =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(
                      spellcheckService.termFrequency(indexName = indexName,
                        extractionRequest = extractionRequest)
                    ) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error("index(" + indexName + ") uri=(" + request.uri +
                          ") method=(" + request.method.name + ") : " + e.getMessage)
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

  def termsExtractionRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "extraction") { indexName =>
      pathPrefix("keywords") {
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  entity(as[TermsExtractionRequest]) { extractionRequest =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(spellcheckService.textTerms(indexName = indexName,
                      extractionRequest = extractionRequest)) {
                      case Success((_, terms)) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, terms)
                      case Failure(e) =>
                        log.error("index(" + indexName + ") uri=(" + request.uri +
                          ") method=(" + request.method.name + ") : " + e.getMessage)
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

  def synExtractionRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "extraction") { indexName =>
      pathPrefix("synonyms") {
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  entity(as[SynExtractionRequest]) { extractionRequest =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(spellcheckService.termsSynonyms(indexName = indexName,
                      extractionRequest = extractionRequest)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "synExtractionRoutes",
                          request.method, request.uri), e)
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
