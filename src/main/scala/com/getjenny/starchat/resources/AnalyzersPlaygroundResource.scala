package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 07/04/17.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.AnalyzerService

import scala.util.{Failure, Success}

trait AnalyzersPlaygroundResource extends StarChatResource {
  private[this] val analyzerService: AnalyzerService.type = AnalyzerService

  def analyzersPlaygroundRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "analyzer" ~ Slash ~ "playground") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName,
                Set(Permissions.read, Permissions.write, Permissions.read))) {
              extractRequest { request =>
                entity(as[AnalyzerEvaluateRequest]) { analyzerRequest =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(analyzerService.evaluateAnalyzer(indexName, analyzerRequest)) {
                    case Success(value) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, value)
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "analyzersPlaygroundRoutes", request.method, request.uri), e)
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
