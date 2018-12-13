package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.analyzers._
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import scalaz.Scalaz._

import scala.collection.immutable.{List, Map}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class ResponseServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDocumentNotFoundException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDTNotLoadedException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements response functionalities
  */
object ResponseService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService
  private[this] val cronReloadDTService: CronReloadDTService.type = CronReloadDTService

  def getNextResponse(indexName: String, request: ResponseRequestIn): Future[ResponseRequestOutOperationResult] = Future {

    val evaluationClass = request.evaluationClass match {
      case Some(c) => c
      case _ => "default"
    }

    if(! AnalyzerService.analyzersMap.contains(indexName)) {
      val message = "Decision table not ready for index(" + indexName + "), please retry later"
      log.debug(message)
      throw ResponseServiceDTNotLoadedException(message)
    }

    val userText: String = request.userInput match {
      case Some(t) =>
        t.text.getOrElse("")
      case _ => ""
    }

    val conversationId: String = request.conversationId

    val variables : Map[String, String] = request.data.getOrElse(Map.empty[String, String])

    val traversedStates: Vector[String] = request.traversedStates.getOrElse(Vector.empty[String])
    val traversedStatesCount: Map[String, Int] =
      traversedStates.foldLeft(Map.empty[String, Int])((map, word) => map + (word -> (map.getOrElse(word,0) + 1)))

    // refresh last_used timestamp
    Try(AnalyzerService.analyzersMap(indexName).lastEvaluationTimestamp = System.currentTimeMillis) match {
      case Success(_) => ;
      case Failure(e) =>
        val message = "could not update the last evaluation timestamp for: " + indexName
        log.error(message)
        throw ResponseServiceException(message, e)
    }

    // prepare search result for search analyzer
    val analyzerEvaluateRequest = AnalyzerEvaluateRequest(analyzer = "", query = userText, data = None,
      searchAlgorithm = request.searchAlgorithm, evaluationClass = request.evaluationClass)
    val searchResAnalyzers =
      decisionTableService.searchDtQueries(indexName, analyzerEvaluateRequest).map(searchRes => {
        val analyzersInternalData = decisionTableService.resultsToMap(searchRes)
        AnalyzersDataInternal(extractedVariables = variables, traversedStates = traversedStates,
          data = analyzersInternalData)
      })

    val returnState: Future[ResponseRequestOutOperationResult] = searchResAnalyzers.map( data => {
      // No states in the return values
      val maxResults: Int = request.maxResults.getOrElse(2)
      val threshold: Double = request.threshold.getOrElse(0.0d)
      val evaluationList = request.state match {
        case Some(state) =>
          List((state,
            AnalyzerService.analyzersMap(indexName).analyzerMap.get(state) match {
              case Some(v) => v
              case _ =>
                throw ResponseServiceDocumentNotFoundException("Requested state not found: state(" + state + ")")
            }))
        case _ =>
          AnalyzerService.analyzersMap(indexName).analyzerMap.toList
      }

      val analyzersEvalData: Map[String, Result] = evaluationList
        .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.evaluationClass === evaluationClass }
        .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.analyzer.build === true }
        .filter { case (stateName, runtimeAnalyzerItem) =>
          val traversedStateCount = traversedStatesCount.getOrElse(stateName, 0)
          val maxStateCount = runtimeAnalyzerItem.maxStateCounter
          maxStateCount === 0 ||
            traversedStateCount < maxStateCount // skip states already evaluated too much times
        }.map { case (stateName, runtimeAnalyzerItem) =>
        val analyzerEvaluation = runtimeAnalyzerItem.analyzer.analyzer match {
          case Some(starchatAnalyzer) =>
            Try(starchatAnalyzer.evaluate(userText, data = data)) match {
              case Success(evalRes) =>
                log.debug("ResponseService: Evaluation of State(" +
                  stateName + ") Query(" + userText + ") Score(" + evalRes.toString + ")")
                evalRes
              case Failure(e) =>
                val message = "ResponseService: Evaluation of State(" + stateName + ") : " + e.getMessage
                log.error(message)
                throw AnalyzerEvaluationException(message, e)
            }
          case _ =>
            val message = "ResponseService: analyzer is None (" + stateName + ")"
            log.debug(message)
            Result(score = threshold, data = data)
        }
        val stateId = stateName
        (stateId, analyzerEvaluation)
      }.filter { case (_, analyzerEvaluation) => analyzerEvaluation.score >= threshold }
        .sortWith {
          case ((_, analyzerEvaluation1), (_, analyzerEvaluation2)) =>
            analyzerEvaluation1.score > analyzerEvaluation2.score
        }.take(maxResults).toMap

      if(analyzersEvalData.isEmpty) {
        throw
          ResponseServiceDocumentNotFoundException(
            "The analyzers evaluation list is empty, threshold could be too high")
      }

      decisionTableService.read(indexName, analyzersEvalData.keys.toList).map { docResults =>
          val dtDocumentsList = docResults.hits.par.map { item =>
              val doc: DTDocument = item.document
              val state = doc.state
              val evaluationRes: Result = analyzersEvalData(state)
              val maxStateCount: Int = doc.maxStateCount
              val analyzer: String = doc.analyzer
              var bubble: String = doc.bubble
              var actionInput: Map[String, String] = doc.actionInput
              val stateData: Map[String, String] = doc.stateData

              for ((key, value) <- data.extractedVariables) {
                bubble = bubble.replaceAll("%" + key + "%", value)
                actionInput = doc.actionInput map { case (ki, vi) =>
                  val newValue: String = vi.replaceAll("%" + key + "%", value)
                  (ki, newValue)
                }
              }

              for ((key, value) <- evaluationRes.data.extractedVariables) {
                bubble = bubble.replaceAll("%" + key + "%", value)
                actionInput = doc.actionInput map { case (ki, vi) =>
                  val newValue: String = vi.replaceAll("%" + key + "%", value)
                  (ki, newValue)
                }
              }

              val cleanedData =
                data.extractedVariables ++
                  evaluationRes.data.extractedVariables
                    .filter { case (key, _) => !(key matches "\\A__temp__.*") }

              val traversedStatesUpdated: Vector[String] = traversedStates ++ Vector(state)
              val responseItem: ResponseRequestOut = ResponseRequestOut(conversationId = conversationId,
                state = state,
                maxStateCount = maxStateCount,
                traversedStates = traversedStatesUpdated,
                analyzer = analyzer,
                bubble = bubble,
                action = doc.action,
                data = cleanedData,
                actionInput = actionInput,
                stateData = stateData,
                successValue = doc.successValue,
                failureValue = doc.failureValue,
                score = evaluationRes.score)
              responseItem
          }.toList.sortWith(_.score > _.score)
          ResponseRequestOutOperationResult(ReturnMessageData(200, ""),
            Option {dtDocumentsList})
      }

    }).flatten.map(x => x)
    Await.result(returnState, 30.second)
  }
}
