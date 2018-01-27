package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import java.util

import akka.actor.ActorSystem
import com.getjenny.starchat.entities._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.collection.immutable.{List, Map}
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.client.transport.TransportClient
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.action.index.IndexResponse
import org.elasticsearch.action.update.UpdateResponse
import org.elasticsearch.action.delete.{DeleteRequestBuilder, DeleteResponse}
import org.elasticsearch.action.get.{GetResponse, MultiGetItemResponse, MultiGetRequestBuilder, MultiGetResponse}
import org.elasticsearch.action.search.{SearchRequestBuilder, SearchResponse, SearchType}
import org.elasticsearch.index.reindex.{DeleteByQueryAction, BulkByScrollResponse}
import org.elasticsearch.index.query.{BoolQueryBuilder, InnerHitBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.common.unit._

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import org.elasticsearch.search.SearchHit
import org.elasticsearch.rest.RestStatus
import com.getjenny.starchat.analyzer.analyzers._

import java.io.{File, FileReader}
import scala.util.{Failure, Success, Try}
import akka.event.{Logging, LoggingAdapter}
import akka.event.Logging._
import com.getjenny.starchat.SCActorSystem
import org.elasticsearch.action.admin.indices.refresh.RefreshResponse
import org.apache.lucene.search.join._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable


/**
  * Implements functions, eventually used by DecisionTableResource, for searching, get next response etc
  */
object DecisionTableService {
  val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  val queriesScoreMode = Map[String, ScoreMode]("min" -> ScoreMode.Min, "max" -> ScoreMode.Max,
    "avg" -> ScoreMode.Avg, "total" -> ScoreMode.Total)

  def getIndexName(index_name: String, suffix: Option[String] = None): String = {
    index_name + "." + suffix.getOrElse(elasticClient.dtIndexSuffix)
  }

  def search(index_name: String, documentSearch: DTDocumentSearch): Future[Option[SearchDTDocumentsResults]] = {
    val client: TransportClient = elasticClient.getClient()
    val searchBuilder : SearchRequestBuilder = client.prepareSearch(getIndexName(index_name))
      .setTypes(elasticClient.dtIndexSuffix)
      .setSearchType(SearchType.DFS_QUERY_THEN_FETCH)

    val minScore = documentSearch.min_score.getOrElse(
      Option{elasticClient.queryMinThreshold}.getOrElse(0.0f)
    )

    val boostExactMatchFactor = documentSearch.boost_exact_match_factor.getOrElse(
      Option{elasticClient.boostExactMatchFactor}.getOrElse(1.0f)
    )

    searchBuilder.setMinScore(minScore)

    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()
    if (documentSearch.state.isDefined)
      boolQueryBuilder.must(QueryBuilders.termQuery("state", documentSearch.state.get))

    if (documentSearch.execution_order.isDefined)
      boolQueryBuilder.must(QueryBuilders.matchQuery("execution_order", documentSearch.state.get))

    if(documentSearch.queries.isDefined) {
      val nestedQuery: QueryBuilder = QueryBuilders.nestedQuery(
        "queries",
        QueryBuilders.boolQuery()
          .must(QueryBuilders.matchQuery("queries.query.stem_bm25", documentSearch.queries.get))
          .should(QueryBuilders.matchPhraseQuery("queries.query.raw", documentSearch.queries.get)
            .boost(1 + (minScore * boostExactMatchFactor))
          ),
        queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
      ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100))
      boolQueryBuilder.must(nestedQuery)
    }

    searchBuilder.setQuery(boolQueryBuilder)

    val searchResponse : SearchResponse = searchBuilder
      .setFrom(documentSearch.from.getOrElse(0)).setSize(documentSearch.size.getOrElse(10))
      .execute()
      .actionGet()

    val documents : Option[List[SearchDTDocument]] =
      Option { searchResponse.getHits.getHits.toList.map( { case(e) =>

        val item: SearchHit = e

        val state : String = item.getId

        val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

        val executionOrder: Int = source.get("execution_order") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val maxStateCount : Int = source.get("max_state_count") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val analyzer : String = source.get("analyzer") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val queries : List[String] = source.get("queries") match {
          case Some(t) =>
            val offsets = e.getInnerHits.get("queries").getHits.toList.map(innerHit => {
              innerHit.getNestedIdentity.getOffset
            })
            val query_array = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]].asScala.toList
              .map(q_e => q_e.get("query"))
            val queriesOrdered : List[String] = offsets.map(i => query_array(i))
            queriesOrdered
          case None => List.empty[String]
        }

        val bubble : String = source.get("bubble") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val action : String = source.get("action") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val actionInput : Map[String,String] = source.get("action_input") match {
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val stateData : Map[String,String] = source.get("state_data") match {
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val successValue : String = source.get("success_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val failureValue : String = source.get("failure_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val document : DTDocument = DTDocument(state = state, execution_order = executionOrder,
          max_state_count = maxStateCount,
          analyzer = analyzer, queries = queries, bubble = bubble,
          action = action, action_input = actionInput, state_data = stateData,
          success_value = successValue, failure_value = failureValue)

        val searchDocument : SearchDTDocument = SearchDTDocument(score = item.getScore, document = document)
        searchDocument
      }) }

    val filteredDoc : List[SearchDTDocument] = documents.getOrElse(List[SearchDTDocument]())

    val maxScore : Float = if(searchResponse.getHits.totalHits > 0) {
      searchResponse.getHits.getMaxScore
    } else {
      0.0f
    }

    val total : Int = filteredDoc.length
    val searchResults : SearchDTDocumentsResults = SearchDTDocumentsResults(total = total, max_score = maxScore,
      hits = filteredDoc)

    val searchResultsOption : Future[Option[SearchDTDocumentsResults]] = Future { Option { searchResults } }
    searchResultsOption
  }

  def searchDtQueries(index_name: String, user_text: String): Option[SearchDTDocumentsResults] = {
    val dtDocumentSearch: DTDocumentSearch =
      DTDocumentSearch(from = Option {
        0
      }, size = Option {
        10000
      },
        min_score = Option {
          elasticClient.queryMinThreshold
        },
        execution_order = None: Option[Int],
        boost_exact_match_factor = Option {
          elasticClient.boostExactMatchFactor
        },
        state = None: Option[String], queries = Option {
          user_text
        })

    val search_result: Try[Option[SearchDTDocumentsResults]] =
      Await.ready(this.search(index_name, dtDocumentSearch), 10.seconds).value.get
    val found_documents = search_result match {
      case Success(t) =>
        t
      case Failure(e) =>
        val message = "ResponseService search"
        log.error(message + " : " + e.getMessage)
        throw new Exception(message, e)
    }
    found_documents
  }

  def resultsToMap(index_name: String, results: Option[SearchDTDocumentsResults]): Map[String, Any] = {
    val search_results_map: Map[String, Any] = if (results.isEmpty || results.get.hits.isEmpty) {
      Map.empty[String, Any]
    } else {
      val m: Map[String, (Float, SearchDTDocument)] = results.get.hits.map(doc => {
        (doc.document.state, (doc.score, doc))
      }).toMap
      Map("dt_queries_search_result" -> Option{m})
    }
    search_results_map
  }

  def create(index_name: String, document: DTDocument, refresh: Int): Future[Option[IndexDocumentResult]] = Future {
    val builder : XContentBuilder = jsonBuilder().startObject()

    builder.field("state", document.state)
    builder.field("execution_order", document.execution_order)
    builder.field("max_state_count", document.max_state_count)
    builder.field("analyzer", document.analyzer)

    val array = builder.startArray("queries")
    document.queries.foreach(q => {
      array.startObject().field("query", q).endObject()
    })
    array.endArray()

    builder.field("bubble", document.bubble)
    builder.field("action", document.action)

    val action_input_builder : XContentBuilder = builder.startObject("action_input")
    for ((k,v) <- document.action_input) action_input_builder.field(k,v)
    action_input_builder.endObject()

    val state_data_builder : XContentBuilder = builder.startObject("state_data")
    for ((k,v) <- document.state_data) state_data_builder.field(k,v)
    state_data_builder.endObject()

    builder.field("success_value", document.success_value)
    builder.field("failure_value", document.failure_value)

    builder.endObject()

    val client: TransportClient = elasticClient.getClient()
    val response = client.prepareIndex().setIndex(getIndexName(index_name))
      .setType(elasticClient.dtIndexSuffix)
      .setId(document.state)
      .setSource(builder).get()

    if (refresh != 0) {
      val refresh_index = elasticClient.refreshIndex(getIndexName(index_name))
      if(refresh_index.failed_shards_n > 0) {
        throw new Exception("DecisionTable : index refresh failed: (" + index_name + ")")
      }
    }

    val doc_result: IndexDocumentResult = IndexDocumentResult(index = response.getIndex,
      dtype = response.getType,
      id = response.getId,
      version = response.getVersion,
      created = response.status == RestStatus.CREATED
    )

    Option {doc_result}
  }

  def update(index_name: String, id: String, document: DTDocumentUpdate, refresh: Int):
  Future[Option[UpdateDocumentResult]] = Future {
    val builder : XContentBuilder = jsonBuilder().startObject()

    document.analyzer match {
      case Some(t) => builder.field("analyzer", t)
      case None => ;
    }

    document.execution_order match {
      case Some(t) => builder.field("execution_order", t)
      case None => ;
    }
    document.max_state_count match {
      case Some(t) => builder.field("max_state_count", t)
      case None => ;
    }
    document.queries match {
      case Some(t) =>

        val array = builder.startArray("queries")
        t.foreach(q => {
          array.startObject().field("query", q).endObject()
        })
        array.endArray()
      case None => ;
    }
    document.bubble match {
      case Some(t) => builder.field("bubble", t)
      case None => ;
    }
    document.action match {
      case Some(t) => builder.field("action", t)
      case None => ;
    }
    document.action_input match {
      case Some(t) =>
        val action_input_builder : XContentBuilder = builder.startObject("action_input")
        for ((k,v) <- t) action_input_builder.field(k,v)
        action_input_builder.endObject()
      case None => ;
    }
    document.state_data match {
      case Some(t) =>
        val state_data_builder : XContentBuilder = builder.startObject("state_data")
        for ((k,v) <- t) state_data_builder.field(k,v)
        state_data_builder.endObject()
      case None => ;
    }
    document.success_value match {
      case Some(t) => builder.field("success_value", t)
      case None => ;
    }
    document.failure_value match {
      case Some(t) => builder.field("failure_value", t)
      case None => ;
    }
    builder.endObject()

    val client: TransportClient = elasticClient.getClient()
    val response: UpdateResponse = client.prepareUpdate().setIndex(getIndexName(index_name))
      .setType(elasticClient.dtIndexSuffix).setId(id)
      .setDoc(builder)
      .get()

    if (refresh != 0) {
      val refresh_index = elasticClient.refreshIndex(getIndexName(index_name))
      if(refresh_index.failed_shards_n > 0) {
        throw new Exception("DecisionTable : index refresh failed: (" + index_name + ")")
      }
    }

    val doc_result: UpdateDocumentResult = UpdateDocumentResult(index = response.getIndex,
      dtype = response.getType,
      id = response.getId,
      version = response.getVersion,
      created = response.status == RestStatus.CREATED
    )

    Option {doc_result}
  }

  def deleteAll(index_name: String): Future[Option[DeleteDocumentsResult]] = Future {
    val client: TransportClient = elasticClient.getClient()
    val qb: QueryBuilder = QueryBuilders.matchAllQuery()
    val response: BulkByScrollResponse =
      DeleteByQueryAction.INSTANCE.newRequestBuilder(client).setMaxRetries(10)
        .source(getIndexName(index_name))
        .filter(qb)
        .get()

    val deleted: Long = response.getDeleted

    val result: DeleteDocumentsResult = DeleteDocumentsResult(message = "delete", deleted = deleted)
    Option {result}
  }

  def delete(index_name: String, id: String, refresh: Int): Future[Option[DeleteDocumentResult]] = Future {
    val client: TransportClient = elasticClient.getClient()
    val response: DeleteResponse = client.prepareDelete().setIndex(getIndexName(index_name))
      .setType(elasticClient.dtIndexSuffix).setId(id).get()

    if (refresh != 0) {
      val refresh_index = elasticClient.refreshIndex(getIndexName(index_name))
      if(refresh_index.failed_shards_n > 0) {
        throw new Exception("DecisionTable : index refresh failed: (" + index_name + ")")
      }
    }

    val doc_result: DeleteDocumentResult = DeleteDocumentResult(index = response.getIndex,
      dtype = response.getType,
      id = response.getId,
      version = response.getVersion,
      found = response.status != RestStatus.NOT_FOUND
    )

    Option {doc_result}
  }

  def getDTDocuments(index_name: String): Future[Option[SearchDTDocumentsResults]] = {
    val client: TransportClient = elasticClient.getClient()

    val qb : QueryBuilder = QueryBuilders.matchAllQuery()
    val scroll_resp : SearchResponse = client.prepareSearch(getIndexName(index_name))
      .setTypes(elasticClient.dtIndexSuffix)
      .setQuery(qb)
      .setScroll(new TimeValue(60000))
      .setSize(10000).get()

    //get a map of stateId -> AnalyzerItem (only if there is smt in the field "analyzer")
    val decisiontable_content : List[SearchDTDocument] = scroll_resp.getHits.getHits.toList.map({ e =>
      val item: SearchHit = e
      val state : String = item.getId
      val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

      val execution_order : Int = source.get("execution_order") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val max_state_count : Int = source.get("max_state_count") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val analyzer : String = source.get("analyzer") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val queries : List[String] = source.get("queries") match {
        case Some(t) => t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]]
          .asScala.map(_.getOrDefault("query", null)).filter(_ != null).toList
        case None => List[String]()
      }

      val bubble : String = source.get("bubble") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val action : String = source.get("action") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val action_input : Map[String,String] = source.get("action_input") match {
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String,String]()
      }

      val state_data : Map[String,String] = source.get("state_data") match {
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String,String]()
      }

      val success_value : String = source.get("success_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val failure_value : String = source.get("failure_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val document : DTDocument = DTDocument(state = state, execution_order = execution_order,
        max_state_count = max_state_count,
        analyzer = analyzer, queries = queries, bubble = bubble,
        action = action, action_input = action_input, state_data = state_data,
        success_value = success_value, failure_value = failure_value)

      val search_document : SearchDTDocument = SearchDTDocument(score = .0f, document = document)
      search_document
    }).sortBy(_.document.state)

    val max_score : Float = .0f
    val total : Int = decisiontable_content.length
    val search_results : SearchDTDocumentsResults = SearchDTDocumentsResults(total = total, max_score = max_score,
      hits = decisiontable_content)

    Future{Option{search_results}}
  }

  def read(index_name: String, ids: List[String]): Future[Option[SearchDTDocumentsResults]] = {
    val client: TransportClient = elasticClient.getClient()
    val multiget_builder: MultiGetRequestBuilder = client.prepareMultiGet()

    if (ids.nonEmpty) {
      multiget_builder.add(getIndexName(index_name), elasticClient.dtIndexSuffix, ids:_*)
    } else {
      val all_documents = getDTDocuments(index_name)
      return all_documents
    }

    val response: MultiGetResponse = multiget_builder.get()

    val documents : Option[List[SearchDTDocument]] = Option { response.getResponses
      .toList.filter((p: MultiGetItemResponse) => p.getResponse.isExists).map( { case(e) =>

      val item: GetResponse = e.getResponse

      val state : String = item.getId

      val source : Map[String, Any] = item.getSource.asScala.toMap

      val execution_order : Int = source.get("execution_order") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val max_state_count : Int = source.get("max_state_count") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val analyzer : String = source.get("analyzer") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val queries : List[String] = source.get("queries") match {
        case Some(t) => t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]]
          .asScala.map(_.getOrDefault("query", null)).filter(_ != null).toList
        case None => List[String]()
      }

      val bubble : String = source.get("bubble") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val action : String = source.get("action") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val actionInput : Map[String,String] = source.get("action_input") match {
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String,String]()
      }

      val stateData : Map[String,String] = source.get("state_data") match {
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String,String]()
      }

      val successValue : String = source.get("success_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val failureValue : String = source.get("failure_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val document : DTDocument = DTDocument(state = state, execution_order = execution_order,
        max_state_count = max_state_count,
        analyzer = analyzer, queries = queries, bubble = bubble,
        action = action, action_input = actionInput, state_data = stateData,
        success_value = successValue, failure_value = failureValue)

      val searchDocument : SearchDTDocument = SearchDTDocument(score = .0f, document = document)
      searchDocument
    }) }

    val filteredDoc : List[SearchDTDocument] = documents.getOrElse(List[SearchDTDocument]())

    val max_score : Float = .0f
    val total : Int = filteredDoc.length
    val searchResults : SearchDTDocumentsResults = SearchDTDocumentsResults(total = total, max_score = max_score,
      hits = filteredDoc)

    val searchResultsOption : Future[Option[SearchDTDocumentsResults]] = Future { Option { searchResults } }
    searchResultsOption
  }


  def indexCSVFileIntoDecisionTable(index_name: String, file: File, skiplines: Int = 1, separator: Char = ','):
  Future[Option[IndexDocumentListResult]] = {
    val documents: Try[Option[List[DTDocument]]] =
      Await.ready(
        Future{
          Option{
            FileToDTDocuments.getDTDocumentsFromCSV(log = log, file = file, skiplines = skiplines, separator = separator)
          }
        },
        30.seconds).value.get

    val document_list = documents match {
      case Success(t) =>
        t
      case Failure(e) =>
        val message = "error indexing CSV file, check syntax"
        log.error(message + " : " + e.getMessage)
        throw new Exception(message, e)
    }

    val indexDocumentListResult = if (document_list.isDefined) {
      val values = document_list.get.map(d => {
        val indexingResult: Try[Option[IndexDocumentResult]] =
          Await.ready(create(index_name, d, 1), 10.seconds).value.get

        indexingResult match {
          case Success(t) =>
            t.get
          case Failure(e) =>
            val message = "Cannot index document: " + d.state
            log.error(message + " : " + e.getMessage)
            throw new Exception(message, e)
        }
      })
      Option { IndexDocumentListResult(data = values) }
    } else {
      val message = "I could not index any document"
      log.error(message)
      throw new Exception(message)
    }

    Future { indexDocumentListResult }
  }

}
