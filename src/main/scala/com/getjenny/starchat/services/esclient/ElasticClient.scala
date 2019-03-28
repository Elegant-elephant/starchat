package com.getjenny.starchat.services.esclient

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import java.io.{FileInputStream, InputStream}
import java.net.InetAddress
import java.security.{KeyStore, SecureRandom}

import com.getjenny.starchat.entities._
import com.typesafe.config.{Config, ConfigFactory}
import javax.net.ssl._
import org.apache.http.HttpHost
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder
import org.elasticsearch.action.admin.indices.refresh.{RefreshRequest, RefreshResponse}
import org.elasticsearch.client.indices._
import org.elasticsearch.client.{RequestOptions, RestClient, RestClientBuilder, RestHighLevelClient}
import scalaz.Scalaz._

import scala.collection.immutable.{List, Map}

trait ElasticClient {
  val config: Config = ConfigFactory.load()
  val clusterName: String = config.getString("es.cluster_name")
  val sniff: Boolean = config.getBoolean("es.enable_sniff")
  val ignoreClusterName: Boolean = config.getBoolean("es.ignore_cluster_name")
  val numberOfShards: Int = config.getInt("es.number_of_shards")
  val numberOfReplicas: Int = config.getInt("es.number_of_replicas")

  val hostProto : String = config.getString("es.host_proto")
  val hostMapStr : String = config.getString("es.host_map")
  val hostMap : Map[String, Int] = hostMapStr.split(";")
    .map(x => x.split("=")).map(x => (x(0), x(1).toInt)).toMap

  val keystore: String = config.getString("starchat.client.https.keystore")
  val keystoreType: String = config.getString("starchat.client.https.keystore_type")
  val keystorePassword: String = config.getString("starchat.client.https.keystore_password")
  val disableHostValidation: Boolean = config.getBoolean("starchat.client.https.disable_host_validation")

  val inetAddresses: List[HttpHost] =
    hostMap.map{ case(k,v) => new HttpHost(InetAddress.getByName(k), v, hostProto) }.toList


  object AllHostsValid extends HostnameVerifier {
    def verify(hostname: String, session: SSLSession) = true
  }

  object HttpClientConfigCallback extends RestClientBuilder.HttpClientConfigCallback {
    override def customizeHttpClient(httpClientBuilder: HttpAsyncClientBuilder): HttpAsyncClientBuilder = {
      val httpBuilder = httpClientBuilder.setSSLContext(sslContext)
      if(disableHostValidation)
        httpBuilder.setSSLHostnameVerifier(AllHostsValid)
      httpBuilder
    }
  }

  def sslContext: SSLContext = {
    val password = if(keystorePassword.nonEmpty)
      keystorePassword.toCharArray
    else null

    val ks = KeyStore.getInstance(keystoreType)

    val keystoreIs: InputStream = if(keystore.startsWith("/tls/certs/")) {
      getClass.getResourceAsStream(keystore)
    } else {
      new FileInputStream(keystore)
    }

    require(keystoreIs != None.orNull, "Keystore required!")
    ks.load(keystoreIs, password)

    val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, password)

    val tmf = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ks)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
    sslContext
  }

  def buildClient(hostProto: String): RestClientBuilder = {
    if(hostProto === "http") {
      RestClient.builder(inetAddresses:_*)
    } else {
      RestClient.builder(inetAddresses:_*)
        .setHttpClientConfigCallback(HttpClientConfigCallback)
    }
  }

  private[this] var esHttpClient : RestHighLevelClient = openHttp()

  def openHttp(): RestHighLevelClient = {
    val client: RestHighLevelClient = new RestHighLevelClient(
      buildClient(hostProto)
    )
    client
  }

  def refresh(indexName: String): RefreshIndexResult = {
    val refreshReq = new RefreshRequest(indexName)
    val refreshRes: RefreshResponse =
      esHttpClient.indices().refresh(refreshReq, RequestOptions.DEFAULT)

    val failedShards: List[FailedShard] = refreshRes.getShardFailures.map(item => {
      val failedShardItem = FailedShard(indexName = item.index,
        shardId = item.shardId,
        reason = item.reason,
        status = item.status.getStatus
      )
      failedShardItem
    }).toList

    val refreshIndexResult =
      RefreshIndexResult(indexName = indexName,
        failedShardsN = refreshRes.getFailedShards,
        successfulShardsN = refreshRes.getSuccessfulShards,
        totalShardsN = refreshRes.getTotalShards,
        failedShards = failedShards
      )
    refreshIndexResult
  }

  def httpClient: RestHighLevelClient = {
    this.esHttpClient
  }

  def sqlClient: RestHighLevelClient = {
    this.esHttpClient
  }

  def existsIndices(indices: List[String]): Boolean = {
    val request = new GetIndexRequest(indices:_*)
    request.local(false)
    request.humanReadable(true)
    request.includeDefaults(false)
    httpClient.indices().exists(request, RequestOptions.DEFAULT)
  }

  def closeHttp(client: RestHighLevelClient): Unit = {
    client.close()
  }

  val indexName: String
  val indexSuffix: String
  val indexMapping: String

  val commonIndexArbitraryPattern: String = config.getString("es.common_index_arbitrary_pattern")
  val commonIndexDefaultOrgPattern: String = config.getString("es.common_index_default_org_pattern")

  val convLogsIndexSuffix: String = config.getString("es.logs_data_index_suffix")
  val dtIndexSuffix: String = config.getString("es.dt_index_suffix")
  val kbIndexSuffix: String = config.getString("es.kb_index_suffix")
  val priorDataIndexSuffix: String = config.getString("es.prior_data_index_suffix")
  val sysRefreshDtIndexSuffix: String = config.getString("es.system_refresh_dt_index_suffix")
  val systemRefreshDtIndexSuffix: String = config.getString("es.system_refresh_dt_index_suffix")

  val systemClusterNodesIndexSuffix: String = "cluster_nodes"
  val systemDtNodesStatusIndexSuffix: String = "decision_table_node_status"

  val termIndexSuffix: String = config.getString("es.term_index_suffix")
  val feedbackIndexSuffix: String = config.getString("es.feedback_index_suffix")
  val userIndexSuffix: String = config.getString("es.user_index_suffix")

  val enableDeleteIndex: Boolean = config.getBoolean("es.enable_delete_application_index")
  val enableDeleteSystemIndex: Boolean = config.getBoolean("es.enable_delete_system_index")
}
