package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._

class ClusterNodesResourceTest extends TestBase {

  "StarChat" should {
    "return an HTTP code 200 when updating alive timestamp for the node" in {
      Post(s"/cluster_node") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterNode]
        response.alive shouldBe true
      }
    }
  }

  it should {
    "return an HTTP code 200 when retrieving information about a node" in {
      Post(s"/cluster_node") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        val response = responseAs[ClusterNode]
        response.uuid
      }.andThen(uuid => {
        Get(s"/cluster_node/$uuid") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[ClusterNode]
          response.uuid shouldEqual uuid
          response.alive shouldBe true
        }
      })
    }
  }

  it should {
    "return an HTTP code 200 when getting alive nodes" in {
      Get(s"/cluster_node") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterNodes]
      }
    }
  }

  it should {
    "return an HTTP code 200 when removing dead nodes" in {
      Delete(s"/cluster_node") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        response.message shouldEqual "delete"
      }
    }
  }

}
