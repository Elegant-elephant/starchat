package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.utils.Index

class NodeDtLoadingStatusResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 201 when creating a new user" in {
      val user = User(
        id = "test_user",
        password = "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9",
        salt = "salt",
        permissions = Map[String, Set[Permissions.Value]]("index_getjenny_english_0" -> Set(Permissions.read, Permissions.write))
      )
      Post(s"/user", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting node status for index" in {
      val strict = true
      Get(s"/index_getjenny_english_0/node_dt_update?strict=$strict") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterLoadingDtStatusIndex]
        print(responseEntity)
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting node status for all indices" in {
      val strict = true
      val verbose = true
      Get(s"/node_dt_update?strict=$strict&verbose=$verbose") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[NodeLoadingAllDtStatus]
        print(responseEntity)
      }
    }
  }

  it should {
    "return an HTTP code 200 when node status for an index" in {
      val request = NodeDtLoadingStatus(index = "index_getjenny_english_0")
      Post(s"/node_dt_update", request) ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting decision table loading records for dead nodes" in {
      Delete(s"/node_dt_update") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        println(response)
      }
    }
  }
}
