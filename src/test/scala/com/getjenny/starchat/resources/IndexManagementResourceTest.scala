package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.utils.Index

class IndexManagementResourceTest extends TestEnglishBase {

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
    "return an HTTP code 400 when trying to create again the same index" in {
      Post(s"/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "Elasticsearch exception \\[type=resource_already_exists_exception, reason=index \\[index_getjenny_.*\\] already exists\\]"
      }
    }
  }

  it should {
    "return an HTTP code 200 when calling elasticsearch index refresh" in {
      Post(s"/index_getjenny_english_0/index_management/refresh") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting informations from the index" in {
      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "IndexCheck: " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }


  /*it should {
    "return an HTTP code 200 updating the index" in {
      Put(s"/index_getjenny_english_0/english/index_management") ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }*/


  it should {
    "return an HTTP code 200 when deleting an existing index" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

  it should {
    "return an HTTP code 400 when deleting a non existing index" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

}


