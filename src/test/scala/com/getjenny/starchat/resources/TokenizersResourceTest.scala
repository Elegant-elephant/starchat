package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.utils.Index

class TokenizersResourceTest extends TestEnglishBase {

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
    "return an HTTP code 200 when listing all tokenizers" in {
      Get(s"/index_getjenny_english_0/tokenizers") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Map[String, String]]
      }
    }
  }

  it should {
    "return an HTTP code 200 when extracting tokens from text" in {
      val text = "a quick brown fox"
      val tokenizer = "base_stem"
      val request = TokenizerQueryRequest(tokenizer = tokenizer, text = text)
      Post(s"/index_getjenny_english_0/tokenizers", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[TokenizerResponse]
        response.tokens.length shouldBe 4
        response.tokens.map(_.token) shouldEqual text.split(" ").toSeq
      }
    }

    "return an HTTP code 400 when tokenizer not supported" in {
      val text = "a quick brown fox"
      val tokenizer = "unknown"
      val request = TokenizerQueryRequest(tokenizer = tokenizer, text = text)
      Post(s"/index_getjenny_english_0/tokenizers", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
      }
    }
  }

}
