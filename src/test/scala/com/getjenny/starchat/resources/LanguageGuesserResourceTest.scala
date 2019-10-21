package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import com.getjenny.starchat.entities._
import com.getjenny.starchat.utils.Index

class LanguageGuesserResourceTest extends TestEnglishBase {

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

  val languageIterator: Iterator[(String, String)] = {
    val input_file = getClass.getResourceAsStream("/test_data/language_guesser_test_parameters.csv")
    val input_data = scala.io.Source.fromInputStream(input_file, "UTF-8").getLines
    input_data.next() //skip column names
    new Iterator[(String, String)] {
      override def hasNext: Boolean = input_data.hasNext
      override def next(): (String, String) = {
        val line = input_data.next().split(",")
        (line(0), line(1))
      }
    }
  }

  for((language, sentence) <- languageIterator) {
    it should {
      s"return an HTTP code 200 when checking if '$language' is supported" in {
        Get(s"/index_getjenny_english_0/language_guesser/$language") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[LanguageGuesserInformations]
          response.supportedLanguages should (contain key "languages" and contain value Map(language -> true))
        }
      }
    }

    it should {
      val languageGuesserRequestIn: LanguageGuesserRequestIn = LanguageGuesserRequestIn(
        inputText = sentence
      )

      s"return an HTTP code 200 when guessing '$language' language" in {
        Post(s"/index_getjenny_english_0/language_guesser", languageGuesserRequestIn) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[LanguageGuesserRequestOut]
          response.language should be(language)
        }
      }
    }
  }

  it should {
    val languageGuesserRequestIn: LanguageGuesserRequestIn = LanguageGuesserRequestIn(
      inputText = "I am unauthorized"
    )
    val unauthorizedUserCredentials = BasicHttpCredentials("jack", "sparrow")

    s"return an HTTP code 401 when unauthorized" in {
      Post(s"/index_getjenny_english_0/language_guesser", languageGuesserRequestIn) ~> addCredentials(unauthorizedUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
    }
  }

}
