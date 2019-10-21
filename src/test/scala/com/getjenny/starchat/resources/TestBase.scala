package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.getjenny.starchat.StarChatService
import com.getjenny.starchat.entities.IndexManagementResponse
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import scala.concurrent.duration._
import akka.testkit._

trait TestBase extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport with BeforeAndAfterAll {

  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(30.seconds.dilated(system))
  val service: StarChatService = TestFixtures.service
  val routes: Route = service.routes

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")
  val testUserCredentials = BasicHttpCredentials("test_user", "p4ssw0rd")

  override protected def beforeAll(): Unit = {
    Post(s"/system_index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

  override protected def afterAll(): Unit = {
    Delete(s"/system_index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }
}
