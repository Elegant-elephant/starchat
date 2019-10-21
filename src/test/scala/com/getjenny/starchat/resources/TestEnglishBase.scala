package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities.IndexManagementResponse
import com.getjenny.starchat.utils.Index

trait TestEnglishBase extends TestBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Post(s"/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }

    Post(s"/index_getjenny_english_common_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    Delete(s"/index_getjenny_english_0/index_management") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }

    Delete(s"/index_getjenny_english_common_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

}
