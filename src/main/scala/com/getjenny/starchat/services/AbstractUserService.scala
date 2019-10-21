package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 4/12/17.
  */

import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.auth.AbstractStarChatAuthenticator

import scala.concurrent.{ExecutionContext, Future}

trait AbstractUserService {
  implicit def executionContext: ExecutionContext = SCActorSystem.system.dispatchers.lookup("starchat.dispatcher")
  def create(user: User): IndexDocumentResult
  def update(user: UserUpdate): UpdateDocumentResult
  def delete(user: UserId): DeleteDocumentResult
  def read(user: UserId): User
  def genUser(user: UserUpdate, authenticator: AbstractStarChatAuthenticator): User
}
