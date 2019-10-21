package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 10/03/17.
  */

import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{LanguageGuesserInformations, LanguageGuesserRequestIn, LanguageGuesserRequestOut}
import org.apache.tika.langdetect.OptimaizeLangDetector
import org.apache.tika.language.detect.{LanguageDetector, LanguageResult}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Implements functions, eventually used by LanguageGuesserResource
  */
object LanguageGuesserService {
  implicit def executionContext: ExecutionContext = SCActorSystem.system.dispatcher
  def guessLanguage(indexName: String, requestData: LanguageGuesserRequestIn): LanguageGuesserRequestOut = {
    val detector: LanguageDetector = new OptimaizeLangDetector().loadModels()
    val result: LanguageResult = detector.detect(requestData.inputText)

    LanguageGuesserRequestOut(result.getLanguage, result.getRawScore,
      result.getConfidence.name,
      detector.hasEnoughText
    )
  }

  def getLanguages(indexName: String, languageCode: String /*ISO 639-1 name for language*/):LanguageGuesserInformations = {
    val detector: LanguageDetector = new OptimaizeLangDetector().loadModels()
    val hasModel: Boolean = detector.hasModel(languageCode)
    LanguageGuesserInformations(
      Map[String,Map[String,Boolean]](
        "languages" -> Map[String, Boolean](languageCode -> hasModel))
    )
  }
}
