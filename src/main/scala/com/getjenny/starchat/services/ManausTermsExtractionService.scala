package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/05/18.
  */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.util.VectorUtils
import com.getjenny.manaus.{KeywordsExtraction, TokenOccurrence}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.utils.{EMDVectorDistances, SumVectorDistances, TextToVectorsTools}
import com.getjenny.starchat.entities.{TermsExtractionRequest, _}
import com.getjenny.starchat.services.esclient.ManausTermsExtractionElasticClient
import com.getjenny.starchat.utils.Index

import scala.collection.immutable.Map
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ManausTermsExtractionService {
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val elasticClient: ManausTermsExtractionElasticClient.type = ManausTermsExtractionElasticClient
  private[this] val termService: TermService.type = TermService
  private[this] val priorDataService: PriorDataService.type = PriorDataService
  private[this] val convLogDataService: ConversationLogsService.type = ConversationLogsService
  private[this] val knowledgeBaseService: KnowledgeBaseService.type = KnowledgeBaseService

  class PriorTokenOccurrenceMap(indexName: String,
                                commonOrSpecificSearch: CommonOrSpecificSearch.Value,
                                field: TermCountFields.Value) extends TokenOccurrence {

    private[this] val idxName: String = commonOrSpecificSearch match {
      case CommonOrSpecificSearch.COMMON =>
        Index.getCommonIndexName(indexName)
      case _ => indexName
    }

    override def tokenOccurrence(word: String): Long = {
      field match {
        case TermCountFields.question =>
          val termCount = priorDataService.termCount(indexName = idxName, field = TermCountFields.question,
            term = word)
          termCount.count
        case TermCountFields.answer =>
          val termCount = priorDataService.termCount(indexName = idxName, field = TermCountFields.answer, term = word)
          termCount.count
        case _ =>
          val termCountQ = priorDataService.termCount(indexName = idxName,
            field = TermCountFields.question, term = word)
          val termCountA = priorDataService.termCount(indexName = idxName,
            field = TermCountFields.answer, term = word)
          termCountQ.count + termCountA.count
      }
    }

    override def totalNumberOfTokens: Long = {
      val numOfTerms = priorDataService.totalTerms(idxName)
      field match {
        case TermCountFields.question =>
          numOfTerms.question
        case TermCountFields.answer =>
          numOfTerms.answer
        case _ =>
          numOfTerms.question + numOfTerms.answer
      }
    }
  }

  class ObservedTokenOccurrenceMap(indexName: String,
                                   commonOrSpecificSearch: CommonOrSpecificSearch.Value,
                                   observedDataSource: ObservedDataSources.Value,
                                   field: TermCountFields.Value) extends TokenOccurrence {

    private[this] val idxName: String = commonOrSpecificSearch match {
      case CommonOrSpecificSearch.COMMON =>
        Index.getCommonIndexName(indexName)
      case _ => indexName
    }

    private[this] val dataService: QuestionAnswerService = observedDataSource match {
      case ObservedDataSources.KNOWLEDGEBASE =>
        knowledgeBaseService
      case _ =>
        convLogDataService
    }

    override def tokenOccurrence(word: String): Long = {
      field match {
        case TermCountFields.question =>
          val termCount = dataService.termCount(indexName = idxName, field = TermCountFields.question,
            term = word)
          termCount.count
        case TermCountFields.answer =>
          val termCount = dataService.termCount(indexName = idxName, field = TermCountFields.answer, term = word)
          termCount.count
        case _ =>
          val termCountQ = dataService.termCount(indexName = idxName,
            field = TermCountFields.question, term = word)
          val termCountA = dataService.termCount(indexName = idxName,
            field = TermCountFields.answer, term = word)
          termCountQ.count + termCountA.count
      }
    }

    override def totalNumberOfTokens: Long = {
      val numOfTerms = dataService.totalTerms(idxName)
      field match {
        case TermCountFields.question =>
          numOfTerms.question
        case TermCountFields.answer =>
          numOfTerms.answer
        case _ =>
          numOfTerms.question + numOfTerms.answer
      }
    }
  }

  private[this] def extractKeywords(sentenceTokens: List[String],
                                    observedOccurrences: TokenOccurrence,
                                    priorOccurrences: TokenOccurrence,
                                    minWordsPerSentence: Int,
                                    pruneTermsThreshold: Int,
                                    misspellMaxOccurrence: Int,
                                    activePotentialDecay: Int,
                                    minSentenceInfoBit: Int = 16,
                                    minKeywordInfo: Int = 8,
                                    totalInfo: Boolean,
                                    activePotential: Boolean): (List[String], Map[String, Double]) = {

    val keywordsExtraction = new KeywordsExtraction(priorOccurrences=priorOccurrences,
      observedOccurrences=observedOccurrences)

    val freqData: String = sentenceTokens.map { case(e) =>
      "word(" + e + ") -> observedOccurrences(" + observedOccurrences.tokenOccurrence(e) + ") priorOccurrences(" +
        priorOccurrences.tokenOccurrence(e) + ") totalNumberOfObservedTokens(" +
        observedOccurrences.totalNumberOfTokens + ") totalNumberOfObservedTokens(" +
        priorOccurrences.totalNumberOfTokens + ")"
    }.mkString(" ; ")

    log.debug("SentenceFrequencies: " + freqData)

    /* Informative words */
    val rawBagOfKeywordsInfo: List[(String, Double)] =
      keywordsExtraction.extractInformativeWords(sentence = sentenceTokens,
        pruneSentence = pruneTermsThreshold,
        minWordsPerSentence = minWordsPerSentence,
        minSentenceInfoBit = minSentenceInfoBit,
        minKeywordInfo = minKeywordInfo,
        totalInformationNorm = totalInfo)

    /* Map(keyword -> active potential) */
    val activePotentialKeywordsMap = keywordsExtraction.getWordsActivePotentialMapForSentence(rawBagOfKeywordsInfo,
      activePotentialDecay)

    val informativeKeywords: (List[String], List[(String, Double)]) = (sentenceTokens, rawBagOfKeywordsInfo)

    // list of the final keywords
    val bags: (List[String], Map[String, Double]) =
      if(activePotential) {
        keywordsExtraction.extractBagsActiveForSentence(activePotentialKeywordsMap = activePotentialKeywordsMap,
          informativeKeywords = informativeKeywords, misspellMaxOccurrence = misspellMaxOccurrence)
      } else {
        keywordsExtraction.extractBagsNoActiveForSentence(informativeKeywords = informativeKeywords,
          misspellMaxOccurrence = misspellMaxOccurrence)
      }
    bags
  }

  private[this] def initTokenOccurrence(indexName: String,
                                        extractionRequest: TermsExtractionRequest): (TokenOccurrence, TokenOccurrence) = {
    val priorOccurrences: TokenOccurrence = new PriorTokenOccurrenceMap(indexName = indexName,
      commonOrSpecificSearch = extractionRequest.commonOrSpecificSearchPrior.getOrElse(CommonOrSpecificSearch.COMMON),
      field = extractionRequest.fieldsPrior.getOrElse(TermCountFields.all))

    val observedOccurrences: TokenOccurrence = new ObservedTokenOccurrenceMap(indexName: String,
      commonOrSpecificSearch = extractionRequest.commonOrSpecificSearchObserved
        .getOrElse(CommonOrSpecificSearch.IDXSPECIFIC),
      observedDataSource = extractionRequest.observedDataSource.getOrElse(ObservedDataSources.KNOWLEDGEBASE),
      field = extractionRequest.fieldsObserved.getOrElse(TermCountFields.all))

    (priorOccurrences, observedOccurrences)
  }

  private[this] def tokenize(indexName: String, extractionRequest: TermsExtractionRequest): TokenizerResponse = {
    val tokenizerReq = TokenizerQueryRequest(extractionRequest.tokenizer.getOrElse("base"),
      extractionRequest.text)

    val tokens: TokenizerResponse = termService.esTokenizer(indexName, tokenizerReq) match {
      case Some(t) => t
      case _ => TokenizerResponse(tokens = List.empty[TokenizerResponseItem])
    }

    tokens
  }

  def termFrequency(indexName: String, extractionRequest: TermsExtractionRequest): TokenFrequency = {
    val tokens = tokenize(indexName, extractionRequest)
    val (priorOccurrences, observedOccurrences) = initTokenOccurrence(indexName, extractionRequest)
    val freqItems = tokens.tokens.map(_.token).distinct.map { case (token) =>
      TokenFrequencyItem(
        token = token,
        priorFrequency = priorOccurrences.tokenOccurrence(token),
        observedFrequency = observedOccurrences.tokenOccurrence(token)
      )
    }

    TokenFrequency( tokensFreq = freqItems,
      priorTotalTerms = priorOccurrences.totalNumberOfTokens,
      observedTotalTerms = observedOccurrences.totalNumberOfTokens
    )
  }

  def termFrequencyFuture(indexName: String, extractionRequest: TermsExtractionRequest):
  Future[TokenFrequency] = Future {
    termFrequency(indexName, extractionRequest)
  }

  def textTerms(indexName: String,
                extractionRequest: TermsExtractionRequest
               ): (TokenizerResponse, Map[String, Double]) = {

    val tokens = tokenize(indexName, extractionRequest)
    val (priorOccurrences, observedOccurrences) = initTokenOccurrence(indexName, extractionRequest)

    log.debug("ExtractionRequest:" + extractionRequest)

    val bags = extractKeywords(sentenceTokens = tokens.tokens.map(_.token),
      observedOccurrences = observedOccurrences,
      priorOccurrences = priorOccurrences,
      minWordsPerSentence = extractionRequest.minWordsPerSentence.getOrElse(5),
      pruneTermsThreshold = extractionRequest.pruneTermsThreshold.getOrElse(100000),
      misspellMaxOccurrence = extractionRequest.misspellMaxOccurrence.getOrElse(5),
      activePotentialDecay = extractionRequest.activePotentialDecay.getOrElse(10),
      activePotential = extractionRequest.activePotential.getOrElse(true),
      minSentenceInfoBit = extractionRequest.minSentenceInfoBit.getOrElse(16),
      minKeywordInfo = extractionRequest.minKeywordInfo.getOrElse(8),
      totalInfo = extractionRequest.totalInfo.getOrElse(false))
    (tokens, bags._2)
  }

  def textTermsFuture(indexName: String,
                      extractionRequest: TermsExtractionRequest
                     ): Future[(TokenizerResponse, Map[String, Double])] = Future {
    textTerms(indexName = indexName, extractionRequest = extractionRequest)
  }

  def termsSynonyms(indexName: String,
                    extractionRequest: SynExtractionRequest
                   ): List[SynonymExtractionItem] = {

    // preparing the terms extraction request
    val termsExtractionRequest = TermsExtractionRequest(
      text = extractionRequest.text,
      tokenizer = extractionRequest.tokenizer,
      commonOrSpecificSearchPrior = extractionRequest.commonOrSpecificSearchPrior,
      commonOrSpecificSearchObserved = extractionRequest.commonOrSpecificSearchObserved,
      observedDataSource = extractionRequest.observedDataSource,
      fieldsPrior = extractionRequest.fieldsPrior,
      fieldsObserved = extractionRequest.fieldsObserved,
      minWordsPerSentence =  extractionRequest.minWordsPerSentence,
      pruneTermsThreshold = extractionRequest.pruneTermsThreshold,
      misspellMaxOccurrence = extractionRequest.misspellMaxOccurrence,
      activePotentialDecay = extractionRequest.activePotentialDecay,
      activePotential = extractionRequest.activePotential,
      minSentenceInfoBit = extractionRequest.minSentenceInfoBit,
      minKeywordInfo = extractionRequest.minKeywordInfo,
      totalInfo = extractionRequest.totalInfo
    )

    // extract manaus terms
    val (tokenizationRes, manausKeywords) = textTerms(indexName, termsExtractionRequest)
    log.info("ManausTermsExtraction: " + tokenizationRes)

    log.debug("Terms extraction Request: " + extractionRequest)

    // calculate source index name for the terms (vectorial representation)
    val termsIndexName = extractionRequest.commonOrSpecificSearchTerms match {
      case CommonOrSpecificSearch.IDXSPECIFIC =>
        indexName
      case _ =>
        Index.getCommonIndexName(indexName)
    }

    // extraction of vectorial terms representation
    val tokenTermsId: Set[String] = tokenizationRes.tokens.map(_.token).toSet // all tokens
    val extractedSentenceTerms = termService.termsById(termsIndexName, TermIdsRequest(ids = tokenTermsId.toList))
    val tokenTerms = extractedSentenceTerms match {
      case Some(terms) => terms.terms.map { case(t) => (t.term, t) }.toMap
      case _ => Map.empty[String, Term]
    }

    // extraction of vectorial synonyms representation, exclude terms already in tokens (used as a cache)
    val synsTermsId = tokenTerms.map { case(_, term) =>
      term.synonyms match {
        case Some(synList) => synList.keys.toSet
        case _ => Set.empty[String]
      }
    }.toList.flatten.filter(! tokenTermsId.contains(_)).toSet
    val extractedSynsTerms = termService.termsById(termsIndexName, TermIdsRequest(ids = synsTermsId.toList))
    val synsTerms = extractedSynsTerms match {
      case Some(terms) => terms.terms.map { case(t) => (t.term, t) }.toMap
      case _ => Map.empty[String, Term]
    }

    // token and synonyms terms map
    val allTerms: Map[String, Term] = tokenTerms ++ synsTerms

    val numberOfTokens = tokenizationRes.tokens.length

    // calculate the vector representation for the sentence
    val sentenceVectors = tokenizationRes.tokens.map{ case(token) =>
      allTerms.get(token.token) match {
        case Some(t) => (token.token, t.vector)
        case _ => (token.token, None)
      }
    }.filter(_._2.nonEmpty).map { case (t) => t._2.get}.toVector
    val termsInSentence = sentenceVectors.length

    val indexedTokenizationRes = tokenizationRes.tokens.zipWithIndex

    val baseSentenceTextTerms = TextTerms(
      text = "",
      text_terms_n = numberOfTokens,
      terms_found_n = termsInSentence,
      terms = extractedSentenceTerms)

    // iterate over tokens and calculate the synonyms score
    indexedTokenizationRes.map { case(token, index) =>
      // getting current token and rest of the sentence tokens
      val currentTokenTerm = allTerms.get(token.token)
      val restOfTheListTerms = indexedTokenizationRes.filter(_._2 != index).map { case(t) =>
        allTerms.get(t._1.token)
      }.filter(_.nonEmpty).map(_.get).filter(_.vector.nonEmpty)
      // calculating vector representation of the sentence with the current token replaced by a synonym
      val replacedTokenInSentence = currentTokenTerm match {
        case Some(t) =>
          t.vector match {
            case Some(_) =>
              // take all the synonyms and discard those without a vector representation
              val syns = t.synonyms.getOrElse(Map.empty[String, Double]).keys
                .map { case(s) => allTerms.get(s) }.filter(_.nonEmpty).map(_.get)
                .filter(_.vector.nonEmpty).toList

              syns.map { case(s) =>
                val synSentenceTerms = s :: restOfTheListTerms
                val synSentenceTermsLength = synSentenceTerms.length
                val synSentenceTextTerms = TextTerms(
                  text = "",
                  text_terms_n = numberOfTokens,
                  terms_found_n = synSentenceTermsLength,
                  terms = Some(Terms(terms = synSentenceTerms)))

                val sentencesDistance = extractionRequest.distanceFunction match {
                  case SynonymExtractionDistanceFunction.EMDCOSINE =>
                    EMDVectorDistances.distanceCosine(Some(baseSentenceTextTerms), Some(synSentenceTextTerms))
                  case SynonymExtractionDistanceFunction.SUMCOSINE =>
                    SumVectorDistances.distanceCosine(Some(baseSentenceTextTerms), Some(synSentenceTextTerms))
                  case _ =>
                    EMDVectorDistances.distanceCosine(Some(baseSentenceTextTerms), Some(synSentenceTextTerms))
                }

                (t, s, sentencesDistance)
              }
            case _ =>
              List.empty[(Term, Term, Double)]
          }
        case _ => List.empty[(Term, Term, Double)]
      }

      val synItems = replacedTokenInSentence.map { case(term, synonym, distance) =>
        val termsDistance = VectorUtils.cosineDist(
          term.vector.getOrElse(TextToVectorsTools.emptyVec(term.vector.get.length)), synonym.vector.get)

        SynonymItem(
          synonym = synonym.term,
          synonymScore = distance,
          termSimilarityScore = termsDistance,
          textDistanceWithSynonym = distance
        )
      }.filter(_.synonymScore > extractionRequest.sentencesThreshold.getOrElse(0.0d))
        .filter(_.termSimilarityScore > extractionRequest.synonymsThreshold.getOrElse(0.0d))
        .sortWith((a, b) => a.synonymScore >= b.synonymScore)

      SynonymExtractionItem(
        token = token,
        isKeywordToken = manausKeywords.contains(token.token),
        keywordExtractionScore = manausKeywords.getOrElse(token.token, 0.0d),
        synonymItem = synItems
      )
    }
  }

  def termsSynonymsFuture(indexName: String,
                          extractionRequest: SynExtractionRequest
                         ): Future[List[SynonymExtractionItem]] = Future {
    termsSynonyms(indexName = indexName, extractionRequest = extractionRequest)
  }

}
