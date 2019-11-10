package com.getjenny.starchat.Analyzers

import com.getjenny.starchat.analyzer.analyzers.GetJennyAnalyzerBuilder
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import Utils._

class DslTest extends WordSpec with BeforeAndAfterAll {

  val writer = new Writer("dsl_analyzer_test", "dsl_eval_time µs")

  override def afterAll {
    writer.close
  }

  val dslAnalyzerScript = "vOneKeyword(\"password\")"
  val dslAnalyzer = GetJennyAnalyzerBuilder.build(dslAnalyzerScript, Map.empty)
  val sentence = "I forgot my password"
  val iterations = 1000

  "Analyzer" should {
    "evaluate dsl analyzers" in {
      val dslTimes: Seq[Long] = for(i <- 1 to iterations) yield {
        val (result, evalTime) = timeMicroSeconds("Evaluating") {
          dslAnalyzer.evaluate(sentence)
        }
        assert(result.score === 0.25)
        evalTime
      }

      writer.write(dslTimes.map(_.toString()))
    }
  }
}
