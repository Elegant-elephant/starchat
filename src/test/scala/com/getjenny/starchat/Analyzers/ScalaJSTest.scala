package com.getjenny.starchat.Analyzers

import com.getjenny.starchat.Analyzers.Utils._
import com.getjenny.starchat.analyzer.analyzers.script_support.scalajs.ScalaJSAnalyzerBuilder
import org.scalatest.{BeforeAndAfterAll, WordSpec}

import scala.io.Source

class ScalaJSTest extends WordSpec with BeforeAndAfterAll {

  val writer = new Writer("sjs_analyzer_test", "sjs_eval_time µs")

  override def afterAll {
    writer.close
  }

  val sjsAnalyzerScript = Source.fromResource("test_data/KeywordAnalyzerScalaJS.scala").getLines().mkString("\n")
  val sjsAnalyzer = ScalaJSAnalyzerBuilder.build(sjsAnalyzerScript, Map.empty)
  val sentence = "I forgot my password"
  val iterations = 1000

  "Analyzer" should {
    "evaluate ScalaJS analyzers" in {
      val sjsTimes: Seq[Long] = for(i <- 1 to iterations) yield {
        val (result, evalTime) = timeMicroSeconds("Evaluating") {
          sjsAnalyzer.evaluate(sentence)
        }
        assert(result.score === 0.25)
        evalTime
      }

      writer.write(sjsTimes.map(_.toString()))
    }
  }
}
