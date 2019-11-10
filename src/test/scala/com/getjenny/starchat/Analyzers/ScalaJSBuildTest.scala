package com.getjenny.starchat.Analyzers

import com.getjenny.starchat.Analyzers.Utils._
import com.getjenny.starchat.analyzer.analyzers.script_support.scalajs.ScalaJSAnalyzerBuilder
import org.scalatest.{BeforeAndAfterAll, WordSpec}

import scala.io.Source

class ScalaJSBuildTest extends WordSpec with BeforeAndAfterAll {

  val writer = new Writer("sjs_analyzer_build_test", "sjs_build_time µs")

  override def afterAll {
    writer.close
  }

  val sjsAnalyzerScript = Source.fromResource("test_data/test.scala").getLines().mkString("\n")
  val iterations = 500

  "Analyzer" should {
    "evaluate ScalaJS analyzers" in {
      val sjsTimes: Seq[Long] = for(i <- 1 to iterations) yield {
        val (result, buildTime) = timeMicroSeconds("Building") {
          ScalaJSAnalyzerBuilder.build(sjsAnalyzerScript, Map.empty)
        }
        buildTime
      }
      writer.write(sjsTimes.map(_.toString()))
    }
  }
}
