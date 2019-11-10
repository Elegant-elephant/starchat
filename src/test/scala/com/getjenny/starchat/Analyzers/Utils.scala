package com.getjenny.starchat.Analyzers

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Path, Paths}

object Utils {
  class Writer(fileName: String, headers: String) {

    val file = {
      val targetDir: Path = Paths.get(getClass.getClassLoader.getResource("").toURI).getParent.getParent
      val folderString = targetDir.toString() + "/analyzer_test_reports"
      val fileString = folderString + s"/$fileName.csv"
      val directory: File = new File(folderString)
      val file: File = new File(fileString)
      if(!directory.exists()) directory.mkdir()
      if(!file.exists()) file.createNewFile()
      file
    }
    val bw = new BufferedWriter(new FileWriter(file))
    write(headers)

    def write(lines: Seq[String]): Unit = {
      for (line <- lines) {
        bw.write(line + "\n")
      }
    }

    def write(line: String): Unit = {
      bw.write(line + "\n")
    }

    def close: Unit = bw.close()
  }

  def toMicro(nanoSeconds: Long): Long = nanoSeconds/1000

  def timeMicroSeconds[T](str: String)(thunk: => T): (T, Long) = {
    print(str + "... ")
    val t1 = System.nanoTime()
    val x = thunk
    val t2 = System.nanoTime()
    val t3 = toMicro(t2 - t1)
    println(t3 + " µsecs")
    (x, t3)
  }

}

