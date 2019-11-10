
val Result = g.Java.applyDynamic("type")("com.getjenny.analyzer.expressions.Result")

val keyword = "password"
val rx = {"""\b""" + keyword + """\b"""}.r

val freq = rx.findAllIn(sentence).toList.length
val queryLength = """\S+""".r.findAllIn(sentence).toList.length


val score = if(queryLength > 0)
  freq.toDouble / queryLength.toDouble
else
  0.0

Result.applyDynamic("apply")(score, analyzersDataInternal)

