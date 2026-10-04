trait Base:
  def captureOut[T](body: => T): (T, List[String]) =
    val bytes = new java.io.ByteArrayOutputStream
    val stream = new java.io.PrintStream(bytes)
    val value = try Console.withOut(stream)(body) finally stream.close()
    (value, bytes.toString("UTF-8").linesIterator.toList)