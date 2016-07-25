object Timer{
  def time[R](block: => R): R = {  
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("\nElapsed time: " + (t1 - t0) / 1E9 + "s\n")
      result
  }
}
