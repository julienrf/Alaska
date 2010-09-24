package alaska

object Main {

  def main(args: Array[String]): Unit = {
    val alaska = new Alaska(new PrefixedPrinterFactory)
    println(alaska process ExprParser("5-4"))
  }
  
}
