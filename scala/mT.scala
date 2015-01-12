import java.util.Date

trait DataScheme {
    type Type <: List[Any]
    class ExtractorMethods(ticker: String, dataList: List[Type]) {
        def getDatetime(datum: Type): Date = new Date(datum(columnIndex(Names.datetime)).toString)
        def upperDatum(date: Date): Type = dataList.minBy(datum => getDatetime(datum) >= date)
        def lowerDatum(date: Date): Type = dataList.maxBy(datum => getDatetime(datum) <= date)
    }
}
trait IndexScheme extends DataScheme {
    type Type = (Date, Double, Double, Double, Double, Long)
    class ExtractorMethods(ticker: String, dataList: List[Type]) extends super.ExtractorMethods(ticker: String, dataList: List[Type]){
        def testing12(int: Int):Int = 12
        val test123 = 123
    }
}
class Data[+T <: DataScheme](val ticker: String, val dataList: List[T#Type], val isSorted: Boolean)
    (implicit m: Manifest[T], mm: Manifest[T#Type]) extends Symbols {
    def this(ticker: String, dataList: List[T#Type])(implicit m: Manifest[T], mm: Manifest[T#Type]) = this(ticker, dataList, false)(m: Manifest[T], mm: Manifest[T#Type])
    val dataScheme: T
    val extractorMethods = new dataScheme.ExtractorMethods(ticker, dataList.asInstanceOf[List[dataScheme.Type]])
}

val lowerDatum = sortedData.extractorMethods.lowerDatum(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse("2010-03-31 00:00:00"))

val testing = sortedData.extractorMethods.testing12(123)
