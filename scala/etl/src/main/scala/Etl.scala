object Etl {
    def transform(mapping :Map[Int,Seq[String]]) :Map[String, Int]  =  {
        mapping.flatMap {
            case (point,letters) => letters.map {letter => letter.toLowerCase -> point}
        }.toSeq.sortBy(_._1).toMap
    }
}