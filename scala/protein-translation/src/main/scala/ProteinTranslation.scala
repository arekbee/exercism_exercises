object  ProteinTranslation {
    def getProtein :PartialFunction[String,String] = {
        case "AUG"	=>          "Methionine"
        case "UUU"| "UUC" =>    "Phenylalanine"
        case "UUA"| "UUG" =>    "Leucine"
        case "UCU"| "UCC" | "UCA" | "UCG" => "Serine"
        case "UAU"| "UAC" 	=>  "Tyrosine"
        case "UGU"| "UGC" 	=>  "Cysteine"
        case "UGG"	=>          "Tryptophan"
        case "UAA"| "UAG" | "UGA" =>	"STOP"
    }

    def proteins(str:String) :Seq[String] = {
        str.toList.grouped(3).map(_.mkString).map(getProtein(_)).takeWhile(_ != "STOP").toSeq
    }
}