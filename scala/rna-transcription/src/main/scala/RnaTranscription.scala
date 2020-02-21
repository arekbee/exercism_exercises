object RnaTranscription {

    def convert : PartialFunction[Char,Char] = {
        case 'G' => 'C'
        case 'C' => 'G'
        case 'T' => 'A'
        case 'A' => 'U'
        case c => c
    } 

    def toRna : PartialFunction[String,Option[String]] = {
        case "" => None
        case dna => Some(dna.toUpperCase.map(convert(_)))
        }
    }