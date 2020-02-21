object Strain {
    
    def keep[T](input:Seq[T], predicat : T=>Boolean):Seq[T] =   input.filter(predicat)    
    def discard[T](input:Seq[T], predicat : T=>Boolean):Seq[T] =   input.filterNot(predicat)
}