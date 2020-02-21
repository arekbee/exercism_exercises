package example

import example.Calculator.parentheses

object Calculator  extends App{

    def checkBracket(str:String) = {
      def checkBracketRec(list:Seq[String], counter:Int) :Boolean = {
        list match {
          case "("::xs => checkBracketRec(xs, counter+1)
          case ")"::xs if counter >0 => checkBracketRec(xs, counter-1)
          case ")"::xs => false
          case x::xs => checkBracketRec(xs, counter)
          case Nil | ""::Nil if counter == 0 => true
          case _ => false 
        }
      }
      checkBracketRec(str.split("").toList, 0)
    }
    

    def operators  = operatorsWithoutDash +  '-'
    def operatorsWithoutDash = Set('+', '/', '*')
    def decSep = '.'
    def parentheses = Set('(', ')')

    def getOpPriority : PartialFunction[String, Int] = {
      case "*" | "/" => 2
      case "+" | "-" => 1
      case _ => 0
    }

    def isOperator(str:String, whithDash:Boolean) :Boolean = {
      (str.trim(), whithDash) match {
        case (x , true) if x.size==1 &&  operators.contains(x.head) => true
        case (x , false) if x.size==1 &&  operatorsWithoutDash.contains(x.head) => true
        case _ => false
      }
    }

    def checkAvailableChars(str:String) = {
      def checkAvailableCharsRec(list:List[Char]) :Boolean = {
        list match {
          case Nil => true
          case x::Nil if x.isDigit || operators.contains(x) || x == decSep || parentheses.contains(x)  => true
          case x::xs if x.isDigit || operators.contains(x) || x == decSep || parentheses.contains(x)  =>  checkAvailableCharsRec(xs) //
          case _ => false
        }
      }
      checkAvailableCharsRec(str.toCharArray.toList)
    }

    

    def isNumber(str :String)=  {
      def isNumberWithoutDashRec(strList:List[String], hadDecSep:Boolean , hadDigit:Boolean, reqDigit:Boolean) :Boolean = {
        strList match  {
          case x::xs if  x.head == '-' => false
          case x::xs if x.head.isDigit => isNumberWithoutDashRec(xs,hadDecSep, true,false )
          case x::xs if hadDigit && !hadDecSep && x.head == decSep => isNumberWithoutDashRec(xs,true, hadDigit,false )
          case x::xs if !hadDecSep && x.head == decSep => isNumberWithoutDashRec(xs,true, hadDigit, reqDigit )
          case Nil => !reqDigit
          case _ => false
        }
      }

      str.trim() match {
        case ""|"." => false
        case str if str.head == '-' && str.size>1 =>  isNumberWithoutDashRec(str.substring(1).split("").toList,  false, false, true)
        case str =>  isNumberWithoutDashRec(str.split("").toList,  false, false, true)
      }
    }

    def splitEval (str :String) = {
      def splitEvalRec(list:List[String], ret:List[String])  : Option[List[String]] = {
        (ret, list) match {
          case (_ , Nil) => Some(ret.reverse) //stop
          case (_, ""::Nil) => Some(List("")) //init
          case (Nil, x::Nil) if isOperator(x, true) || parentheses.contains(x.head) => None
          case (Nil, x::_)  if isOperator(x, false)=> None // starts with + * /
          case (Nil, x::x1::xs) if x.head == '-' && isNumber(x1) => splitEvalRec(xs, (x+x1)::Nil) //initial
          case (r::rs, x::x1::xs) if isNumber(r+x+x1) => splitEvalRec(xs, (r+x+x1)::rs) //initial
          case (r::r1::rs, x::x1::xs) if  parentheses.contains(r1.head) && isNumber(r) && isOperator(x, true) &&  isNumber(x1) => splitEvalRec(xs, x1::x::ret)
          case (r::rs, x::x1::xs) if r.head == '-' && isNumber(x+x1) => splitEvalRec(xs, (x+x1)::ret) //initial
          case (r::rs, x::x1::xs) if isNumber(r) && x.head=='-' && isNumber(x1) => splitEvalRec(xs, x1::x::ret) //initial
          case (r::rs, x::xs) if parentheses.contains(x.head)  => splitEvalRec(xs, x::ret)
          case (Nil, x::xs) => splitEvalRec(xs, x::Nil) //initial
          case (r::rs, x::xs) if isNumber(r+x)  => splitEvalRec(xs, (r+x)::rs)  //dd
          case (r::rs, x::xs) if x.head == decSep && r.contains(decSep) => None //..
          case (r::rs, x::xs) if x.head == decSep && isNumber(r) => splitEvalRec(xs, (r+x)::rs) //d .
          case (r::rs, x::xs) if x.head.isDigit && r.head == decSep  => splitEvalRec(xs, (r+x)::rs) //.d
          case (r::rs, x::xs) if x.head.isDigit && isOperator(x, true)  => splitEvalRec(xs, x :: ret) //d +
          case (r::rs, x::x1::x2::xs) if isNumber(r) && isOperator( x, true)  && x1.head == '-'  &&  isNumber(x2) =>  splitEvalRec(xs, (x1+x2)::x::ret )  //
          case (r::rs, x::xs) if isNumber(r) && isOperator(x, true) => splitEvalRec(xs, x::ret)
          case (r::rs, x::xs) if isOperator(r, true) && x.head == decSep => splitEvalRec(xs, x::ret)
          case (r::rs, x::x1::xs) if r.head =='(' && isNumber(x+x1) => splitEvalRec(x1::xs, x::ret)
          case (r::rs, x::xs) if isNumber(r) && x.head == ')' => splitEvalRec(xs, x::ret)
          case (r::rs, x::xs) if r.head == ')' && isOperator(x, false)  => splitEvalRec(xs, x::ret)
          case (r::rs, x::x1::xs) if r.head == ')' && x.head=='-' && isNumber(x1)=> splitEvalRec(xs, x1::x::ret)
          case (r::rs, x::Nil) if isOperator(r, true) && isOperator(x, true)  => None // ends on 2 opeartors
          case (r::rs, x::xs) if isOperator(r, false) && isOperator(x, false) => None // 2 operator near each other
          case (r::rs, x::x1::xs) if   parentheses.contains(r.head) && isNumber(x+x1) => splitEvalRec(xs, (x+x1)::ret)
          case (r::rs, x::xs) if   parentheses.contains(r.head) && isNumber(x) => splitEvalRec(xs, x::ret)
          case (r::rs, x::x1::xs) if   parentheses.contains(r.head) && isOperator(x, true) && isNumber(x1) => splitEvalRec(xs, x1::x::ret)
          case (r::rs, x::x1::x2::xs) if   parentheses.contains(r.head) && isOperator(x, true) && isNumber(x1+x2) => splitEvalRec(xs, (x1+x2)::x::ret)
          case (r::rs, x::xs) if   isOperator(r, false)&&  isNumber(x) => splitEvalRec(xs, x::ret)

          case (_,_) => None
        }
      }
      splitEvalRec(str.split("").toList, Nil)
    }


    def calculateByList(operandsAndOperators :List[String] ):Double = {
      def calculateByListRec(toProcess :List[String], ops:List[String], values:List[String] ) : List[String] = {
          toProcess match {
            case "("::xs =>  calculateByListRec(xs, "("::ops, values)
            case x::xs if isNumber(x) => calculateByListRec(xs, ops, x::values)
            case x::xs if isOperator(x, true) =>
              (values, ops) match {
                case (v1 :: v2 :: vs, op1::opss) if getOpPriority(x) <= getOpPriority(op1)  => {
                  val res = biOperate(op1, v2.toDouble, v1.toDouble).toString
                  calculateByListRec(x::xs, opss, res :: vs)
                }
                case _ =>calculateByListRec(xs, x :: ops, values)
              }
            case ")"::xs  =>
              (values, ops) match {
                case (v1::v2::vs, o::op1::os) =>  {
                  val res = biOperate(o, v2.toDouble, v1.toDouble).toString
                  calculateByListRec(xs, os, res::vs)
                }
              }
            case x::xs if ops.size > 0 && getOpPriority(x) <= getOpPriority(ops.last)  =>
              (values, ops) match {
                case (v1::v2::vs, ops) => {
                  val res = biOperate(x, v2.toDouble, v1.toDouble).toString
                  calculateByListRec(xs, ops, res :: vs)
                }
                case (v1::v2::vs, o::os) =>  {
                  val res = biOperate(o, v2.toDouble, v1.toDouble).toString
                  calculateByListRec(xs, os, res::vs)
                }
              }
            case x::xs   => calculateByListRec(xs, x::ops, values)
            case Nil => (values, ops) match {
                case (v1::v2::vs, o::os) =>  {
                  val res = biOperate(o, v2.toDouble,v1.toDouble).toString
                  calculateByListRec(Nil, os, res::vs)
                }
                case _ => values
              }
          }
      }
      
      operandsAndOperators match {
        case Nil => 0.0
        case  x::Nil => x.toDouble
        case ops => calculateByListRec(ops, List(), List())(0).toDouble
      }
    }

    def calculate(str :String) =  { 
      str.replace(" ", "") match {
      case "" =>  Some(0.0)
      case str if str.matches(raw"\d+(\.\d*)?") => Some(str.toDouble)
      case str if checkAvailableChars(str) && checkBracket(str) => splitEval(str) match {
        case Some(ops) => Some(calculateByList(ops))
        case None => None
      }
      case _ => None
      }
    }

    def biOperate (op:String, x:Double,y:Double) = op.trim() match {
      case "+" => x + y
      case "-" =>  x - y
      case "*" =>  x * y
      case "/" =>  x / y
    }
}
