package example

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._

class CalculatorSpec extends FunSpec with Matchers  {
 

  val checkBracketTestCases =  Table(
      ("str", "result"),  
      ("",   true),  
      ("a",   true),      
      ("(a)",   true),  
      ("(a a v )",   true),  
      (" () ",   true),  
      (")(",   false),  
      (")a",   false),        
      ("a(",   false),  
      ("((",   false),  
      (")((",   false),  
      (")(()",   false),  
      (") (((",   false),  
    )
  it ("checkBracket") {
    forAll (checkBracketTestCases) { (str:String, res:Boolean) =>
      Calculator.checkBracket(str) should be (res)
    }
  }



  val isOperatorTestCases =  Table(
      ("str", "whithDash", "result"),  
      ("+",  true, true ),
      ("/",  true, true ),
      ("*",  true, true ),
      ("-",  true,true ),
      ("-",  false,false ),
      ("-123",  true,false ),
      ("-.",  true,false ),
      (" -",  true,true ), 
      (".-",  true,false ),
      ("",  true,false ),
    )
  it ("isOperator") {
    forAll (isOperatorTestCases) { (str:String,  whithDash:Boolean, res:Boolean) =>
      Calculator.isOperator(str, whithDash) should be (res)
    }
  }

  val isNumberTestCases =  Table(
    ("str", "result"),
    ("", false ),
    ("1",   true),
    ("1.",   true),
    (".1",   true),
    ("-.1",   true),
    ("-.1345",   true),
    ("-0.1345",   true),
    ("-2.1",   true),
    ("-123.234",   true),
    (".1.",   false),
    ("--1",   false),
    (".",   false),
    (".-",   false),
    ("1.-",   false),
    ("1-2",   false),
    ("1.-2.",   false),
    (".1-.2",   false),
  )

  it ("isNumber") {
    forAll (isNumberTestCases) { (str:String, res:Boolean) =>
      Calculator.isNumber(str) should be (res)
    }
  }


  val checkAvailableCharsTestCases =  Table(
      ("str", "result"), 
      ("",   true), 
      ("++",   true),  
      ("2 + 3",   false),
      ("(++)",   true),
      ("2+a",   false),
      ("123+123-234234",   true),
      ("123+123-234234",   true), 
      ("1+++**+123---",   true), 
      ("1*((23+123))",   true), 
    )
  it ("checkAvailableChars") {
    forAll (checkAvailableCharsTestCases) { (str:String, res:Boolean) =>
      Calculator.checkAvailableChars(str) should be (res)
    }
  }


  val getOpPriorityTestCases =  Table(
      ("str", "result"), 
      ("+", 1),
      ("-", 1),
      ("/", 2),
      ("*", 2),
      ("(", 0),
      (")", 0),
    )
  it ("getOpPriority") {
    forAll (getOpPriorityTestCases) { (str:String, res:Int) =>
      val prio :Int = Calculator.getOpPriority(str)
      prio should be (res)
    }
  }


val splitEvalTestCases =  Table(
      ("str", "result"), 
      ("", Some(List(""))),
      ("123", Some(List("123"))),
      ("1.", Some(List("1."))),
      (".1", Some(List(".1"))),
      ("2.1", Some(List("2.1"))),
      ("12.3.", None),
      ("13++", None),
      ("14+-", None),
      ("+-15", None),
      ("16+-1", Some(List("16","+","-1"))),
      ("-17", Some(List("-17"))),
      ("-.17", Some(List("-.17"))),
      ("-17.", Some(List("-17."))),
      ("1+-18", Some(List("1", "+", "-18"))),
      ("1*/19", None),
      ("1*/10.", None),
      ("..*22.", None),
      ("12.3+.234*456.12123", Some(List("12.3","+", ".234","*", "456.12123"))),
      ("(-17)", Some(List("(", "-17", ")"))),
      ("1+(-18)", Some(List("1", "+", "(", "-18", ")"))),
      ("*.", None),    
      ("./2", None),
      (".1*.2", Some(List(".1", "*", ".2"))),
      ("(*.)", None),    
      ("(./2)", None),
      ("(.1*.2)", Some(List("(", ".1", "*", ".2", ")"))),
      ("12.3+(.234*456.12123)", Some(List("12.3","+", "(", ".234","*", "456.12123" , ")"))),
      ("1+2*3", Some(List("1", "+","2", "*", "3"))),
      ("(1+2)*3", Some(List("(","1", "+","2",")", "*", "3"))),
      ("(1+2)-3", Some(List("(","1", "+","2",")", "-", "3"))),
      ("(1.+2.)*3.", Some(List("(","1.", "+","2.",")", "*", "3."))),
      ("(1.+2.)-3.", Some(List("(","1.", "+","2.",")", "-", "3."))),
      ("(.1+.2)*.3", Some(List("(",".1", "+",".2",")", "*", ".3"))),
      ("(.1+.2)-.3", Some(List("(",".1", "+",".2",")", "-", ".3"))),
      ("(1-2)-3", Some(List("(","1", "-","2",")", "-", "3"))),
      ("(1-1)*2+3*(1-3+4)+10/2", Some(List("(", "1", "-", "1", ")", "*", "2", "+", "3", "*", "(", "1","-","3","+","4",")","+","10","/","2")))
    )
  it ("splitEval") {
    forAll (splitEvalTestCases) { (str:String, res:Option[List[String]]) =>
      Calculator.splitEval(str) should be (res)
    }
  }



  val calculateByListTestCases =  Table(
      ("strs", "result"), 
      (List("1"),   1.0),
      (List("(", "1", "+","2", ")"),   3.0),
      (List("1", "+","2"),   3.0),
      (List("11.0", "*","2"),   22.0),
      (List("(", "1", "+","2", ")", "*", "3"),   9.0),
      (List("(", "1", "+","2", ")", "/", "3"),   1.0),
      (List("(", "3", "*","2", ")", "/", "3"),   2.0),
      (List("1", "+","(","2", "*", "3", ")"),   7.0),
      (List("1", "+","(","12", "/", "3", ")"),   5.0),
      (List("10", "-","(","12", "/", "3", ")"),   6.0),
      (List("10", "+","1", "+", "3", "+", "5"),   19.0),
      (List("10", "-","1", "-", "3", "-", "5"),   1.0),
      (List("10", "+","-1", "+", "-3", "+", "-5"),   1.0),
      (List("1", "*","-2", "*", "-3", "*", "-4"),   -24.0),
      (List("36", "/","3", "/", "2", "/", "3"),   2.0),
      (List("(", "(", "36", "/","3", ")", "/", "2",")", "/", "3"),   2.0),
      (List("36", "/","3", "+", "6", "/", "3"),   14.0),
      (List("36", "+","6", "/", "6", "+", "3"),   40.0),
      (List("(", "1", "-", "1", ")", "*", "2", "+", "3", "*", "(", "1","-","3","+","4",")","+","10","/","2"),   11.0),
  )
  it ("calculateByList") {
    forAll (calculateByListTestCases) { (strs:List[String], result:Double) =>
      Calculator.calculateByList(strs)  should be (result)
    }
  }


  
  val calculateTestCases =  Table(
      ("str", "result"),  
      (  "",   Some(0.0)),  
      ( " ",   Some(0.0)),
      ( "1.0",   Some(1.0)),
      ( "1.1 ",   Some(1.1)),
      ( "  1123.234  ",   Some(1123.234)),
      ("(1-1)*2+3*(1-3+4)+10/2", Some(11.0)),
      ("(1.-1.)*2.+3.*(1.-3.+4.)+10./2.", Some(11.0)),
      ("(1-1)*2+3*(1-3+4)+10/4", Some(8.5)),
      (" ( 1 - 1 ) * 2 + 3 * ( 1 - 3 + 4 ) + 10 / 4  ", Some(8.5)),
    )

  it ("calculate") {
    forAll (calculateTestCases) { (str:String, res:Option[Double]) =>
      Calculator.calculate(str) should be (res)
    }
  }



  it ("the one") {
     Calculator.calculate("(1-1)*2+3*(1-3+4)+10/2")  should be (Some(11.0))
    }

}
