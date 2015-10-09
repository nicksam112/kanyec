package org.kanyec

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import org.kanyec.ast._

class KanyeParser extends Parser {


  val ParseError = "BALLING TOO HARD"

  val DeclareInt = "I am god's vessel"
  val SetInitialValue = "God has chosen me to be"
  val BeginMain = "I am the greatest"
  val PlusOperator = "gettin paid"
  val MinusOperator = "gettin laid"
  val MultiplicationOperator = "what you said"
  val DivisionOperator = "I liberate minds"
  val EndMain = "I still think I am the greatest"
  val Print = "make her say"
  val Read = "I am not a fan of books"
  val AssignVariable = "I don't even listen to rap"
  val SetValue = "I am"
  val EndAssignVariable = "shakespeare in the flesh"
  val False = "ain't sayin she a gold digger"
  val True = "ain't messing with no broke"
  val EqualTo = "isn't equal to me, neither is"
  val GreaterThan = "I am greater than"
  val Or = "I don't know what's better"
  val And = "and ball so hard that"
  val If = "I ask cuz I'm not sure"
  val Else = "don't let me get in my zone"
  val EndIf = "imma let you finish but"
  val While = "our work is never over"
  val EndWhile = "back at the crib"
  val DeclareMethod = "I will be the loudest voice"
  val MethodArguments = "no one can tell me where I can and can't go"
  val Return = "coldest story ever told"
  val EndMethodDeclaration = "I'll just say goodnight"
  val CallMethod = "one thing i gotta call out boy"
  val NonVoidMethod = "ninjas are kind of cool"
  val AssignVariableFromMethodCall = "need you right now"
  val Modulo = "ball so hard"

  val EOL = zeroOrMore("\t" | "\r" | " ") ~ "\n" ~ zeroOrMore("\t" | "\r" | " " | "\n")
  val WhiteSpace = oneOrMore(" " | "\t")

  def Root: Rule1[RootNode] = rule {
    oneOrMore(AbstractMethod) ~ EOI ~~> RootNode
  }

  def AbstractMethod: Rule1[AbstractMethodNode] = rule {
    (MainMethod | Method) ~ optional(EOL)
  }

  def MainMethod: Rule1[AbstractMethodNode] = rule {
    BeginMain ~ EOL ~ zeroOrMore(Statement) ~ EndMain ~~> MainMethodNode
  }

  def Method: Rule1[AbstractMethodNode] = rule {
    DeclareMethod ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~
      zeroOrMore((MethodArguments ~ WhiteSpace ~ Variable ~ EOL)) ~
      (NonVoidMethod | "") ~> ((m: String) => m == NonVoidMethod) ~ optional(EOL) ~
      zeroOrMore(Statement) ~ EndMethodDeclaration ~~> MethodNode
  }

  def Statement: Rule1[StatementNode] = rule {
    DeclareIntStatement | PrintStatement |
      AssignVariableStatement | ConditionStatement |
      WhileStatement | CallMethodStatement | ReturnStatement | CallReadMethodStatement
  }

  def CallMethodStatement: Rule1[StatementNode] = rule {
    (AssignVariableFromMethodCall ~ WhiteSpace ~ VariableName ~> (v => v) ~ EOL | "" ~> (v => v)) ~
      CallMethod ~ WhiteSpace ~ VariableName ~> (v => v) ~
      zeroOrMore(WhiteSpace ~ Operand) ~ EOL ~~> CallMethodNode
  }

  def CallReadMethodStatement: Rule1[StatementNode] = rule {
    (AssignVariableFromMethodCall ~ WhiteSpace ~ VariableName ~> (v => v) ~ EOL | "" ~> (v => v)) ~
      CallMethod ~ EOL ~ Read ~ EOL ~~> CallReadMethodNode
  }

  def ConditionStatement: Rule1[ConditionNode] = rule {
    If ~ WhiteSpace ~ Operand ~ EOL ~ zeroOrMore(Statement) ~
      (Else ~ EOL ~ zeroOrMore(Statement) ~~> ConditionNode
        | zeroOrMore(Statement) ~~> ConditionNode) ~ EndIf ~ EOL

  }

  def WhileStatement: Rule1[WhileNode] = rule {
    While ~ WhiteSpace ~ Operand ~ EOL ~ zeroOrMore(Statement) ~ EndWhile ~ EOL ~~> WhileNode
  }

  def PrintStatement: Rule1[PrintNode] = rule {
    Print ~ WhiteSpace ~ (Operand ~~> PrintNode | "\"" ~ String ~ "\"" ~~> PrintNode) ~ EOL
  }

  def DeclareIntStatement: Rule1[DeclareIntNode] = rule {
    DeclareInt ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~ SetInitialValue ~ WhiteSpace ~ Operand ~~> DeclareIntNode ~ EOL
  }

  def AssignVariableStatement: Rule1[AssignVariableNode] = rule {
    AssignVariable ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~ Expression ~ EndAssignVariable ~ EOL ~~> AssignVariableNode
  }

  def ReturnStatement: Rule1[StatementNode] = rule {
    Return ~ ((WhiteSpace ~ Operand ~~> (o => ReturnNode(Some(o)))) | "" ~> (s => ReturnNode(None))) ~ EOL
  }

  def Operand: Rule1[OperandNode] = rule {
    Number | Variable | Boolean
  }

  def Expression: Rule1[AstNode] = rule {
    SetValueExpression ~
      (zeroOrMore(ArithmeticOperation | LogicalOperation))
  }

  def LogicalOperation: ReductionRule1[AstNode, AstNode] = rule {
    Or ~ WhiteSpace ~ Operand ~ EOL ~~> OrNode |
      And ~ WhiteSpace ~ Operand ~ EOL ~~> AndNode |
      EqualTo ~ WhiteSpace ~ Operand ~ EOL ~~> EqualToNode |
      GreaterThan ~ WhiteSpace ~ Operand ~ EOL ~~> GreaterThanNode

  }

  def RelationalExpression: ReductionRule1[AstNode, AstNode] = {
    EqualToExpression ~~> EqualToNode |
      GreaterThanExpression ~~> GreaterThanNode
  }


  def EqualToExpression: Rule1[OperandNode] = {
    EqualTo ~ WhiteSpace ~ Operand ~ EOL
  }

  def GreaterThanExpression: Rule1[OperandNode] = {
    GreaterThan ~ WhiteSpace ~ Operand ~ EOL
  }

  def ArithmeticOperation: ReductionRule1[AstNode, AstNode] = rule {
    PlusExpression ~~> PlusExpressionNode |
      MinusExpression ~~> MinusExpressionNode |
      MultiplicationExpression ~~> MultiplicationExpressionNode |
      DivisionExpression ~~> DivisionExpressionNode |
      ModuloExpression ~~> ModuloExpressionNode
  }

  def SetValueExpression: Rule1[OperandNode] = rule {
    SetValue ~ WhiteSpace ~ Operand ~ EOL
  }


  def PlusExpression: Rule1[AstNode] = rule {
    PlusOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def MinusExpression: Rule1[AstNode] = rule {
    MinusOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def MultiplicationExpression: Rule1[AstNode] = rule {
    MultiplicationOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def DivisionExpression: Rule1[AstNode] = rule {
    DivisionOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def ModuloExpression: Rule1[AstNode] = rule {
    Modulo ~ WhiteSpace ~ Operand ~ EOL
  }

  def Variable: Rule1[VariableNode] = rule {
    VariableName ~> VariableNode
  }

  def VariableName: Rule0 = rule {
    rule("A" - "Z" | "a" - "z") ~ zeroOrMore("A" - "Z" | "a" - "z" | "0" - "9")
  }

  def Number: Rule1[NumberNode] = rule {
    oneOrMore("0" - "9") ~> ((matched: String) => NumberNode(matched.toInt)) |
      "-" ~ oneOrMore("0" - "9") ~> ((matched: String) => NumberNode(-matched.toInt))
  }

  def Boolean: Rule1[NumberNode] = rule {
    "@" ~ True ~> (_ => NumberNode(1)) |
      "@" ~ False ~> (_ => NumberNode(0))
  }

  def String: Rule1[StringNode] = rule {
    zeroOrMore(rule {
      !anyOf("\"\\") ~ ANY
    }) ~> StringNode
  }

  def parse(expression: String): RootNode = {
    val parsingResult = ReportingParseRunner(Root).run(expression)
    parsingResult.result match {
      case Some(root) => root
      case None => throw new ParsingException(ParseError + ":\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }
  }

}