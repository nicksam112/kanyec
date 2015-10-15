package org.kanyec.ast

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.kanyec.SymbolTable

case class SetValueExpressionNode(operand: OperandNode) extends ExpressionNode{
  def generate(mv: MethodVisitor, symbolTable: SymbolTable) {
    operand.generate(mv, symbolTable)
  }
}
