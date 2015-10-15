package org.kanyec.ast

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.kanyec.SymbolTable

case class StringNode(value: String) extends OperandNode {
  def generate(mv: MethodVisitor, symbolTable: SymbolTable) {
    mv.visitLdcInsn(value)
  }
}
