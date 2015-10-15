package org.kanyec.ast

import org.objectweb.asm.{MethodVisitor, ClassWriter}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.commons.{GeneratorAdapter, Method}
import org.kanyec.SymbolTable

abstract class AstNode {
  def generate(mv: MethodVisitor, symbolTable: SymbolTable)
}
