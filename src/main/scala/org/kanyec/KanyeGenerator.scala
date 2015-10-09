package org.kanyec

import org.kanyec.ast.RootNode


class KanyeGenerator extends ClassLoader {

  def generate(kanyeCode: String, filename: String): (Array[Byte], RootNode) = {
    val parser = new KanyeParser
    val rootNode = parser.parse(kanyeCode)
    (rootNode.generateByteCode(filename), rootNode)
  }
}
