package swam.slumps.internals


import fastparse._


class Id(val id: String)

class SouperSentence(val id: Id, val tokens: Seq[Either[String, Id]])

class SouperCFG(val instructions: Map[Id, SouperSentence], result: Id) {

  // DFS starting from entry, return subgraph
  def merge(cfg: SouperCFG, entry: Id): SouperCFG = null

}

class SouperParser {

  def CFG[_:P]: P[SouperCFG] = null

}


object SouperParser