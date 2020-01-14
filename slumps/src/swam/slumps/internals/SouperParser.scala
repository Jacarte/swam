package swam.slumps.internals



import fastparse._



sealed trait Token

sealed trait LeftToken extends Token

sealed trait RightToken extends Token

sealed trait Val extends Any {
  def value: Any
  def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
  def apply(s: java.lang.String): Val =
    this.asInstanceOf[Obj].value.find(_._1 == s).get._2
}


// Basic values
case class Arr(value: Val*) extends AnyVal with Val
case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
case class Str(value: java.lang.String) extends AnyVal with Val
case class Integer(value: Int) extends AnyVal with Val
case class T(value: Int) extends AnyVal with Val

class Id(val id: String, _type: T) extends LeftToken

class Reference(val id: String) extends RightToken

class Const(val value: Int, val t: T) extends RightToken

class Opcode(val value: String) extends RightToken

class SouperSentence(val tokens: Seq[RightToken], val name: Option[Id]){

  //val dependencies: Seq[Reference] = tokens.map{
  //  case a:Reference => a
  //}
}


class SouperDAG(val instructions: Map[Id, SouperSentence], val entry: RightToken) {

  // DFS starting from entry, return subgraph
  def merge(cfg: SouperDAG, entry: Id): SouperDAG = null

}

class SouperParser {

  var counter = 0

  private def getCount() = {
    this.counter += 1
    this.counter
  }
  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    CharsWhileIn(" \t", 0)
  }

  def stringChars(c: Char) = c != ':' && c != ';' && c != '=' && c != '%' && c != ','
  def space[_: P]         = P( CharsWhileIn(" ", 0) )
  def digits[_: P]        = P( CharsWhileIn("0-9") )
  def strChars[_: P] = P( CharsWhile(stringChars, 1) )


  def dag[_:P]: P[SouperDAG] =
    P(
      sentence.rep(1)  ~ result ~ End
    ).map(t => new SouperDAG(
      Map.from(t._1.map(sentence => ( sentence._1 match {
        case Some(i) => i
        case None => new Id(s"non%${this.getCount()}", T(0))
      } , sentence._2))),
      t._2
    )).log

  def comment[_:P]: P[String] = P(";" ~ AnyChar.rep.! ~ "\n") // TODO implement annotations on comments here

  def id[_:P]:P[Id] = P( "%" ~/ digits.! ~/ ":" ~/ size).map(t => new Id(t._1, t._2))

  def reference[_:P]:P[Reference] = P("%" ~ strChars.!).map(t => new Reference(t))

  def const[_:P]:P[Const] = P( digits.! ~ size ).map(t => new Const(t._1.toInt, t._2))


  def result[_:P]:P[RightToken] = P(
    ("infer" | "result") ~/ right
  )

  def right[_:P]:P[RightToken] =
    P(
      const |
      reference |
      opcode
    )

  def sentence[_:P]:P[(Option[Id], SouperSentence)] =
    P( (id  ~/ "=").?  ~/ right.rep(1) ~/ (comment.? | "\n")).map(t => (t._1, new SouperSentence(t._2, t._1)))

  def size[_:P]:P[T] = P("i" ~ digits.!).map(t => T(t.toInt))

  // TODO define opcodes by concrete names: add, mul, select, phi, etc
  def opcode[_:P]:P[Opcode] = P(strChars.!map(t => new Opcode(t)))


}


object SouperParser