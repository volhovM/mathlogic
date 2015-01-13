package com.volhovm.mathlogic.ordinals

// http://www.ccs.neu.edu/home/pete/pub/cade-algorithms-ordinal-arithmetic.pdf

sealed trait Ordinal
case class *(lhs: Ordinal, rhs: Ordinal) extends Ordinal
{ override def toString = lhs.toString + "*" + rhs }
case class +(lhs: Ordinal, rhs: Ordinal) extends Ordinal
{ override def toString = lhs.toString + "+" + rhs }
case class ^(lhs: Ordinal, rhs: Ordinal) extends Ordinal
{ override def toString = lhs.toString + "^" + rhs }
case class Num(num: Integer) extends Ordinal
{ override def toString = num.toString }
case class W() extends Ordinal
{ override def toString = "w" }

object CNF {
  def addPair(pair: (CNF, Int), that: CNF) = that match {
      case t@Atom(n) => CList(List((pair._1, pair._2)), t)
      case CList(list, a) => CList((pair._1, pair._2) :: list, a)
    }


  // natural to infinite power
  def exp1(p: Atom, b: CNF): CNF = b match {
      case a if (b.fe == Atom(1)) =>
        CList(List((Atom(b.fc),
                    math.pow(p.nat, b.rest.asInstanceOf[Atom].nat).toInt)),
              zero)
      case a if (b.rest.isInstanceOf[Atom]) =>
        CList(List(
                (CList(List((b.fe - Atom(1), b.fc)), zero),
                 math.pow(p.nat, b.rest.asInstanceOf[Atom].nat).toInt)),
              zero)
      case _ =>
        val c = exp1(p, b.rest)
        CList(List((addPair((b.fe - Atom(1), 1), c.fe),c.fc)), zero)
    }

  // limit to natural power
  def exp2(a: CNF, q: Atom): CNF = q match {
      case Atom(1) => a
      case Atom(n) => CList(List((a.fe * Atom(n - 1), 1)), zero) * a
    }

  // infinite to natural power
  def exp3(a: CNF, q: Atom): CNF = q match {
      case `zero` => Atom(1)
      case Atom(1) => a
      case x if a.limitp => exp2(a, q)
      case Atom(n) => a * exp3(a, Atom(n - 1))
    }

  def exp4(a: CNF, b: CNF): CNF = CList(List((a.fe * b.limitpart, 1)), zero) * exp3(a, b.natpart)
}

sealed trait CNF extends Ordered[CNF]{
  import CNF._
  override def compare(that: CNF): Int = this match
    {
      case Atom(n) => that match {
        case Atom(m) => n compare m
        case _       => -1
      }
      case CList(_, _) if (that match {
                             case Atom(_) => true
                             case _       => false
                           }) => 1
      case CList(_, _) if (this.fe != that.fe) =>
        this.fe compare that.fe
      case CList(_, _) if (this.fc != that.fc) =>
        this.fc compare that.fc
      case _ => this.rest compare that.rest
    }

  def first: CNF = this match {
      case Atom(_)        => this
      case CList(list, a) => CList(list.head :: Nil, 0)
    }

  def rest: CNF = this match {
      case Atom(_)        => ??? //it's ok
      case CList(list, a)
          if (list.length == 1) => a
      case CList(list, a) => CList(list.tail, a)
    }

  def firstn(n: Int): CNF = this.first match {
      case t@Atom(n) => t
      case CList(l, _) => if (n > 1)
        (this.rest.firstn(n - 1) match {
           case t@Atom(_) => CList(l, t)
           case CList(l2, a) => CList(l ::: l2, a)
         })
         else
           if (n == 1) this.rest match {
             case t@Atom(_) => CList(l, t)
             case CList(l2, a) => CList(l ::: l2, zero)
           } else throw new Exception("firstn")
    }

  def restn(n: Int): CNF = n match {
      case 0 => this
      case i => this.rest.restn(i - 1)
    }

  def fe: CNF = this match {
      case Atom(_)        => Atom(0)
      case CList(list, _) => list.head._1
    }

  def fc: Int = this match {
      case Atom(n)        => n
      case CList(list, _) => list.head._2
    }

  def length: Int = this match {
      case Atom(_)        => 0
      case t@CList(_, _) => 1 + t.rest.length
    }

  def size: Int = this match {
      case Atom(_)        => 1
      case t@CList(_, _) => t.fe.size + t.rest.size
    }

  // FIXME
  def @@(that: CNF): CNF = this match {
      case Atom(_) => that
      case t@CList(l, a) => (t.rest @@ that) match {
        case a@Atom(b) => CList(l, a)
        case CList(l2, a2) => CList(l ::: l2, a2)
      }
    }

  // index of the first exp of this ≤ b.fe
  def c(b: CNF): Int = if (b.fe < this.fe) 1 + this.rest.c(b) else 0

  // skips over first n elems of this, then calls c
  def count(b: CNF, n: Int) = n + this.restn(n).c(b)

  // skips n of this, adds rest to b
  def padd(b: CNF, n: Int) = this.firstn(n) @@ (this.restn(n) + b)

  // true if ordinal is limit
  def limitp: Boolean = this match {
      case Atom(n) => n == 0
      case t => t.rest.limitp
    }

  //the greatest limit ordinal < this
  def limitpart: CNF = this match {
      case Atom(n) => zero
      case CList(_, _) => this.first match {
        case CList(l, _) => this.rest.limitpart match {
          case t@Atom(n) => CList(l, t)
          case CList(l2, a) => CList(l ::: l2, a)
        }
        case _ => ??? // not possible
      }
    }

  def natpart: Atom = this match {
      case t@Atom(n) => t
      case _ => this.rest.natpart
    }

  def +(that: CNF): CNF = this match {
      case Atom(n) if (that match {
                         case Atom(m) => true
                         case _ => false
                       }) => Atom(n + that.asInstanceOf[Atom].nat)
      case _ =>
        if (this.fe < that.fe)
          that
        else
          if (this.fe == that.fe)
          addPair((this.fe, this.fc + that.fc), that.rest)
        else
          addPair((this.fe, this.fc), this.rest + that)
    }

  def -(that: CNF): CNF = this match {
      case Atom(n) if that.isInstanceOf[Atom] => that match {
        case Atom(m) => Atom(if (n <= m) 0 else n - m)
        case _ => throw new Exception("scala has broken, piu ((99(");
      }
      case a if this.fe < that.fe => zero
      case a if this.fe > that.fe => this
      case a if this.fc < that.fc => zero
      case a if this.fc > that.fc => addPair((this.fe, this.fc - that.fc), a.rest)
      case _                      => this.rest - that.rest
    }

  def *(that: CNF): CNF = that match {
      case `zero` => zero
      case a if (this == zero) => zero
      case Atom(m) => this match {
        case Atom(n) => Atom(n * m)
        case _ => addPair((this.fe, this.fc * m), this.rest)
      }
      case _ => addPair((this.fe + that.fe, that.fc), this * that.rest)
    }


  def ^(that: CNF) = this match {
      case Atom(1) => Atom(1)
      case a if (that == Atom(0)) => Atom(1)
      case Atom(0) => Atom(0)
      case t@Atom(n) => that match {
        case Atom(m) => Atom(math.pow(n, m).toInt)
        case _ => exp1(t, that)
      }
      case a if (that.isInstanceOf[Atom]) => exp3(this, that.asInstanceOf[Atom])
      case _ => exp4(this, that)
    }
}

// Nil + atom, atom -- do not allow to use
// two of this at one time
case class CList(list: List[(CNF, Int)], atom: Atom) extends CNF {
  if (list.length == 0) throw new Exception("Nil in CList")
  override def toString =
    list.map(a => (if (a._2 > 1) a._2.toString + "*" else ""
               + "w" + (if (a._1 != Atom(1))
                 "^(" + a._1.toString + ")" else "")).mkString("+") + (if (atom != zero)
                                                                  "+" + atom.toString else "")
}

case class Atom(nat: Int) extends CNF {
  if (nat < 0) throw new Exception("Atom should contain int >= 0")
  override def toString = nat.toString
}
