package fintech.homework03

import scala.annotation.tailrec

//import scala.collection.mutable.{Map}


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]
  def get: V
}

class MPrefixTree [K, V] private (value: Option[V], transitions: Map[K, PrefixTree[K, V]])
  extends PrefixTree[K, V]{

  def this(){
    this(None: Option[V], Map[K, MPrefixTree[K, V]]())
  }


  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = path match {
    case Seq() => new MPrefixTree[K, U](Option(value), this.transitions)
    case Seq(head, tail @ _*) => transitions.get(head) match {
      case Some(_) => new MPrefixTree[K, U](
        this.value,
        transitions + (path.head -> transitions(path.head).put(path.tail, value)))
      case None =>  new MPrefixTree[K, U](
        this.value,
        transitions + (path.head -> new MPrefixTree[K, U]().put(path.tail, value)))
    }
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = path match {
    case Seq() => this
    case Seq(head, tail @ _*) => transitions.get(head) match {
      case Some(_) => transitions(path.head).sub(path.tail)
      case None => new MPrefixTree[K, V]()
    }
  }

  override def get: V = value.getOrElse(throw new NoSuchElementException)
}