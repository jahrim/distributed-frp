package it.unibo.distributedfrp.core

trait Export[+A]:
  private val INDENT_AMOUNT = "  "

  def root: A

  def children: Map[Any, Export[Any]]

  def followPath(path: Seq[Any]): Option[Export[Any]] = path match
    case h :: t => children.get(h).flatMap(_.followPath(t))
    case _ => Some(this)

  private def format(indent: String, sb: StringBuilder): Unit =
    sb.append("[").append(root).append("]\n")
    if (children.nonEmpty) {
      sb.append(indent).append("{\n")
      children.foreach { (k, v) =>
        sb.append(indent).append(INDENT_AMOUNT).append(k).append(" -> ")
        v.format(indent + INDENT_AMOUNT, sb)
      }
      sb.append(indent).append("}\n")
    }

  override def toString: String =
    val sb = new StringBuilder()
    format("", sb)
    sb.toString

object Export:
  case class ExportImpl[+A](root: A, children: Map[Any, Export[Any]]) extends Export[A]


  def apply[A](root: A, children: (Any, Export[Any])*): Export[A] = ExportImpl(root, children.toMap)
