package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	// ex25
	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(r) + size(l)
	}

	// ex26
	def maximum(tree: Tree[Int]): Int = tree match {
		case Leaf(n) => n
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	// ex27
	def depth[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 0
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	// ex28
	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
		case Leaf(a) => Leaf(f(a))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	// ex29
//	def fold[A, B](tree: Tree[A])(f: A => B): Tree[B]




}