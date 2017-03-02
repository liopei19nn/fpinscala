package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("return tail from Nil")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("set empty head")
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l
    else l match {
      case Nil  => sys.error("list length is shorter than n")
      case Cons(_, xs) => drop(xs, n - 1)
    }

  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on an empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs ,f(z, x))(f)
  }

  def sumFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  // reverse using foldLeft
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((t, h) => Cons(h, t))

  // it should be O(N^2) to reverse with foldRight without reverse the list, since for every element, you have to
  // put it in the end of its tail

  def foldRightUsingLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  // do not using reverse for this, because reverse is implemented with foldLeft, so you actually implemented reverseLeft with
  // reverseLeft
  // tricky and do not know why

//  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
//  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  // ex 14 implement append via foldLeft or foldRight
  def appendViaRightFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // ex 15 concatenates list of lists to single list
  def concatLists[A](lists: List[List[A]]): List[A] = foldRight(lists, List[A]())((a, b) => append(a, b))

  // ex 16 add 1 to each element in List, without map function
  def addOneWithoutMap(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  // ex 17 turn double to string in list
  def doubleToList(l: List[Double]): List[String] = foldRight(l, List[String]())((a, b) => Cons(a.toString, b))

  // ex 18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  // ex 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((a, b) => if(f(a)) Cons(a, b) else b)

  // ex 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concatLists(map(l)(f))

  // ex 21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  // ex 22 implement pairwise add function. List(1,2,3) + List(4,5,6) => List(5,7,9)
  // implement ex 23 zip first
  def zipSum(list1: List[Int], list2: List[Int]): List[Int] = zipWith(list1, list2)(_ + _)


  // ex 23, if one list is longer than the other, the remaining will be ignored
  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] =
    (list1, list2) match {
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
      case _ => Nil
    }

  // ex 24: check if l has sub-sequence sub
  def matchSub[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(x1, xs1), Cons(x2, xs2)) => (x1 == x2) && matchSub(xs1, xs2)
    case (Nil, _) => sub == Nil
  }
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if(matchSub(l, sub)) => true // used to break the recursive loop when first match happened
    case Cons(x, xs) => hasSubsequence(xs, sub)
  }



}

object TestList {
  def main(args: Array[String]) {
    val a1 = Nil
    val a2 = List(1, 2, 2, 4, 5, 6)

    val result = List.hasSubsequence(a2, a1)

    println(result)
  }
}