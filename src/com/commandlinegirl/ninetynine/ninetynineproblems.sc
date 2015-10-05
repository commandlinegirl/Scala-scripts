import scala.util.Random

/* 1. Find the last element of a list. */
def last[A](xs: List[A]): A = xs match {
  case Nil => throw new IllegalArgumentException("Empty list")
  case h :: Nil => h
  case h :: t => last(t)
}
assert (last(List(1, 1, 2, 3, 5, 8)) == 8)

/* 2. Find the last but one element of a list. */
def penultimate[A](xs: List[A]): A = xs match {
  case Nil => throw new IllegalArgumentException("Empty list")
  case h :: _ :: Nil => h
  case _ :: t => penultimate(t)
}
assert (penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
assert (penultimate(List(1, 2, 3)) == 2)
assert (penultimate(List(1, 2)) == 1)

/* 3. Find the Kth element of a list. */
def getElement[A](xs: List[A], k: Int): A = (k, xs) match {
  case (_, Nil) => throw new IllegalArgumentException("Empty list")
  case (0, h :: _) =>  h
  case (_, _ :: t) =>  getElement(t, k - 1)
}

assert (getElement(List(1, 1, 2, 3, 5, 8), 4) == 5)
assert (getElement(List(1, 2, 3), 2) == 3)
assert (getElement(List(1, 2), 0) == 1)

/* 4. Find the number of elements of a list. */
def length[A](xs: List[A]): Int =
  xs.foldRight(0)((_, acc) => acc + 1)

/* 5. Reverse a list. */
def reverse[A](xs: List[A]): List[A] = xs match {
  case Nil => xs
  case h :: t => reverse(t ::: List(h))
}

def reverse2[A](xs: List[A]): List[A] =
  xs.foldLeft(List[A]()) ((t, h) => h :: t)

/* 6. Find out whether a list is a palindrome. */
def isPalindrome[A](xs: List[A]): Boolean =
  xs == xs.reverse

/* 7. Flatten a nested list structure. */
def flatten[A](xs: List[A]): List[A] =
  xs flatMap {
    case el: List[A] => flatten(el)
    case x => List(x)
  }
assert (flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))

/* 8. Eliminate consecutive duplicates of list elements. */
def eliminateConsDuplicates[A](xs: List[A]): List[A] = xs match {
  case Nil => Nil
  case h :: t  => h :: eliminateConsDuplicates(t.dropWhile(_ == h))
}

def eliminateConsDuplicates2[A](xs: List[A]): List[A] =
  xs.foldRight(List[A]()) ((h, t) =>
    if (t.isEmpty || t.head != h) h :: t
    else t
  )

/* 9. Pack consecutive duplicates of list elements into sublists. */
def pack[A](xs: List[A]): List[List[A]] = {
  if (xs.isEmpty) {
    List()
  } else {
    val (first, rest) = xs span ( _ == xs.head)
    if (rest == Nil) List(first)
    else first :: pack(rest)
  }
}

/* 10. Run-length encoding of a list. */
def encode[A](xs: List[A]): List[(Int, A)] = {
  val packed = pack(xs)
  packed map (x => (x.length, x.head))
}
assert (encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))

/* 11. Modified run-length encoding. */
def encodeModified[A](xs: List[A]): List[_] =
  encode(xs) map { x => if (x._1== 1) x._2 else x }

assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))

/* 12. Decode a run-length encoded list. */
def decode[A](xs: List[(Int, A)]): List[A] =
  xs flatMap {case (x, y) => List.fill(x)(y)}

assert (decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

/* 13. Run-length encoding of a list (direct solution). */
def encodeDirect[A](xs: List[A]) =
  (xs groupBy (x => x)) map (xy => (xy._1, xy._2.length)) toList

/* 14. Duplicate the elements of a list. */
def duplicate[A](xs: List[A]): List[A] = xs match {
  case Nil => Nil
  case h :: t => h :: (h :: duplicate(t))
}

def duplicate2[A](xs: List[A]): List[A] = {
  xs flatMap (x => List(x, x))
}

/* 15. Duplicate the elements of a list a given number of times. */
def duplicateN[A](n: Int, xs: List[A]): List[A] =
  xs flatMap (x => for (m <- 1 to n) yield x)

/* 16. Drop every Nth element from a list. */
def drop[A](n: Int, xs: List[A]): List[A] = {
  def d(x: Int, ys: List[A]): List[A] = ys match {
    case Nil => Nil
    case h :: t if (x + 1) % n != 0 => h :: d(x - 1, t)
    case _ :: t => d(x - 1, t)
  }
  d(xs.length, xs)
}

def drop3[A](n: Int, xs: List[A]): List[A] =
  xs.zipWithIndex filter {xy => (xy._2 + 1) % 2 != 0} map (_._1)

/* 17. Split a list into two parts. */
def split[A](n: Int, xs: List[A]): (List[A], List[A]) =
  (xs take n, xs drop n)

/* 18. Extract a slice from a list. Given two indices, I and K, the slice
   is the list containing the elements from and including the Ith element up
   to but not including the Kth element of the original list. Start counting
   the elements with 0.*/
def slice[A](a: Int, b: Int, xs: List[A]): List[A] =
  (xs drop a) take (b - a)

/* 19. Rotate a list N places to the left. */
def rotate[A](n: Int, xs: List[A]): List[A] = {
  if (n > 0) {
    (xs drop n) ::: (xs take n)
  } else {
    (xs drop (xs.length - 2)) ::: (xs take (xs.length - 2))
  }
}

assert (rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

/* 20. Remove the Kth element from a list. */
def removeAt[A](n: Int, xs: List[A]): List[A] = {
  val (x, y) = xs.splitAt(n)
  x ::: y.tail
}

/* 21. Insert an element at a given position into a list. */
def insert[A](el: A, n: Int, ns: List[A]): List[A] = {
  if (n > ns.length) throw new IllegalArgumentException("Insert position lager than list length")
  (ns take n) ::: (el :: (ns drop n))
}

/* 22. Create a list containing all integers within a given range. */
def range(a: Int, b: Int): List[Int] = {
  (a to b).toList
}

def range2(a: Int, b: Int): List[Int] = {
  if (a <= b) a :: range2(a + 1, b)
  else List()
}

/* 23. Extract a given number of randomly selected elements from a list. */
def randomSelect[A](n: Int, xs: List[A]): List[A] = {
  if (xs.isEmpty || n == 0) {
    List()
  } else {
    val ind: Int = Random.nextInt(xs.size)
    xs(ind) :: randomSelect(n - 1, removeAt(ind, xs))
  }
}
assert(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length == 3)

/* 24. Lotto: Draw N different random numbers from the set 1..M */
def lotto(n: Int, m: Int): List[Int] = {
  randomSelect(n, (1 to m).toList)
}

