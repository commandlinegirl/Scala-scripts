package com.commandlinegirl.snippets.lists

/***
One-liners and snippets.
They are solutions to coding exercises found around the net:
- http://hackerrank.com
- own, self-inflicted ideas ;)
***/

/* Repeat the nth value of the list num times */
def f(num:Int, arr:List[Int]): List[Int] = 
    arr flatMap (List.fill(num)(_))

/* For a given list with  integers, return a new list removing the elements at odd positions. */
def f(arr:List[Int]): List[Int] = 
    ((0 until arr.size) flatMap (x => if (x % 2 != 0) Some(arr(x)) else None)).toList

def f(arr:List[Int]): List[Int] = 
    arr.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)    

/* Reverse a list (inbuilt function: arr.reverse) */
def f(arr:List[Int]): List[Int] = 
    arr foldLeft (List[Int]())((r, c) => c :: r)

/* Sum of odd elements from the given list */
def f(arr:List[Int]): Int = 
    arr.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).foldLeft(0)((acc, x) => acc + x)

/* Nth factorial (using foldLeft) */
def fib(n: Int): Int = 
    (1 to n).foldLeft(1)((a, b) => a * b)

/* List length */
def f(arr:List[Int]): Int = 
    arr.foldLeft(0)((acc, _) => acc + 1)

/* Update the values of a list with their absolute values. */
def f(arr:List[Int]): List[Int] = 
    arr.map(math.abs(_))

/* Filter a given array of integers and output only those values that are less than a specified value  */
def f(delim:Int, arr:List[Int]): List[Int] = 
    arr.filter(_ < delim)

