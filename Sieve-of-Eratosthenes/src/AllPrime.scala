/**
 * Subject: Advanced Programming - Lab 5: Sieve of Eratosthenes
 *
 * Coded by: Ubaid ur Rehman
 * Email: uurehman.bscs15seecs@seecs.edu.pk
 * Date: October 19, 2017
 */

import scala.annotation.tailrec // for @tailrec
import scala.collection.parallel.mutable // to make parSet mutable
import util.Random.nextInt
import io.StdIn._
import scala.compat.Platform

object AllPrime {
  def main(args: Array[String]): Unit = {
    print("Enter the number to which you want to find prime numbers: ")
    val numProvided: Int = readInt()
    
    val executionStart = Platform.currentTime
    val setOfPrimeNumbers = sieveOfEratosthenes(numProvided)
    val executionEnd = Platform.currentTime
    
    println(setOfPrimeNumbers)

    println("SUCCESS: Completed without errors.\nInput: " + numProvided + "\t\tTime taken: " + (executionEnd - executionStart) + " ms")
  } // end main
  
  // source: https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Scala
  def sieveOfEratosthenes(limit: Int) = {
    val setOfPrimeNumbers: mutable.ParSet[Int] = mutable.ParSet.empty ++ (2 to limit) // mutable parallel set of prime numbers
    val sqrtLimit = math.sqrt(limit).toInt // square root of limit in integer
    //    tail recursive function
    @tailrec
    def isPrime(numberGiven: Int): Unit = {
      if (numberGiven <= sqrtLimit) {
        /* if numberGiven is in setOfPrimeNumbers, remove the series:
        numberGiven^2, numberGiven^2 + numberGiven, numberGiven^2 + ^2numberGiven, numberGiven^2
        + 3numberGiven, ..., not exceeding limit.*/
        if (setOfPrimeNumbers contains numberGiven) setOfPrimeNumbers --= numberGiven * numberGiven to limit by numberGiven
        isPrime(numberGiven + 1) //recursive call for next integer
      }
    }
    isPrime(2)
    setOfPrimeNumbers 
    val sortedSetOfPrimeNumbers = collection.immutable.SortedSet[Int]() ++ setOfPrimeNumbers
    sortedSetOfPrimeNumbers
  }
  
}