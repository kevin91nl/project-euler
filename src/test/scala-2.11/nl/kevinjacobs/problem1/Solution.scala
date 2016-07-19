package nl.kevinjacobs.problem1

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by kevin on 19-7-16.
  */
class Solution extends FlatSpec with Matchers {

    // Check the different solvers
    "solvers" should "solve subproblem" in {
        checkSolutions(new Solver().solveSmart)
        checkSolutions(new Solver().solveNaivelyFunctional)
        checkSolutions(new Solver().solveNaivelyImperative)
    }

    // Check a number of problems
    def checkSolutions(method: (Int) => Int): Unit = {
        // The multiples of 3 and 5 for integers x (1 <= x <= 9) should sum up to 23
        shouldSolve(method, 9, 23)
        // The multiples of 3 and 5 for integers x (1 <= x <= 999) should sum up to 233168
        shouldSolve(method, 999, 233168)
    }

    /**
      * Check whether a method produces the correct solution to a given problem.
      *
      * @param method the method to check
      * @param m m such that for all integers x (1 <= x <= m) the given method should produce the specified solution
      * @param solution the solution to the problem (the sum of all integers x (1 <= x <= m) for which holds that x
      *                 is divisible by 3 or by 5)
      */
    def shouldSolve(method: (Int) => Int, m: Int, solution: Int): Unit = {
        assert(method(m) == solution)
    }

}
