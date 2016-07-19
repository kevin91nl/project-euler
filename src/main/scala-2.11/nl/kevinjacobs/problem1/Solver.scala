package nl.kevinjacobs.problem1

/**
  * Created by kevin on 19.07.16.
  */
class Solver {

    /**
      * Sum up all integers x (for 1 <= x <= m) for which holds that x is divisible by 3
      * or x is divisible by 5. A functional style is used here and some mathematics are
      * used which makes this solution more elegant than the naive solution.
      *
      * @param m the upperbound for the check
      * @return sum of all integers x (1 <= x <= m) for which holds that x % 3 == 0 or x % 5 == 0
      */
    def solveSmart(m: Int): Int = {
        def sumGauss(n: Int): Int = n * (n + 1) / 2
        def countDivisors(d: Int): Int = (m - m % d) / d
        3 * sumGauss(countDivisors(3)) + 5 * sumGauss(countDivisors(5)) - 15 * sumGauss(countDivisors(15))
    }

    /**
      * Sum up all integers x (for 1 <= x <= m) for which holds that x is divisible by 3
      * or x is divisible by 5. A functional style is used here.
      *
      * @param m the upperbound for the check
      * @return sum of all integers x (1 <= x <= m) for which holds that x % 3 == 0 or x % 5 == 0
      */
    def solveNaivelyFunctional(m: Int): Int = {
        // Note that the functional solution is much cleaner and smaller than the imperative solution
        if (m == 1) return 0
        val partialSum = if (m % 3 == 0 || m % 5 == 0) m else 0
        solveNaivelyFunctional(m - 1) + partialSum
    }

    /**
      * Sum up all integers x (for 1 <= x <= m) for which holds that x is divisible by 3
      * or x is divisible by 5. An imperative style is used here.
      *
      * @param m the upperbound for the check
      * @return sum of all integers x (1 <= x <= m) for which holds that x % 3 == 0 or x % 5 == 0
      */
    def solveNaivelyImperative(m: Int): Int = {
        // Since sum is a var, we don't know how it will change over time
        var sum = 0
        for (i <- 1 to m) {
            // Here the magic happens to our sum
            if (i % 3 == 0 || i % 5 == 0) {
                sum += i
            }
        }
        sum
    }

}
