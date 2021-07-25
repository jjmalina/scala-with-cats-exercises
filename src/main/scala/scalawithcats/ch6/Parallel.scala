package scalawithcats.ch6

object ParallelApp extends App {
  /*
  6.4.0.1 Exercise: Parallel List

    Does List have a Parallel instance?
    If so, what does the Parallel instance do?

    List is a monad, and presumably there is an applicative for list which would
    combine two lists, so yes it must have a Parallel instance.
    Parallel for list would combine each element of multiple lists like a cartesian product
  */
}
