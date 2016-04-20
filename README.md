# diener

Utility monad stack to be used in servant

Instead of running in IO, you can use servant with the monad called "Diener". It has

* logging functionality
* a reader (among others: for database connections)
* its own errors
* convenience function for lifts

