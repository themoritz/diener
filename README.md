# diener

Utility monad stack to be used in servant

Instead of running in IO, you can use servant with the monad called "Diener". It has

* logging functionality
* a reader (among others: for database connections)
* its own errors
* convenience function for lifts

## example-persistent

A full example web-server application, using Sqlite3 and logging

## example-mongodb

A full example web-server application, using MongoDB.
Check out the `handler.hs` to see how you can use type-classes to fine-tune effects.
