# ORF-saffari
My implementation of Online Random Forest by Saffari

## Scala

### Generating the jar file with sbt
In the `sbt` directory in the terminal, execute
`sbt package`. This will generate `orf_2.10-versionNumber.jar` in 
`sbt/scala/target/scala-2.10/`. To use it in a scala session, execute
`scala -cp orf_2.10-versionNumber.jar`. Import in scala using 
`import ORF.models._ `.


### Developing and Testing
Before testing, make sure to download the usps dataset. Go to
`sbt/src/test/resources/usps` and then execute `./download_data`.  After this,
the tests provided should executed properly by `sbt ~test` in the `sbt`
directory. The test currently takes about a minute.


### Example usage:
Coming... For now, just look at `sbt/src/test/scala/ConfirmResults.scala`.


## Python
More details soon...
