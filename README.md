# ORF-saffari
Implementations of Online Random Forest [1] in Scala and Python.

Online random forests are ensembles of online decision trees. Online decision trees are updated when new observations
are presented. The observations need not be stored after they are used to update the trees / forest.

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
See `README.md` in the `sbt/` directory.


## Python
See `README.md` in the `python/` directory.


***

[1] Amir Saffari, Christian Leistner, Jakob Santner, Martin Godec, and Horst Bischof, "On-line Random Forests," 3rd IEEE ICCV Workshop on On-line Computer Vision, 2009.
