# ORF-saffari
My implementation of Online Random Forest by Saffari

# Compiling ORF.scala
Here is the simplest and recommended way of compiling and using `ORF.scala`.

## To Compile
To generate a light jar file, 

```bash
cd etc
./lightScalaCompile
```

A new jar file `orf.jar` will appear in `etc`.
You can use this jar in a scala session by adding the classpath like so:

```bash
scala -cp orf.jar
```

And import it in scala like this:

```scala
import ORF._
```



# Generating the jar file with sbt (if you are developing the package)
In order to generate the `jar` file, you will first need to cd into
`sbt/src/test/resources/usps` and then `./download_data`. This is 
so that the `usps` does not need to be tracked by git. After this, the
tests provided will be executed, and should be executed successfully
in a minute.

Then, in the `sbt` directory in the terminal, execute
`sbt assembly`. This will generate `orf.jar` in 
`sbt/scala/target/scala-2.10/`.

