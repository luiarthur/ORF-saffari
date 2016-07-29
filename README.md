# ORF-saffari
My implementation of Online Random Forest by Saffari

# Generating the jar file
In order to generate the `jar` file, you will first need to cd into
`scala/src/test/resources/usps` and then `./download_data`. This is 
so that the `usps` does not need to be tracked by git. After this, the
tests provided will be executed, and should be executed successfully
in a minute.

Then, in the `scala` directory in the terminal, execute
`sbt assembly`. This will generate `orf.jar` in 
`scala/target/scala-2.10/`.
