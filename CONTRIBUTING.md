## Building a native-image

First, install [Jabba](https://github.com/shyiko/jabba).

Next, install GraalVM 20.1.0
```
jabba install graalvm-ce-java11@20.1.0
```

Next, run `sbt native-image`.
This step may take a while to complete.
Once it's done, the binary should be ready to use

```
./fastpass/target/graalvm-native-image/fastpass help
```
