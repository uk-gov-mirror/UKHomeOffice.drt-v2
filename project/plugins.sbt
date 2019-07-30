// repository for Typesafe plugins
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("net.virtual-void" % "sbt-optimizer" % "0.1.2")

// the protobuf sbt plugin must come before the scalajs plugin, see
// https://github.com/scalapb/ScalaPB/issues/150
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.20")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.0-M1"
libraryDependencies += "com.typesafe" % "config" % "1.3.0"


addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.5.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.24")

addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.13.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-js-engine" % "1.2.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.6.15")

addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-gzip" % "1.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")
