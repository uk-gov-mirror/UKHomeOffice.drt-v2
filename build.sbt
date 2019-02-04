import sbt.Keys._
import sbt.Project.projectToRef
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import com.typesafe.config._

scalaVersion := Settings.versions.scala

// uncomment the following to get a breakdown  of where build time is spent
//enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)

// a special crossProject for configuring a JS/JVM/shared structure
lazy val shared = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
  .settings(
    scalaVersion := Settings.versions.scala,
    libraryDependencies ++= Settings.sharedDependencies.value
  )
  // set up settings specific to the JS project
  .jsConfigure(_ enablePlugins ScalaJSWeb)

val bundle = project.in(file("bundle"))

addCommandAlias("bundle", "bundle/bundle")

lazy val sharedJVM = shared.jvm.settings(name := "sharedJVM")

lazy val sharedJS = shared.js.settings(name := "sharedJS")

// use eliding to drop some debug code in the production build
lazy val elideOptions = settingKey[Seq[String]]("Set limit for elidable functions")

// instantiate the JS project for SBT with some additional settings
lazy val client: Project = (project in file("client"))
  .settings(
    name := "client",
    version := Settings.version,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= Settings.scalacOptions,
    libraryDependencies ++= Settings.scalajsDependencies.value,
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("drt.client.SPAMain"),
    webpackBundlingMode := BundlingMode.LibraryOnly(),
    version in webpack := "4.8.1",
    // by default we do development build, no eliding
    elideOptions := Seq(),
    scalacOptions ++= elideOptions.value,
    jsDependencies ++= Settings.jsDependencies.value,
    // reactjs testing
    requiresDOM := true,
    scalaJSStage in Test := FastOptStage,
    // 'new style js dependencies with scalaBundler'
    npmDependencies in Compile ++= Settings.clientNpmDependences,
    npmDevDependencies in Compile += Settings.clientNpmDevDependencies,
    // RuntimeDOM is needed for tests
    jsDependencies += RuntimeDOM % "test",
    useYarn := true,
    // yes, we want to package JS dependencies
    skip in packageJSDependencies := false,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.defaultLocal,
    // use uTest framework for tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalaJSUseMainModuleInitializer := true,
    parallelExecution in Test := false
  )
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(ScalaJSWeb)
  .dependsOn(sharedJS)

// Client projects (just one in this case)
lazy val clients = Seq(client)

lazy val drtSplits = (project in file("drt-splits"))
  .settings(
    name := "drt-splits",
    version := Settings.version,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= Settings.scalacOptions
  )

// instantiate the JVM project for SBT with some additional settings
lazy val server = (project in file("server"))
  .enablePlugins(PlayScala)
  .enablePlugins(WebScalaJSBundlerPlugin)
  .enablePlugins(BuildInfoPlugin)
  .disablePlugins(PlayLayoutPlugin) // use the standard directory layout instead of Play's custom
  .settings(
  name := "drt",
  version := Settings.version,
  scalaVersion := Settings.versions.scala,
  scalacOptions ++= Settings.scalacOptions,
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "buildinfo",
  javaOptions in Test += "-Duser.timezone=UTC",
  javaOptions in Test += "-Xmx1750m",
  javaOptions in Runtime += "-Duser.timezone=UTC",
  libraryDependencies ++= Settings.jvmDependencies.value,
  libraryDependencies += specs2 % Test,
  libraryDependencies += guice,
  dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-core" % "2.8.7",
  dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.7",
  dependencyOverrides += "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.8.7",
  commands += ReleaseCmd,
  // connect to the client project
  scalaJSProjects := clients,
  pipelineStages := Seq(scalaJSProd, digest, gzip),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  resolvers += Resolver.bintrayRepo("dwhjames", "maven"),
  resolvers += Resolver.bintrayRepo("mfglabs", "maven"),
  resolvers += "Artifactory Release Realm" at "http://artifactory.registered-traveller.homeoffice.gov.uk/artifactory/libs-release-local/",
  resolvers += "BeDataDriven" at "https://nexus.bedatadriven.com/content/groups/public",

  //dependencyOverrides += "com.github.dwhjames" %% "aws-wrap" % "0.9.0",
  publishArtifact in(Compile, packageBin) := false,
  // Disable scaladoc generation for this project (useless)
  publishArtifact in(Compile, packageDoc) := false,
  // Disable source jar for this project (useless)
  publishArtifact in(Compile, packageSrc) := false,
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  // compress CSS
  LessKeys.compress in Assets := true,
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
  ),
  TwirlKeys.templateImports += "buildinfo._",
  parallelExecution in Test := false,
  slick <<= slickCodeGenTask // register manual sbt command
)
  .aggregate(clients.map(projectToRef): _*)
  .dependsOn(sharedJVM, drtSplits)

// Command for building a release
lazy val ReleaseCmd = Command.command("release") {
  state =>
    "set elideOptions in client := Seq(\"-Xelide-below\", \"WARNING\")" ::
      "server/dist" ::
      "set elideOptions in client := Seq()" ::
      state
}

// code generation task
val conf = ConfigFactory.parseFile(new File("server/src/main/resources/application.conf")).resolve()

lazy val slick = TaskKey[Seq[File]]("gen-tables")
val tuple = (sourceManaged, dependencyClasspath in Compile, runner in Compile, streams)
lazy val slickCodeGenTask = tuple map { (dir, cp, r, s) =>
  val outputDir = (dir / "slick").getPath // place generated files in sbt's managed sources folder
  val url = conf.getString("aggregated-db.url")
  val jdbcDriver = "org.postgresql.Driver"
  val slickDriver = "slick.jdbc.PostgresProfile"
  val pkg = "drtdb"
  toError(r.run("slick.codegen.SourceCodeGenerator", cp.files, Array(slickDriver, jdbcDriver, url, outputDir, pkg), s.log))
  val fname = outputDir + "/" + pkg + "/Tables.scala"
  Seq(file(fname))
}

parallelExecution in Test := false
// loads the Play server project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value

// Docker Plugin§
enablePlugins(DockerPlugin)
// enabled for Alpine JVM docker image compatibility
enablePlugins(AshScriptPlugin)
