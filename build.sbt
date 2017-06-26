inThisBuild(Seq(
  organization := "io.github.mariusmuja",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.11",
  scalacOptions in Compile ++= Seq(
    "-deprecation",
    "-feature"
  ),
  javacOptions in Compile ++= Seq(
    "-source", "1.7", 
    "-target", "1.7"
  )
))

val outwatchVersion = "0.10.0-SNAPSHOT"


lazy val root = project.in(file("."))
  .aggregate(app, extras, styles, mdl)

  
lazy val app = project.in(file("demo-spa")).settings(
  name := "demo-spa",
  jsEnv := PhantomJSEnv().value,
  useYarn := true
).dependsOn(extras, styles, mdl)
  .enablePlugins(ScalaJSBundlerPlugin)


lazy val styles = project.in(file("outwatch-styles")).settings(
  name := "outwatch-styles",
  libraryDependencies ++= 
    "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
    "com.github.japgolly.scalacss" %%% "core"  % "0.5.3" ::
    Nil
)
  .enablePlugins(ScalaJSPlugin)


lazy val mdl = project.in(file("outwatch-mdl")).settings(
  name := "outwatch-mdl",
  libraryDependencies ++= 
    "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
    "com.github.japgolly.scalacss" %%% "core"  % "0.5.3" ::
    Nil
)
  .enablePlugins(ScalaJSPlugin)


lazy val extras = project.in(file("outwatch-extras")).settings(
  name := "outwatch-extras",
  libraryDependencies ++= 
    "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
    "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
    Nil
)
  .dependsOn(styles)
  .enablePlugins(ScalaJSPlugin)

