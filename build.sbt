inThisBuild(Seq(
  organization := "com.github.mariusmuja",
  version := "0.1.2-SNAPSHOT",
  scalaVersion := crossScalaVersions.value.head,
  crossScalaVersions := Seq("2.12.4", "2.11.11"),
  scalacOptions in Compile ++= Seq(
    "-deprecation",
    "-feature"
  ),
  javacOptions in Compile ++= Seq(
    "-source", "1.7",
    "-target", "1.7"
  ),
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  publishArtifact in Test := false
)
)

val outwatchVersion = "0.11.1-SNAPSHOT"

val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

lazy val root = project.in(file("."))
  .aggregate(app, redux, styles, mdl, router)
  .settings(noPublish: _*)


lazy val app = project.in(file("demo-spa"))
  .settings(
    name := "demo-spa",
    jsEnv := PhantomJSEnv().value,
    useYarn := true,
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    scalaJSUseMainModuleInitializer := true
  )
  .settings(noPublish: _*)
  .dependsOn(redux, styles, mdl, router)
  .enablePlugins(ScalaJSBundlerPlugin)


lazy val styles = project.in(file("outwatch-styles"))
  .settings(
    name := "outwatch-styles",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
        "com.github.japgolly.scalacss" %%% "core" % "0.5.3" ::
        Nil
  )
  .enablePlugins(ScalaJSPlugin)


lazy val mdl = project.in(file("outwatch-mdl"))
  .settings(
    name := "outwatch-mdl",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
        Nil
  )
  .enablePlugins(ScalaJSPlugin)

lazy val router = project.in(file("outwatch-router"))
  .settings(
    name := "outwatch-router",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
        "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
        Nil
  )
  .enablePlugins(ScalaJSPlugin)


lazy val redux = project.in(file("outwatch-redux"))
  .settings(
    name := "outwatch-redux",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
        Nil
  )
  .enablePlugins(ScalaJSPlugin)

