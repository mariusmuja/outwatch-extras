
inThisBuild(Seq(
  organization := "com.github.mariusmuja",
  version := "0.1.2-monix-SNAPSHOT",
  scalaVersion := crossScalaVersions.value.head,
  crossScalaVersions := Seq("2.12.4", "2.11.11"),
  javacOptions in Compile ++= Seq(
    "-source", "1.7",
    "-target", "1.7"
  ),
  scalacOptions += {
    val local = baseDirectory.value.toURI
    val remote = s"https://raw.githubusercontent.com/mariusmuja/outwatch-extras/${git.gitHeadCommit.value.get}/"
    s"-P:scalajs:mapSourceURI:$local->$remote"
  },
  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    "-Xcheckinit" ::
    "-Xfuture" ::
    "-Xlint" ::
    "-Ypartial-unification" ::
    "-Yno-adapted-args" ::
    "-Ywarn-infer-any" ::
    "-Ywarn-value-discard" ::
    "-Ywarn-nullary-override" ::
    "-Ywarn-nullary-unit" ::
    "-P:scalajs:sjsDefinedByDefault" ::
    Nil,
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  publishArtifact in Test := false
)
)

val outwatchVersion = "0.11.1-monix-SNAPSHOT"

val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

lazy val extras = project.in(file("."))
  .settings(
    name := "outwatch-extras",
  )
  .aggregate(app, redux, styles, mdl, router, util)
  .dependsOn(redux, styles, mdl, router, util)
  .enablePlugins(ScalaJSPlugin)


lazy val app = project.in(file("demo-spa"))
  .settings(
    name := "demo-spa",
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
        "com.github.japgolly.scalacss" %%% "core" % "0.5.4" ::
        Nil
  )
  .enablePlugins(ScalaJSPlugin)

lazy val util = project.in(file("outwatch-util"))
  .settings(
    name := "outwatch-util",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
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
  .dependsOn(util)
  .enablePlugins(ScalaJSPlugin)

lazy val redux = project.in(file("outwatch-redux"))
  .settings(
    name := "outwatch-redux",
    libraryDependencies ++=
      "io.github.outwatch" %%% "outwatch" % outwatchVersion ::
        Nil
  )
  .dependsOn(util)
  .enablePlugins(ScalaJSPlugin)

