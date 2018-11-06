
inThisBuild(Seq(
  version := "0.4.0-"+git.gitHeadCommit.value.get.take(8),
  organization := "com.github.mariusmuja",
  scalaVersion := crossScalaVersions.value.head,
  crossScalaVersions := Seq("2.12.7", "2.11.12"),
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
  resolvers += "jitpack" at "https://jitpack.io",
  publishArtifact in Test := false,
)
)


val outwatch = Def.setting("com.github.mariusmuja" %%% "outwatch" % "1.0.0-RC1-d5f5a844" )
//val outwatch = Def.setting("io.github.outwatch" %%% "outwatch" % "0.11.1-SNAPSHOT")

val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

lazy val extras = project.in(file("."))
  .settings(
    name := "outwatch-extras"
  )
  .aggregate(app, redux, styles, mdl, router, util)
  .dependsOn(redux, styles, mdl, router, util)
  .enablePlugins(ScalaJSPlugin, GitVersioning)


lazy val app = project.in(file("demo-spa"))
  .settings(
    name := "demo-spa",
    useYarn := true,
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    scalaJSUseMainModuleInitializer := true
  )
  .settings(noPublish: _*)
  .dependsOn(redux, styles, mdl, router)
  .enablePlugins(ScalaJSBundlerPlugin, GitVersioning)


lazy val styles = project.in(file("outwatch-styles"))
  .settings(
    name := "outwatch-styles",
    libraryDependencies ++=
      outwatch.value ::
      "com.github.japgolly.scalacss" %%% "core" % "0.5.4" ::
      Nil
  )
  .enablePlugins(ScalaJSPlugin, GitVersioning)

lazy val util = project.in(file("outwatch-util"))
  .settings(
    name := "outwatch-util",
    libraryDependencies ++=
      outwatch.value ::
      Nil
  )
  .enablePlugins(ScalaJSPlugin, GitVersioning)

lazy val mdl = project.in(file("outwatch-mdl"))
  .settings(
    name := "outwatch-mdl",
    libraryDependencies ++=
      outwatch.value ::
      Nil
  )
  .enablePlugins(ScalaJSPlugin, GitVersioning)

lazy val router = project.in(file("outwatch-router"))
  .settings(
    name := "outwatch-router",
    libraryDependencies ++=
      outwatch.value ::
      "com.chuusai" %%% "shapeless" % "2.3.3" ::
      Nil
  )
  .dependsOn(util)
  .enablePlugins(ScalaJSPlugin, GitVersioning)

lazy val redux = project.in(file("outwatch-redux"))
  .settings(
    name := "outwatch-redux",
    libraryDependencies ++=
      outwatch.value ::
      Nil
  )
  .dependsOn(util)
  .enablePlugins(ScalaJSPlugin, GitVersioning)
