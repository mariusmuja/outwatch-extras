enablePlugins(ScalaJSBundlerPlugin)

name := "Outwatchtest"

version := "0.1.0"

organization := "Your organization"

scalaVersion := "2.11.11"

jsEnv := PhantomJSEnv().value

libraryDependencies ++= 
  "io.github.outwatch" %%% "outwatch" % "0.10.0-SNAPSHOT" ::
//  "io.monix" %%% "monix" % "2.3.0" ::
  "com.softwaremill.quicklens" %%% "quicklens" % "1.4.8" ::
  "com.github.japgolly.scalacss" %%% "core"  % "0.5.3" ::
  "org.scalatest" %%% "scalatest" % "3.0.1" % Test ::
  Nil

useYarn := true
//enableReloadWorkflow := true

//scalacOptions ++= Seq("-Xlog-implicits")
