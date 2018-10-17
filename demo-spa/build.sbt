npmDevDependencies in Compile ++= Seq(
   "webpack-merge" -> "4.1.3"
)


webpackConfigFile in fullOptJS := Some(baseDirectory.value / "prod.webpack.config.js")
