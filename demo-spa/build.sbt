npmDevDependencies in Compile ++= Seq(
   "webpack-merge" -> "4.1.1"
)


webpackConfigFile in fullOptJS := Some(baseDirectory.value / "prod.webpack.config.js")
