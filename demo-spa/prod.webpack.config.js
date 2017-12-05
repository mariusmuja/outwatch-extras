var webpack = require('webpack');
var merge = require("webpack-merge");

var generatedConfig = require('./scalajs.webpack.config');

module.exports = merge(generatedConfig, {

  "plugins": [
    new webpack.optimize.UglifyJsPlugin({
      sourceMap: true
    })
  ]
})
