const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  entry: "./src/index.tsx",
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, "dist")
  },

  mode: "development",

  devtool: "inline-source-map",
  devServer: {
    contentBase: "./dist/"
  },

  resolve: {
    extensions: [".ts", ".tsx", ".js", ".json"]
  },

  module: {
    rules: [
      { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
      { enforce: "pre", test: /\.js$/, loader: "source-map-loader" }
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.html"
    })
  ],

  externals: {
    "react": "React",
    "react-dom": "ReactDOM"
  }
}