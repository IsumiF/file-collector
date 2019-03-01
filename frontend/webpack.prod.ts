import webpack from 'webpack';
import merge from "webpack-merge";
import common from "./webpack.common";
import HtmlWebpackPlugin from "html-webpack-plugin";

const config: webpack.Configuration = merge(common, {
  mode: 'production',
  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.dev.html"
    })
  ]
});

export default config;