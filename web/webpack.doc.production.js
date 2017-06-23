var webpack = require('webpack');
var path = require('path');
var loaders = require('./webpack.rules');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var WebpackCleanupPlugin = require('webpack-cleanup-plugin');

// local scss modules
loaders.push({
  test: /[\/\\]src[\/\\].*\.scss/,
  exclude: /(node_modules|bower_components|public)/,
  loader: ExtractTextPlugin.extract({
     use: [
      'style-loader',
      {
        loader: 'css-loader',
        options: {
          importLoaders: 1
        }
      },
      {
        loader: 'postcss-loader',
        options: {
          plugins: function() {
            return [ require('autoprefixer') ]
          }
        }
      },
      {
        loader: 'sass-loader',
        options: {
          includePaths: ['./src/Components']
        }
      }
    ]
 })
});
// global css files
loaders.push({
  test: /[\/\\](node_modules|global)[\/\\].*\.css$/,
  loader: ExtractTextPlugin.extract('css-loader')
});

module.exports = {
  entry: [
    'whatwg-fetch',
    './src/styleguide.js'
  ],
  output: {
    path: path.join(__dirname, 'style'),
    filename: '[chunkhash].js'
  },
  resolve: {
    extensions: ['.js', '.jsx']
  },
  module: {
    rules: loaders
  },
  plugins: [
    new WebpackCleanupPlugin(),
    new webpack.DefinePlugin({
      'process.env': {
	NODE_ENV: '"production"'
      }
    }),
   new webpack.ProvidePlugin({
      'Promise': 'es6-promise'
    }),
    new ExtractTextPlugin({ 
      filename: '[contenthash].css', 
      allChunks: true
    }),
    new HtmlWebpackPlugin({
      template: './src/template.html',
      title: 'Webpack App'
    }),
  ]
};
