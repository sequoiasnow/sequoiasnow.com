module.exports = [
  {
    test: /\.jsx?$/,
    exclude: /(node_modules|bower_components|public)/,
    use: [{
      loader: "babel-loader",
      query: {
        presets: ['es2015', 'react', 'stage-2'],
        plugins: ["transform-decorators-legacy"]
      }
    }]
  }, 
  {
    test: /\.css$/,
    exclude: /(node_modules|bower_components|public)/,
    use: [
      'style-loader',
      'css-loader'
    ]
  },
  {
    test: /\.scss$/,
    exclude: /[\/\\](node_modules|bower_components|public)[\/\\]/,
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
  },
  {
    test: /\.(txt|md)$/,
    loader: "raw-loader"
  },
  {
    test: /\.svg(\?v=\d+\.\d+\.\d+)?$/,
    exclude: /(node_modules|bower_components)/,
    use: [
      'svg-react-loader',
      {
        loader: "babel-loader",
        query: {
          presets: ['es2015', 'react', 'stage-2', { 'modules': false }],
          plugins: ["transform-decorators-legacy"]
        }
      } 
    ]
  },
  {
    test: /\.gif/,
    exclude: /(node_modules|bower_components)/,
    loader: "url-loader?limit=10000&mimetype=image/gif"
  },
  {
    test: /\.jpg/,
    exclude: /(node_modules|bower_components)/,
    loader: "url-loader?limit=10000&mimetype=image/jpg"
  },
  {
    test: /\.png/,
    exclude: /(node_modules|bower_components)/,
    loader: "url-loader?limit=10000&mimetype=image/png"
  },
  {
    test: /\.json/,
    loader: "json-loader"
  }
];
