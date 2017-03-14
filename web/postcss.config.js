module.exports = {
  plugins: [
    require('postcss-cssnext')({
      features: {
        customProperties: {
          preserve: true,
          variables: { theme: 230, 'dark-color': 'black' }
        }
      }
    })
    // Included by css next
    // require('autoprefixer')({ browsers: AUTOPREFIXER_BROWSERS })
  ]
}
