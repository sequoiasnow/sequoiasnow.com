import React     from 'react'
import PropTypes from 'prop-types'

/**
 * There really isn't much complicated going on here, a dotted line, is a dotted line
 * other than that, it has the added benefit of being cool looking. More often then 
 * not this is used as an alternative to the divider component, when its just a little
 * to brash or colorfull.
 */
export default () => (
  <svg style={{width: '100%'}} viewBox="0 0 400 30" xmlns="http://www.w3.org/2000/svg">
    <line stroke-width="2"
          stroke="#beb8bb" 
          strokeDasharray="15, 10, 5, 10" x1="0" y1="15" x2="400" y2="15" />
  </svg>
)
