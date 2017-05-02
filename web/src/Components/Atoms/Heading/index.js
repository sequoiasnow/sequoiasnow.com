import React from 'react'
import cn from 'classNames'

/* ---  --- */

import './styles.scss'

/**
 * @atom Heading
 * @description
 *  The heading is a simple wrapper for a span element. It is used primarily
 *  to create a variety of different headings, and the styles which apply to
 *  it apply also to the standard h1, h2, h3 elements.
 * @markup
 *  <Heading size="medium" center>Hello World</Heading>
 */
const Heading = ({ children, color, size, center, left, right }) => {
  const c = cn('heading', {
    [`heading--${color}`]: color,
    [`heading--${size}`]: size,
    'heading--center': center,
    'heading--left': left,
    'heading--right': right
  })
  return ( <span className={c}>{children}</span> )
}

Heading.defaultProps = {
  color: '',
  size: 'medium',
  center: false,
  left: false,
  right: false
}

Heading.propTypes = {
  /**
   * The color of the heading element.
   */
  color: React.PropTypes.string,
  /**
   * The size of the heading, also affects the size of the margin.
   */
  size: React.PropTypes.string,
  /**
   * Centers the heading.
   */
  center: React.PropTypes.bool,
  /**
   * Aligns the heading to the left.
   */
  left: React.PropTypes.bool,
  /**
   * Aligns the heading to the right.
   */
  right: React.PropTypes.bool
}

export default Heading
