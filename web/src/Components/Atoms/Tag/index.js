import React from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/**
 * A simple tag with a hashtag, in the style first popularized by 
 * twitter.
 */
const Tag = ({ children, onClick }) => (
  <span className="hashtag" onClick={onClick}>{children}</span>
)

Tag.propTypes = {
  /**
   * Whatever to do when the tag is clicked.
   */
  onClick: PropTypes.func
}

Tag.defaultProps = {
  onClick: () => {}
}

export default Tag
