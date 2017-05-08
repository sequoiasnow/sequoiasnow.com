import React from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/* --- Atoms --- */


/* --- Global --- */
import cn from 'classnames'


/**
 * A simple tag with a hashtag, in the style first popularized by 
 * twitter.
 */
const Tag = ({ children, onClick, category = 'programing' }) => (
  <span className={cn('hashtag', `hashtag--${category}`)} onClick={onClick}>{children}</span>
)

Tag.propTypes = {
  /**
   * Whatever to do when the tag is clicked.
   */
  onClick: PropTypes.func,
  /**
   * The category of the tag, which lends it it's specific color.
   */
  category: PropTypes.oneOf(allCategories)
}

  Tag.defaultProps = {
    onClick: () => {}
  }

  export default Tag
