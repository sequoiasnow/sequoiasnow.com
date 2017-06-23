import React from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/* --- Atoms --- */
import { allCategories } from '../Category'

/* --- Global --- */
import cn from 'classnames'


/**
 * A simple tag with a hashtag, in the style first popularized by 
 * twitter.
 */
const Tag = ({ children, onClick, category = 'programing', simple = false, selected }) => (
  <span className={cn('hashtag', `hashtag--${category}`, {
      'hashtag--simple': simple,
      'hashtag--selected': selected
    })}
        onClick={onClick}>
    {children}
  </span>
)

Tag.propTypes = {
  /**
   * Whatever to do when the tag is clicked.
   */
  onClick: PropTypes.func,
  /**
   * The category of the tag, which lends it it's specific color.
   */
  category: PropTypes.oneOf(allCategories),
  /**
   * Simple will make the tag borderless and slightly smaller.
   */
  simple: PropTypes.bool
}

Tag.defaultProps = {
  onClick: () => {}
}

export default Tag
