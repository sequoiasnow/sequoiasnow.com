import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/**
 * The ScrollContainer takes a variety of elements, positions them using flexbox, and 
 * then allows them to overflow scroll on the horizontal. Its an incredibly simple, 
 * but usefull component.
 */
const ScrollContainer = ({ children }) => (
  <div className="scroll-container">{children}</div>
)

export default ScrollContainer
