import React from 'react'
import cn from 'classnames'

/* --- Local --- */
import './styles.scss'

/**
 * @atom
 * @description
 *   A simple icon that is either closed or open. It accepts click events
 */
const IconClose = ({ onClick = () => {}, expanded = false }) => {
  return ( <div className={cn('icon-close', { 'icon-close--expanded': expanded })}
                onClick={onClick} /> )
}
IconClose.propTypes = {
  /* The function to fire on a click event, open state is not modified. */
  onClick: React.PropTypes.func,
  /* The current state of the icon. */
  open: React.PropTypes.bool
}
export default IconClose
