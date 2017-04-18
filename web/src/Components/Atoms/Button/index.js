import React from 'react'
import cn from 'classnames'
import './styles.scss'

/**
 * @atom Button
 * @description
 *  A simpel button that can change color and handle click events.
 * @modifer
 *  inactive   An inactive button.
 *  color      The button's color
 *  size       The size of the button
 */
const Button = ({ children, onClick, inactive = false, color = false, size = false }) => {
  const className = cn('btn', {
    'btn--inactive': inactive,
    [`btn--${color}`]: color,
    [`btn--${size}`]: size
  })
  
  return (
    <button className={className} onClick={(e) => ! inactive && onClick(e)}>{children}</button>
  )
}

export default Button
