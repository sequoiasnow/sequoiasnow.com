import React from 'react'
import cn from 'classnames'
import './styles.scss'

/**
 * A small component that contains a clickable well of color. It is clickable
 * but does not retain it's own state.
 *
 * @atom
 */
const ColorWell = (props) => {
  const { color = 'default'
        , size = 'medium'
        , onClick = () => {}
        , selected = false } = props
  
  const c = cn('color-well', {
    'color-well--selected': selected,
    [`color-well--${color}`]: color,
    [`color-well--${size}`]: size
  }) 
      
  return ( <div className={c} onClick={onClick} /> )
}

export default ColorWell
