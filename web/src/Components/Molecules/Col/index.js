import React from 'react'
import cn from 'classNames'
import './styles.scss'

/**
 * @molecule
 * @description
 *  The molecule contains information about how to behave in terms of 
 *  size and even dealing with changes in width of a screen. This relates
 *  to the grid element specifically.
 * @modifiers
 *  width               1-8 The width of the column to be specified.
 *  top/bottom/center   Alignement of the column vertically.
 *  gutter              The width of the gutter for the column.
 */
const Col = ({ children, width = false, top = false, bottom = false, center = false, gutter = false }) => {
  const c = cn('col', {
    [`col--${width}`]: width,
    'col--top': top,
    'col--bottom': bottom,
    'col--center': center,
    [`col--gutter${gutter}`]: gutter
  })
  return (
    <div className={c}>{children}</div>
  )
}
export default Col
