import React from 'react'
import cn from 'classNames'
import './styles.scss'

/* --- Molecules --- */
import Col from '../../Molecules/Col'


/**
 * @organism
 * @description
 *   The grid contains a variety of columns's, it is an esential part of the design.
 *   And a simple positioning element.
 * @modifier
 *   horizontal  Default, this indicates a variety of columns to be passed.
 *   gutter      The space in between column elements.
 *   align       top/bottom/center Alignement of element's columns.
 */
const Grid = ({ children, horizontal = true, gutter = false, align = "top" }) => {
  const c = cn('grid', {
    [`grid--gutter${gutter}`]: gutter,
    [`grid--${align}`]: align,
    'grid-horizontal': horizontal
  }) 
  return (
    <div className={c}>
      {React.Children.map(children, (child, i) => {
         return React.cloneElement(child, { gutter }) 
       })}
    </div>
  )
}
export default Grid
