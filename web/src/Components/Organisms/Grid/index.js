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
const Grid = ({ children, horizontal = true, gutter = 0, align = "top" }) => {
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
Grid.propTypes = {
  /**
   * If the grid is horizontal or vertical. Currently changing this will have 
   * no effect at all on the grid, it's an experimental feature.
   */
  horizontal: React.PropTypes.bool,
  /**
   * The size of the gutter of the Grid, 0-6.
   */
  gutter: React.PropTypes.oneOfType([
    React.PropTypes.number,
    React.PropTypes.string
  ]),
  /**
   * The alignment of the columns, top to bottom, bottom to top. This can also be set
   * individually by the columns.
   */
  align: React.PropTypes.oneOf(['top', 'bottom', 'center']),
}
export default Grid
