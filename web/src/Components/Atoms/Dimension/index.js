import React     from 'react'
import PropTypes from 'prop-types'

/* --- Global --- */
import cn from 'classnames'

/* --- Local --- */
import './styles.scss'

/* --- Atoms --- */
import { allColors } from '../Colors'


/**
 * In order to create the multidimensional view, the dimension component uses 
 * simple logic to move about within the dimensional view. From the prospective of 
 * this component, there is no internal logic.
 *
 * Some of the more impressive logic of the dimension component comes in the 
 * manner of its appearence, this can be a sideways tilt, or a simple scaling
 * factor. In future versions this dynmaic behavior may be included in this component,
 * for now it is relegated to the [MultiDimension](/molecules/MultiDimension) component.
 */
const Dimension = ({ children, center, left, right, color, padding = false }) => (
  <div className={cn('dimension', {
      'dimension--center': center || !(center || left || right),
      'dimension--left': left,
      'dimension--right': right,
      [`dimension--${color}`]: color,
      'dimension--padding': padding
    })}>
    {children}
  </div>
)

Dimension.propTypes = {
  /**
   * Is this the center component? this is the default.
   */
  center: PropTypes.bool,
  /**
   * Is this the left component?
   */
  left: PropTypes.bool,
  /**
   * Is this the right component?
   */
  right: PropTypes.bool,
  /**
   * The color of the component.
   */
  color: PropTypes.oneOf(allColors),
  /**
   * Should the component provide a small layer of padding around
   * the content. **this feature will be depracted in future versions**.
   * Instead please use the grid component for the desired effect.
   */
  padding: PropTypes.bool
} 

export default Dimension
