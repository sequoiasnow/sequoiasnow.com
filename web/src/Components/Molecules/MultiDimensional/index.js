import React     from 'react'
import PropTypes from 'prop-types'

/* --- Global --- */
import cn from 'classnames'
import Prefixer from 'inline-style-prefixer'

/* --- Local --- */
import './styles.scss'

/* Create the style prefixer, this will create whatever prefixes the current web 
 * browser needs, and no more */
const prefixer = new Prefixer()

/**
 * Thee MultiDimensional component is one of the 
 * most complex elements retained in the molecules folder. 
 *
 * Much of this complexity comes from the fact that the
 * multidimensional container actually contains **four** areas to store
 * content. Above, below, left, right. It works as follows. The central content
 * it identified as Center (these are found in the atoms section). Each of these
 * is then hidden from view inside the container. The container will then transition
 * to each of these components individually. By providing **scroll** to one of the 
 * subcomponents, that component will contain scrolling ability, and the sides will
 * be unaffected by scroll. Ideally both left and right will have the same height
 * as the center. Any nested fixed position elements will break the paradime.
 */
export default class MultiDimensional extends React.Component {
  static propTypes = {
    /**
     * The children must by definition be either DimensionLeft, DimensionRight,
     * DimensionTop, DimensionBottom, DimensionCenter or else the css won't make 
     * sense.
     */
    children: PropTypes.node, 
    /**
     * The component to insert on the lefthand side.
     */
    left: PropTypes.node,
    /**
     * The component to insert on the righthand side.
     */
    right: PropTypes.node, 
    /**
     * Should the MultiDimensional container attempt to take up 
     * all space. In most cases the answer should always be yes :-)
     */
    fullWidth: PropTypes.bool, 
    /**
     * What is the currently selected dimension. This is given as a string, in the 
     * future this might be given by a more complex ref type situation.
     */
    selected: PropTypes.oneOf(['center', 'left', 'right']),
    /**
     * The transition determines how exactly the reveal is supposed to occur.
     */
    transition: PropTypes.shape({
      type: PropTypes.oneOf(['rotate', 'slide']),
      angle: PropTypes.number,
      transformOrigin: PropTypes.string
    })
  }

  static defaultProps = {
    selected: 'center',
    fullWidth: false,
    transition: {
      type: 'slide' 
    }
  }

  /**
   * Despite appearences, this does not get the width, It actually tells 
   * how much to transofm in the x direction for a slide, transition. This is
   * negative for x and positive for y.
   */
 
  _getTx(side) {
    if ( side == 'left' && this.left ) {
      console.log(this.left)
      return (this.left.offsetWidth) + 'px'
    }
    if ( side == 'right' && this.right ) {
      return (-1 * this.right.offsetWidth) + 'px'
    } 
    return '100%'
  }
  
  render() {
    const { transition, selected, children, left, right } = this.props
    
    let transform = 'translateX(0)'
    let defaultTransformOrigin = '50% 50%'
    if ( transition.type == 'slide' ) {
      const w = selected == 'right' ? this._getTx('right') :
                (selected == 'left' ? this._getTx('left') : '0px')
      transform = `translateX(${w})` 
    } else if ( transition.type == 'rotate' ) {
      const degree = transition.degree ? parseInt(transition.degree, 10) + 'deg' : '45deg' 
      if ( selected == 'right' ) {
        defaultTransformOrigin = '0% 100%'
        transform = `rotate(${degree})`
      } else if ( selected == 'left' ) {
        defaultTransformOrigin = '0% 0%' 
        transform = `rotate(-${degree})`
      }
    }
    
    const styles = {
      transformOrigin: transition.transformOrigin || defaultTransformOrigin,
      transform
    } 
    return (
      <div className="multidim">
        <div className="multidim__container" style={prefixer.prefix(styles)}>
          {left && <div className="dimension dimension--left" ref={(r) => this.left = r}>{left}</div>}
          <div className="dimension dimension--center">
            {children}
          </div>
          {right && <div className="dimension dimension--right" ref={(r) => this.right = r}>{right}</div>}
        </div>
      </div>
    )
  }
}
