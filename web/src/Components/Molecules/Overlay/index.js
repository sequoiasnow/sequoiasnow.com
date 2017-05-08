import React     from 'react'
import PropTypes from 'prop-types'

/* --- Global --- */
import cn from 'classnames'

/* --- Local --- */
import './styles.scss'

/**
 * The Overaly component is simple in design, and in function.
 * When added to the dom it adds a set of styles to the body, which in
 * turn reveal a hidden fixed document on the side of the body. Inside
 * that are contained the children. When mobile, this overlay simply
 * covers the entire body, however, this is a __temporary__ effect that
 * may eventually be altered.
 *
 * The Overlay is similair in use, but seperate in function to the 
 * [Lightbox](/molecules/lightbox) component.
 */
export default class Overlay extends React.Component {
  static propTypes = {
    /**
     * All children passed can consist of anything in the overlay, some 
     * suggesstions might be to use a grid to position them.
     */
    children: PropTypes.node,
    /**
     * Should the overlay be visible or not. This allows for the component to be 
     * controlled and lends smoother animations then simply removing it from the 
     * dom
     */
    visible: PropTypes.bool,
    /**
     * The transform tells what exactly the overlay is supposed to look like when its
     * toggles.
     */
    transform: PropTypes.oneOf(['slide', 'rotate']) 
  }

  static defaultProps = {
    visible: false,
    transform: 'slide'
  }
  
  componentWillMount() {
    document.body.classList.add('body__overlay')
    document.body.classList.toggle('body__overlay--visible', this.props.visible)
    document.body.classList.toggle('body__overlay--rotate',
                                   this.props.transform == 'rotate')
  }

  componentWillReceiveProps(nextProps) {
    document.body.classList.toggle('body__overlay--visible', nextProps.visible)
    document.body.classList.toggle('body__overlay--rotate',
                                   this.props.transform == 'rotate') 
  }

  componentWillUnmount() {
    document.body.classList.remove('body__overlay',
                                   'body__overlay--visible',
                                   'body__overlay--rotate') 
  }
  
  render() {
    const { children, visible = false, transform = 'slide' } = this.props
    const c = cn('overlay', {
      'overlay--visible': visible,
      'overlay--rotate': transform == 'rotate'
    })

    return (
      <div className={c}>
        {children}
      </div>
    )
  }
}

