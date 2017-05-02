import React from 'react'
import cn from 'classnames'

/* --- Atoms --- */
import IconClose from '../../Atoms/IconClose'

/* --- Local --- */
import './styles.scss'


/**
 * @atom LightBox
 * @description
 *   The lightbox simply dimms the entire screen and presents children.
 *   by default these children are centered, but if that is not prefered
 *   they can be removed. A lightbox may also be changed in severity by
 *   passing a boolean to opacity and is directly controlled. 
 */
export default class LightBox extends React.Component {
  static propTypes = {
    /**
     * Is the lightbox currently active?
     */
    active: React.PropTypes.bool
  }

  static defaultProps = { active: false }
  
  constructor(props) {
    super(props)
    this.handleClickOutside = this.handleClickOutside.bind(this)
  }

  /**
   * Bound by an on click event of the body
   */
  handleClickOutside(event) {
    if ( this.props.onClose && this.contentRef && !this.contentRef.contains(event.target) )
      this.props.onClose()
  }
  
  componentDidMount() {
    document.addEventListener('mousedown', this.handleClickOutside)
  }
  componentWillUnmount() {
    document.removeEventListener('mousedown', this.handleClickOutside)
  } 
  
  render() {
    const { active = false, children } = this.props
    return (
      <div className={cn('lightbox', { 'lightbox--active': active })}>
        <div className="lightbox__icon"><IconClose expanded/></div>
        <div className="lightbox__container" ref={(ref) => this.contentRef = ref}>  
          {children}                   
        </div>
      </div> 
    )
  }
}

