import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/* --- Global --- */
import cn from 'classnames'


const path = `
   M0,58.7
   L370.8,58.7
   L376.1,46.2
   L381.1,58.7
   L389.9,58.7
   L393.8,66.5
   L402.8,22.7
   L410.6,78.7
   L415.3,58.7
   L426.4,58.7
   L434.4,54.7
   L442.4,58.7
   L800,58.7 `

const expPath = `
   M0,58.7
   L370.8,58.7
   L376.1,46.2
   L381.1,58.7
   L389.9,58.7
   L393.8,66.5
   L402.8,22.7
   L410.6,78.7
   L415.3,58.7
   L426.4,58.7
   L434.4,54.7
   L442.4,58.7
   L800,58.7
   L800, 800
   L0,800
   Z
`

/**
 *   Takes the place of the `<hr />` tag. Basically, the divider looks
 *   like a heartbeat and can have a variety of colors associated with it.
 *   You can also use it as a way to show the percentage, this is done by 
 *   providing a 0 to 1 value for value.
 *  
 *   An example would be shown as 
 *
 *   ```jsx
 *     <Divider value={0.5} />
 *   ```
 */
export default class Divider extends React.Component {
  constructor(props) {
    super(props)
    this.state = { expanded: this.props.expanded || false }
    this.onClick = this.onClick.bind(this)
  }

  onClick() {
    if ( this.props.children && ! this.props.clickable ) {
      this.setState({ expanded: !this.state.expanded })
    } 
  }
  
  render() {
    const { value = 0, children, left = null, right = null } = this.props
    const { expanded } = this.state
    
    return (
      <div className={cn('divider', { 'divider--has-content': children, 'divider--expanded': expanded })}>
        <svg className="divider__svg"
             xmlns="http://www.w3.org/2000/svg"
             viewBox={expanded ? '0 0 800 800' : '0 0 800 100'}
             preserveAspectRatio={expanded ? 'none' : true}
             onClick={this.onClick}>
          <path fill="none" className="divider__line" d={expanded ? expPath : path} />
          {expanded || <path fill="none"
                             className="divider__line-2"
                             d={path}
                             strokeDashoffset={(1 - value) * 925}
                             strokeDasharray={925} />}
        </svg>
        <div className="divider__left">{left}</div>
        <div className="divider__right">{right}</div>
        {children && (
           <div className="divider__content">{children}</div>
         )}
      </div>
    )
  }
      
  static propTypes = {
    /**
     * The value of the divider. A number from zero to 1.
     */
    value: PropTypes.number
  }
}
