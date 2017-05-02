import React from 'react'

/* --- Local --- */
import './styles.scss'

/* --- Molecules --- */
import LightBox from '../LightBox'

/**
 * @atom Alert
 * @description
 *  Shows an alert when displayed in the dom, instead of a window.alert, simply include
 *  this component.
 */
const Alert = ({ children, options = [], active = true }) => ( 
  <LightBox active={active}>
    <article className="alert">
      <section className="alert__content"> 
        {children}
      </section>
      <footer className="alert__footer"> 
        {options.map(({ label, onClick }) => <span key={label} onClick={onClick}>{label}</span>)}
      </footer>
    </article>
  </LightBox>
)
Alert.propTypes = {
  /**
   * Possible items to click on, pass both an onClick event and a label.
   */
  options: React.PropTypes.arrayOf(
    React.PropTypes.shape({
      label: React.PropTypes.string,
      onClick: React.PropTypes.func
    })
  ),
  /**
   * Wether the alert button is shown or not. This is really only usefull for controlling 
   * the component.
   */
  active: React.PropTypes.bool
}
export default Alert
