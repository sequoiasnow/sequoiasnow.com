import React     from 'react'
import PropTypes from 'prop-types'

/* --- Global --- */
import cn from 'classnames'

/* --- Local --- */
import './styles.scss'

/**
 * The toggle is a **stateless/controlled component** that can have multiple
 * options that may be selected. It is exeedingly simple, but easy and fun to 
 * use.
 */
const Toggle = ({ options = [], selected = 0, onClick = () => {} }) => (
  <div className="toggle">
    <div className="toggle__wrap">
      {options.map((label, i) => {
         const c = cn('toggle__item', {
           'toggle__item--selected': selected === i || selected == label
         })
         return (
           <span className={c} key={i} onClick={() => onClick(i, label)}>{label}</span>
         )
       })}
    </div>1
  </div>
)
Toggle.propTypes = {
  /**
   * The array of options that can be toggled between, a simple list.
   */
  options: PropTypes.arrayOf(
    PropTypes.node
  ),
  /**
   * The currently selected option, either the label provided, or an 
   * index.
   */
  selected: PropTypes.oneOfType([
    PropTypes.string,
    PropTypes.number
  ]),
  /**
   * The funciton to call when one of the toggles is clicked on. The first argument 
   * will be the index, the second will be the label, if one is provided.
   */
  onClick: PropTypes.func
}
export default Toggle
