import React from 'react'
import NavigationItem from '../../Atoms/NavigationItem'

import './styles.scss'

/**
 * @molecule
 * @description
 *   A simple stateless component which takes a collection of children and
 *   wraps them to create a navigation bar. Does not preserve state.
 */
const NavigationBar = ({ children }) => {
  return (
    <div className="nav-bar">
      {children}
    </div> 
  ) 
}
export default NavigationBar;


