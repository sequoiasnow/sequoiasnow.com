import React from 'react'
import classNames from 'classnames/bind'
import styles from './styles.scss'

const cn = classNames.bind(styles)

/**
 * @atom
 * @description
 *   The SidebarItem is an item in the vertical list of sidebar options. 
 *   It is meant to be clickable, and has a range of possible actions
 *   associated with it.
 * @modifiers
 *   selected     If this is the currently selected SidebarItem.
 *   onClick      An action to perform when the SidebarItem is clicked.
 */
const SidebarItem = ({ children, selected = false, onClick = () => {} }) => {
  const className = cn('sidebar-item', {
    'sidebar-item--selected': selected
  })
  
  return (
    <div className={className}>
      {children}
    </div>
  )
}
export default SidebarItem
