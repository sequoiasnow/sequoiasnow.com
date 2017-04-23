import React from 'react'
import cn from 'classnames'
import styles from './styles.scss'

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
    <div className={className} onClick={onClick}>
      {children}
    </div>
  )
}

SidebarItem.propTypes = {
  /**
   *  If this is the currently selected SidebarItem.
   */
  selected: React.PropTypes.bool,

  /**
   * An action to perform when the SidebarItem is clicked.
   */
  onClick: React.PropTypes.func
}

export default SidebarItem
