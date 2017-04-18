import React from 'react'
import SidebarItem from '../../Atoms/SidebarItem'
import './styles.scss'

/**
 * @molecule Sidebar
 * @description
 *   The sidebar is a simple verital list of sidebar items.
 *   It is a rather simple list, and a controlled and stateless component.
 */
const Sidebar = ({ children }) => {
  return (
    <section className="sidebar">
      {children}
    </section>
  )
}
export default Sidebar
