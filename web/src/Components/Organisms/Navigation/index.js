import React from 'react'

/* --- Global --- */
import { Link } from 'react-router'

/* --- Atoms --- */
import NavigationItem from '../../Atoms/NavigationItem'

/* --- Molecules --- */
import NavigationBar from '../../Molecules/NavigationBar'


/**
 * All links for the primary navigation bar
 */
const links =
  [ { label: 'Home', href: '/' }
  , { label: 'Blog', href: '/blog' }  
  ]

/**
 * Displays the primary navigation for the site, it can be considered
 * a navigation bar with the data included.
 *
 * The markup looks something like the following
 * 
 * ```jsx
 * <Navigation />
 * ```
 */
const Navigation = () => (
  <NavigationBar>
    {links.map(({ label, href }) => (
       <NavigationItem>
         <Link to={href}>{label}</Link>
       </NavigationItem>
     ))}
  </NavigationBar>
)
export default Navigation
