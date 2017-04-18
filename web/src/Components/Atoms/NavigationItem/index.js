import React from 'react'
import cn from 'classNames'
import styles from './styles.scss'

const NavigationItem = ({ children, size = 'medium', selected = false}) => {
  const className = cn('nav-item', {
    'nav-item--selected': selected,
    [`nav-item--${size}`]: size
  })
  
  return (
    <div className={className}>
      {children}
    </div>
  )
}
export default NavigationItem
