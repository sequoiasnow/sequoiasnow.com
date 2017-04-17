import React from 'react'
import classNames from 'classNames/bind'
import styles from './styles.scss'

const cn = classNames.bind(styles)

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
