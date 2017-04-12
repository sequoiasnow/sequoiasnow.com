import React from 'react'
import classNames from 'classnames/bind'
import styles from './styles.scss'

/* Used for conveniant classNames */
const cn = classNames.bind(styles)

const Button = ({ label, onClick, inactive = false, color = false, size = false }) => {
  console.log(styles)
  const className = cn('btn', {
    'btn--inactive': inactive,
    [`btn--${color}`]: color,
    [`btn--${size}`]: size
  })
  
  return (
    <button className={className} onClick={(e) => ! inactive && onClick(e)}>{label}</button>
  )
}

export default Button
