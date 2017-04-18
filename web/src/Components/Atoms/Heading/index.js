import React from 'react'
import cn from 'classNames'
import './styles.scss'

const Heading = (props) => {
  const { children
        , color = false
        , size = 'medium'
        , center = false
        , left = false
        , right = false } = props
  const c = cn('heading', {
    [`heading--${color}`]: color,
    [`heading--${size}`]: size,
    'heading--center': center,
    'heading--left': left,
    'heading--right': right
  })
  return ( <span className={c}>{children}</span> )
}
export default Heading
