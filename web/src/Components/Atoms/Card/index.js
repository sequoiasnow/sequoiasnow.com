import React from 'react'
import cn from 'classNames'
import './styles.scss'

/** 
 * @atom
 * @description
 *   A simple presentational component to give a background to contenent.
 * @modifiers
 *   dark   A darker color card.
 */
const Card = ({ children, dark = false }) => {
  const c = cn('card', { 'card--dark': dark })
  return (
    <div className={c}>
      <div className="card__container">{children}</div>
    </div>
  )
}
export default Card
