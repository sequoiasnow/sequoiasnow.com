import React from 'react'
import styles from './styles.scss'

/** 
 * @atom
 * A simple presentational component to give a background to contenent.
 */
const Card = ({ children }) => {
  return (
    <div className={styles.card}>
      <div className={styles.cardContainer}>{children}</div>
    </div>
  )
}
export default Card
