import React from 'react'
import styles from './styles.scss'
import ExpanseSVG from './ExpanseSvg'

const Expanse = ({ onPyramidClick }) => {
  return (
    <section className={styles.expance}>
      <div className={styles.svgContainer}>
        <ExpanseSVG onPyramidClick={onPyramidClick} />
      </div>
    </section>
  )
}
export default Expanse
