import React from 'react'
import MarsSVG from './mars.svg'
import AsteroidSVG from './asteroid.svg'
import styles from './styles.scss'

const Mars = () => {
  return (
    <section className={styles.marsContainer}>
      <div className={styles.centerContainer}>
        <div className={styles.asteroidContainer}>
          <AsteroidSVG />
        </div>
        <div className={styles.planetContainer}>
          <MarsSVG />
        </div>
      </div>
    </section>
  )
};

export default Mars
