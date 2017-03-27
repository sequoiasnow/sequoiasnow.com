import React from 'react'
import Page from '../Page'
import haikus from './haiku.json'
import { getRandom
       , sanitizeLineBreaks } from '../utilities'
import styles from './styles.scss'

const NotFound = () => {
  const randomHaiku = sanitizeLineBreaks(getRandom(haikus))
  
  return (
    <Page title="404. Page Not Found">
      <div className={styles.container}>
        <div className={styles.haikuContainer}>
          {randomHaiku}
        </div>
      </div>
    </Page>
  )
}

export default NotFound
