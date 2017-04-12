import React from 'react'
import styles from './styles.scss'
import { renderMarkdown } from '../../utilities'

const Container = ({ preamble, children }) => {
  const html = renderMarkdown(preamble)
  
  return (
    <section className={styles.container}>
      <section className={styles.preamble} dangerouslySetInnerHTML={{__html: html}} />
      {children}
    </section>
  )
}
export default Container
