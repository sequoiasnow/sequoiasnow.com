import React from 'react'
import styles from './styles.scss'
import { renderMarkdown } from '../utilities'

const Content = ({ markdown = '', raw = renderMarkdown(markdown) }) => {
  /* Update mathjax on the page. */
  if ( MathJax ) MathJax.Hub.Queue(["Typeset",MathJax.Hub])
  
  return (
    <section className={styles.content}
             dangerouslySetInnerHTML={{__html: raw}} />
  )
}
export default Content
