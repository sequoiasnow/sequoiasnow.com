import React from 'react'

/* --- Atoms --- */
import Card from '../../Atoms/Card'

import MarkdownIt from 'markdown-it'
import MarkdownMathJax from 'markdown-it-mathjax'
import MarkdownPrism from 'markdown-it-prism'

/* The markdown renderer. */
const md = MarkdownIt().use(MarkdownMathJax())
                       .use(MarkdownPrism)

/**
 * Render's markdown into text. using markdown it
 */
export const renderMarkdown = (raw) => md.render(raw)

import styles from './styles.scss'

/**
 * @molecule
 * @description
 *  Render's a raw markdown string into html, does not hadnle JSX
 */
const Markdown = ({ content }) => {
  return (
    <Card>
      <div className={styles.markdown}
           dangerouslySetInnerHTML={{ __html: renderMarkdown(content) }} />
    </Card>
  )
}
export default Markdown
