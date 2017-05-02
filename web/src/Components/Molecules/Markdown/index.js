import React from 'react'

/* --- Atoms --- */
import Card from '../../Atoms/Card'

import MarkdownIt from 'markdown-it'
import MarkdownMathJax from 'markdown-it-mathjax'
import MarkdownPrism from 'markdown-it-prism'
import cn from 'classNames'

/* The markdown renderer. */
const md = MarkdownIt().use(MarkdownMathJax())
                       .use(MarkdownPrism)

/**
 * Render's markdown into text. using markdown it
 */
export const renderMarkdown = (raw) => md.render(raw)

import './styles.scss'

/**
 * @molecule
 * @description
 *   Render's a raw markdown string into html, does not hadnle JSX
 * @modifiers
 *   seperateContent, splits the content into multiple cards.
 */
const Markdown = ({ content, seperateContent = false }) => {
  const c = cn('markdown', { 'markdown--seperate': seperateContent })
  
  
  return (
    <div className={c}
         dangerouslySetInnerHTML={{ __html: renderMarkdown(content) }} /> 
  )
}
export default Markdown
