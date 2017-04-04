import React from 'react'
import { connect } from 'react-redux'
import Page from '../Page'
import styles from './styles.scss'
import MarkdownIt from 'markdown-it'
import MarkdownMathJax from 'markdown-it-mathjax'
import MarkdownPrism from 'markdown-it-prism'

const md = MarkdownIt().use(MarkdownMathJax())
                       .use(MarkdownPrism)

const Post = ({ title = '', body = '', tags = [] }) => {
  const content = md.render(body)
  /* Update mathjax on the page. */
  MathJax.Hub.Queue(["Typeset",MathJax.Hub])
  return (
    <Page title={title}>
      <article> 
        <header>
          {tags.map((tag) => <span key={tag}>{tag}</span>)}
        </header>
        <section className={styles.content} dangerouslySetInnerHTML={{__html: content}} /> 
      </article> 
    </Page>
  )
}

export default connect(
  (state, ownProps) => {
    const pathname = ownProps.location.pathname.replace(/[\/]/i, '')
    const post = Object.values(state.posts.all).find((p) => {
      return p.url == pathname
    })
    return post || {}
  }
)(Post)
