import React from 'react'
import { renderMarkdown } from '../utilities'
import styles from './styles.scss'

const PostExcerpt = ({ tags, title, body, date, url }) => {
  const content = renderMarkdown(body)
  return (
    <article className={styles.container}>
      <header>
        <div className={styles.titleContainer}>
          <span>{title}</span>
        </div>
      </header>
      <section className={styles.content} dangerouslySetInnerHTML={{__html: content}}/> 
      <footer> 
      </footer>
    </article>
  )
}
export default PostExcerpt
