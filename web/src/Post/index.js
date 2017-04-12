import React from 'react'
import { connect } from 'react-redux'
import Page from '../Page'
import styles from './styles.scss'
import Tags from '../Tags'
import Title from '../Title'
import Content from '../Content'

const Post = ({ title = '', body = '', tags = [] }) => {
  return (
    <Page title={title}> 
      <article> 
        <header>
          <div className={styles.tagsContainer}>
            <Tags tags={tags} />
          </div>
          <Title title={title} /> 
        </header>
        <Content markdown={body} /> 
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
