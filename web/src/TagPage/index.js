import React from 'react'
import { connect } from 'react-redux'
import Tags from '../Tags'
import Title from '../Title'
import Page from '../Page'
import PostExcerpt from '../PostExcerpt'
import styles from './styles.scss'

const TagPage = ({ tag = 'Haskell', posts = [] }) => {
  return (
    <Page title={`Tag: ${tag}`}>
      <article>
        <header>
          <div className={styles.tagContainer}>
            <Tags tags={[tag]} />
          </div>
        </header>
        <section>
          {posts.sort().map((post, index) => {
             return <PostExcerpt {...post} key={post.id} />
           })}
        </section>
      </article>
    </Page>
  )
}

export default connect(
  (state, ownProps) => {
    const { tag } = ownProps.params 
    return {
      posts: state.posts.excerpts.filter((post) => {
        return post.tags.includes(tag)
      })
    }
  }
)(TagPage)
