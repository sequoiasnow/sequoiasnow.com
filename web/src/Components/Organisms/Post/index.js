import React from 'react'
import PropTypes from 'prop-types'

/* --- Atoms --- */
import Tag     from '../../Atoms/Tag'
import Heading from '../../Atoms/Heading'
import Card    from '../../Atoms/Card'

/**
 * A post, in its entirety which renders raw markdown, has a title and is 
 * generally brilliant.
 */
const Post = ({ children, title, tags = [] }) => (
  <article>
    <header>
      <Card>{title}</Card>
      <Card>{tags}</Card>
    </header>
    <section>
      {children}
    </section> 
  </article>
)

Post.propTypes = {
  /**
   * The post's own title, to be displayed as a large heading at the top.
   */
  title: PropTypes.string.isRequired,
  /**
   * The post's tags, already rendered.
   */
  tags: PropTypes.arrayOf(
    PropTypes.instanceOf(Tag)
  )
}

export default Post
