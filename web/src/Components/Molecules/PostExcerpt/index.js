import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/* --- Global --- */
import cn from 'classnames'

/* --- Atoms --- */
import ScrollContainer   from '../../Atoms/ScrollContainer'
import Heading           from '../../Atoms/Heading'
import { allCategories } from '../../Atoms/Category'
import Tag               from '../../Atoms/Tag'


/**
 * By some accounts the post excerpt is actually quite nice to look at, by 
 * others it is said that it needs improvement. Currently it is a small card
 * with a sequance of heading, brief content overview, and then a collection
 * of tags. Eventually there could be featurs like likes, social media, and 
 * even authoer if I got creative.
 */
const PostExcerpt = ({ children, image, tags = [], title, category = 'none' }) => (
  <article className={cn('post-excerpt', `post-excerpt--${category}`)}>
    {image && <img className="post-excerpt__image" src={image} />}
    <header>
      <Heading>{title}</Heading> 
    </header>
    <section>
      {children}
    </section>
    <footer>
      <ScrollContainer>
        {tags.map((tag, i) => (
           <Tag key={i}>{tag}</Tag>
         ))}
      </ScrollContainer>
    </footer>
  </article> 
)

PostExcerpt.propTypes = {
  /**
   * Children is used to allow for various post excerpt content types such as 
   * content 
   */
  children: PropTypes.node,
  /**
   * An image is optional and will show up at the top of the card.
   */
  image: PropTypes.string,
  /**
   * The tags for the post, this is an array of tags that are associated. 
   * Clicking on one will link to the tag, as the tag component suggests.
   */
  tags: PropTypes.arrayOf(
    PropTypes.string
  ),
  /**
   * The title of the post.
   */
  title: PropTypes.string.isRequired,
  /**
   * The category of the current post, this is a required field. Categories are 
   * Essential to the colorsheme of the site.
   */
  category: PropTypes.oneOf(allCategories)
}

export default PostExcerpt
