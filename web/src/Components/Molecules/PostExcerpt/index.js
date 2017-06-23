import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import './styles.scss'

/* --- Global --- */
import cn from 'classnames'
import { Link } from 'react-router'

/* --- Atoms --- */
import ScrollContainer   from '../../Atoms/ScrollContainer'
import Heading           from '../../Atoms/Heading'
import Tag               from '../../Atoms/Tag'
import Divider           from '../../Atoms/Divider'
import DottedLine        from '../../Atoms/DottedLine'

/**
 * By some accounts the post excerpt is actually quite nice to look at, by 
 * others it is said that it needs improvement. Currently it is a small card
 * with a sequance of heading, brief content overview, and then a collection
 * of tags. Eventually there could be featurs like likes, social media, and 
 * even authoer if I got creative.
 */
const PostExcerpt = ({ children, image, tags = [], title, category = 'none', collapsed = false, url = ''}) => {
  const c = cn('post-excerpt', `post-excerpt--${category}`, {
    'post-excerpt--has-image': image,
    'post-excerpt--collapsed': collapsed
  })

  if ( collapsed ) {
    return (
      <article className={c}>
        {image && <div className="post-excerpt__image"
                       style={{ backgroundImage: `url(${image})`}} />}
        <div className="post-excerpt__divider">
          <div className="post-excerpt__category">
            <Tag category={category} simple>{category.toUpperCase()}</Tag>
          </div>
          <DottedLine />
        </div>
        <div className="post-excerpt__container"> 
          <header> 
            <span className="post-excerpt__title">
              <Link to={url}>
                <Heading size="small">{title}</Heading>
              </Link>
            </span>
          </header> 
        </div> 
      </article>
    )
  }
  
  return (
    <article className={c}> 
      {image && <div className="post-excerpt__image"
                     style={{ backgroundImage: `url(${image})`}} />}
      {collapsed || <Divider />}
      <div className="post-excerpt__container"> 
        <header> 
          <span className="post-excerpt__title">
            <Link to={url}>
              <Heading size="small">{title}</Heading>
            </Link>
          </span>
        </header>
        <section className="post-excerpt__body"> 
          {children}
        </section>
      </div>
      <DottedLine /> 
      <footer>
        <ScrollContainer> 
          <div className="post-excerpt__tags">
            {tags.map((tag, i) => (
               <Tag key={i} category={tag.category} simple>{tag.label}</Tag>
             ))}
          </div>
        </ScrollContainer>
      </footer>
    </article>
  )
}

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
    PropTypes.shape({
      label: PropTypes.string,
      category: PropTypes.string
    })
  ),
  /**
   * The title of the post.
   */
  title: PropTypes.string.isRequired,
  /**
   * The category of the current post, this is a required field. Categories are 
   * Essential to the colorsheme of the site.
   */
  category: PropTypes.string,
  /**
   * If the collapsed attribute is specified a smaller version of the post excerpt
   * will be presented, suitible for sidebars.
   */
  collapsed: PropTypes.bool,
  /**
   * The url of the post, this is treated as raw, any modifications to this url should
   * occur in the posts reducer.
   */
  url: PropTypes.string
}

export default PostExcerpt
