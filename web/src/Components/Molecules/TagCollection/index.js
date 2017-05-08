import React from 'react'
import PropTypes from 'prop-types'

/* --- Global --- */
import FaHashtag from 'react-icons/lib/fa/hashtag'

/* --- Local --- */
import './styles.scss'

/* --- Atoms --- */
import Tag from '../../Atoms/Tag'

/**
 * The amount of degrees a hashtag is rotated about.
 */
const degreeInc = 35

/**
 * The tag collection shows a variety of tags in a circular ring, made unique by its ability
 * to spread out when hovered over, or when moble. The markup for creating a TagCollection
 * can use children or an array of tags, if both are used the array of tags will be prioritized.
 * 
 * ```jsx
 * <TagCollection>
 *    <Tag>Hello</Tag>
 *    <Tag>Goodbye></Tag>
 *    <Tag>You Say Stop</Tag>
 *    <Tag>I say go, go go</Tag>
 * </TagCollection>
 * // Or
 * <TagCollection tags={['Hello', 'Goodbye', 'You Say Stop', 'I say go, go go']} />
 * ```
 */
export default class TagCollection extends React.Component {
  static propTypes = {
    /**
     * An array of tags to be converted into a tag, these could be either simple
     * strings or react components.
     */
    tags: PropTypes.oneOfType([
      PropTypes.arrayOf(PropTypes.string),
      PropTypes.arrayOf(PropTypes.element) 
    ]) 
  }
  
  constructor(props) {
    super(props)
    /* The currently selected tag */
    this.state = { selectedIndex: 0 }
  }
  render() {
    const degree = this.state.selectedIndex * 35;
    const { tags, children } = this.props 
    const tagComps = ! tags ? React.Children.map(children, (tag) => React.cloneElement(tag)): tags.map((tag, i) => (
      <Tag key={i}
           onClick={() => this.setState({ selectedIndex: i })}
           selected={this.state.selectedIndex == i}>{tag}</Tag> 
    ))
    return (
      <div className="hashtag-collection">
        <div className="hashtag-collection__circle">
          <FaHashtag />
        </div>
        <div className="hashtag-collection__container"  style={{transform: `rotate(${degree}deg)`}}> 
          {tagComps}
        </div>
      </div>
    )
  } 
}
