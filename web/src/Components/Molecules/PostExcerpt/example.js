import React     from 'react'
import PropTypes from 'prop-types'

import PostExcerpt from './index'

/* --- Atoms --- */
import ScrollContainer from '../../Atoms/ScrollContainer'
import Card            from '../../Atoms/Card'

const posts = [
  {
    excerpt: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint',
    title: 'Gausses Law',
    category: 'math',
    tags: [
      { label: 'Gauss', category: 'math' },
      { label: 'Physics', category: 'physics' }
    ]
  },
  {
    excerpt: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud',
    title: 'Life on Mars?',
    category: 'physics',
    image: 'https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/OSIRIS_Mars_true_color.jpg/1200px-OSIRIS_Mars_true_color.jpg',
    tags: [
      { label: 'Mars', category: 'physics' },
      { label: 'Robots', category: 'physics' },
      { label: 'Stokes Theorem', category: 'math' },
      { label: 'Javascript', category: 'programming' }
    ]
  },
  {
    excerpt: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat',
    title: 'Haskell for Web Development',
    category: 'web',
    tags: [
      { label: 'Haskell', category: 'programming' },
      { label: 'Web Development', category: 'web' }
    ]
  }
]

const Example = () => (
  <div>
    <Card>
      <ScrollContainer>
        {posts.map((post, i) => (
           <PostExcerpt {...post} key={i}>{post.excerpt}</PostExcerpt>
         ))}
      </ScrollContainer>
    </Card>
    <Card>
      <ScrollContainer>
        {posts.map((post, i) => (
           <PostExcerpt {...post} key={i} collapsed>{post.excerpt}</PostExcerpt>
         ))}
      </ScrollContainer>
    </Card>
  </div>
)
export default Example
