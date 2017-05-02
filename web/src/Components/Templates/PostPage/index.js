import React     from 'react'
import PropTypes from 'prop-types'

/* --- Atom --- */
import Card from '../../Atoms/Card'
import Heading from '../../Atoms/Card'

/* --- Molecules --- */
import Col from '../../Molecules/Col'

/* --- Organisms --- */
import Navigation from '../../Organisms/Navigation'
import Grid       from '../../Organisms/Grid'

/**
 * The post page creates a wraper around the navigation to showcase a 
 * specific post, with given tags, title, raw markdown content and 
 * maybe even a background image. What's somewhat cool about this is that 
 * the post page uses the controlling naritive of the multidimensional 
 * view to switch between visuals.
 */
const PostPage = ({ title, content, tags = [], backgroundImage }) => (
  <div>
    <Navigation />
    <Grid>
      <Col width={2}>
        
      </Col>
      <Col width={6}>
        
      </Col>
    </Grid>
  </div>
)

export default PostPage
