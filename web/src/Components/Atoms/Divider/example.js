import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import Divider from './index'

/* --- Atoms --- */
import Card from '../Card'


export default class Example extends React.Component {
  render() {
    return (
      <div>
        <Card>
          <Divider /> 
        </Card> 
        <Card>
          <Divider>
            <h1>Hello, I am Groot</h1>
            <p>
              You could then include some meaningfull dialog about
              what it meant to be groot, or something along those lines if you
              follow.
            </p>
          </Divider>
        </Card>
      </div> 
    )
  }
}
