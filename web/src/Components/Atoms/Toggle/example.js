import React     from 'react'
import PropTypes from 'prop-types'

/* --- Atoms --- */
import Toggle from './index'
import Card from '../Card'

export default class Example extends React.Component {
  constructor(props) {
    super(props)
    this.state = { index: 0 }
  }

  render() {
    const { index } = this.state
    return (
      <Card>
        <Toggle options={['One', 'Two', 'Three']}
                selected={index} 
                onClick={(i) => this.setState({ index: i })} />
      </Card>
    )
  }
}
