import React     from 'react'
import PropTypes from 'prop-types'

/* --- Atoms --- */
import Heading   from '../../Atoms/Heading'
import Card      from '../../Atoms/Card'
import Button    from '../../Atoms/Button'

/* --- Molecules --- */
import MultiDimensional from './index'

export default class Example extends React.Component {
  constructor(props) {
    super(props)
    this.state = { selected: 'center' }
  }
  render() {
    const left = (
      <div>
        <Card>
          <Heading>Welcome to the Wild Blue yonder</Heading>
        </Card>
        <Card>
          <Button onClick={() => this.setState({selected: 'center' })}>Go Back</Button>
        </Card>
      </div>
    )

    const right = (
      <div>
        <Card>
          <Heading>Welcome to the Wild Red yonder</Heading>
        </Card>
        <Card>
          <Button onClick={() => this.setState({selected: 'center' })}>Go Back</Button>
        </Card>
      </div>
    )
    
    return (
      <MultiDimensional selected={this.state.selected}
                        left={left}
                        right={right}> 
        <Card>
          <Heading>Hello Center World</Heading>
          <p>This should always be the default view, at least for this site.</p>
          <Button onClick={() => this.setState({ selected: 'left' })}>Go Left</Button>
          <Button onClick={() => this.setState({ selected: 'right' })}>Go Right</Button>
        </Card>
      </MultiDimensional>
    )
  }
}

