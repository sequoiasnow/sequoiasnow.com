import React from 'react'

/* --- Atoms --- */
import LightBox from './index'
import Button   from '../../Atoms/Button'
import Card     from '../../Atoms/Card'
import Heading  from '../../Atoms/Heading'

/**
 * An example lightbox component used with a button for effect.
 */
export default class Example extends React.Component {
  constructor(props) {
    super(props)
    this.state = { active: false }
  }
  render() {
    const { active } = this.state
    return (
      <div>
        <Card>
          <Button onClick={() => this.setState({ active: true })}>
            Open Lightbox
          </Button>
        </Card> 
        <LightBox onClose={() => this.setState({ active: false })} active={active}>
          <Card dark>
            <Heading>Hello World</Heading>
          </Card>
        </LightBox>
      </div>
    )
  }
}

