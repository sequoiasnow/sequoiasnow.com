import React from 'react'

/* --- Atoms --- */
import Button  from '../Button'
import Card    from '../Card'
import Heading from '../Heading'

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
        <Button onClick={() => this.setState({ active: !active })}>
          Open Lightbox
        </Button>
        <LightBox>
          <Card dark>
            <Heading>Hello World</Heading>
          </Card>
        </LightBox>
      </div>
    )
  }
}

