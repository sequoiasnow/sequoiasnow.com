import React     from 'react'
import PropTypes from 'prop-types'

/* --- Atoms --- */
import Button from '../../Atoms/Button'
import Heading from '../../Atoms/Heading'
import Toggle from '../../Atoms/Toggle'
import Card from '../../Atoms/Card'

/* --- Molecules --- */
import Overlay from './index'

export default class Example extends React.Component {
  constructor(props) {
    super(props)
    this.state = { active: false, transform: 'slide' } 
  } 

  render() {
    const { active, transform } = this.state
    console.log(transform)
    return (
      <Card>
        <Button onClick={() => this.setState({ active: !active })}>Toggle Overlay</Button>
        <br />
        <p>Change the type of transformation that occurs, notice the effect of doing
          this in transition.</p>
        <Toggle options={['slide', 'rotate']}
                selected={transform}
                onClick={(_, n) => this.setState({ transform: n })} />
        <Overlay visible={active} transform={transform}>
          <Heading>Hello World</Heading>
        </Overlay>
      </Card>
    )
  }
}
