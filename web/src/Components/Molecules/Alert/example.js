import React from 'react'

/* --- Atoms --- */
import Alert  from './index'
import Button from '../../Atoms/Button'
import Card   from '../../Atoms/Card'

export default class AlertExample extends React.Component {
  constructor(props) {
    super(props)
    this.state = { active: false }
  }
  render() {
    const options = [{ label: 'close', onClick: () => this.setState({ active: false }) }, { label: 'Other' }]
    return (
      <Card>
        <Button onClick={() => this.setState({ active: true })}>Alert!</Button>
        {this.state.active && <Alert options={options}>Hello World!</Alert>}
      </Card>
    )  
  }
}
