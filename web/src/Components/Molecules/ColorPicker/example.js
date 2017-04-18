import React from 'react'

/* --- Molecules -- */
import ColorPicker from './index'

/**
 * Visible title of this Molecule.
 */
export const title = 'Color Picker'

import Card from '../../Atoms/Card'

/**
 * Bind's a color picker to a component, usefull in constructing examples 
 * than need color pickers.
 */
export class ColorPickerBound extends React.Component {
  constructor(props) {
    super(props)
    this.state = { color: '' }
  }

  render() {
    const { component } = this.props;
    
    return (
      <div>
        <Card>
          <ColorPicker onClick={(color) => this.setState({ color })} />
        </Card>
        {component(this.state.color)}
      </div>
    )
  }
}


const Example = () => {
  return <ColorPicker />
}
export default Example
