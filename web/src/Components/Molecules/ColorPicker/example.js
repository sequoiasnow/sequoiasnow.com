import React from 'react'

/* --- Molecules -- */
import ColorPicker from './index'

/**
 * Visible title of this Molecule.
 */
export const title = 'Color Picker'


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
    return (
      <div> 
        <ColorPicker onClick={(color) => this.setState({ color })} selectedColor={this.state.color} />
        {component(this.state.color)}
      </div>
    )
  }
}


const Example = () => {
  return <ColorPicker />
}
export default Example
