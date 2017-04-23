import React from 'react'
import cn from 'classNames'

/* --- Atoms --- */
import ColorWell     from '../../Atoms/ColorWell'
import Card          from '../../Atoms/Card'
import { allColors } from '../../Atoms/Colors'

import styles from './styles.scss'


export default class ColorPicker extends React.Component {
  constructor(props) {
    super(props)

    /* Only used when state is not passed explicitly */
    this.state = { selectedIndex: 0 }
  }
  
  
  render() {
    const { selectedColor: sc
          , onClick = false
          , fullWidth = false } = this.props
    
    const clickFunc = (color, index) => {
      if (onClick)
        onClick(color) 
      this.setState({ selectedIndex: index })
    }

    const className = cn('color-picker', {
      'color-picker--fullw': fullWidth
    })
    
    return (
      <div className={className}>
        {allColors.map((color, index) => {
           const selected = sc ? (sc == color) : (index == this.state.selectedIndex) 
           return <ColorWell color={color}
                             key={index}
                             onClick={() => clickFunc(color, index)}
                             selected={selected} />
         })}
      </div>
    )
  }
}


export class StatefullColorPicker extends React.Component {
  constructor(props) {
    super(props)

    this.state = { color: allColors[0] }
  }

  render() {
    const { renderComponent } = this.props

    return (
      <div>
        <Card>
          <ColorPicker onClick={(color) => this.setState({ color })} />
        </Card>
        {renderComponent(this.state.color)}
      </div>
    )
  }
}
