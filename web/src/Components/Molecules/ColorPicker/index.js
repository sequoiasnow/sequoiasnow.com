import React from 'react'
import classNames from 'classNames/bind'
import ColorWell from '../../Atoms/ColorWell'
import { allColors } from '../../Atoms/Colors'
import styles from './styles.scss'

const cn = classNames.bind(styles)

export default class ColorPicker extends React.Component {
  constructor(props) {
    super(props)

    /* Only used when state is not passed explicitly */
    this.state = { selectedIndex: 0 }
  }
  
  
  render() {
    const { selectedColor
          , onClick = () => {}
          , fullWidth = false } = this.props
    const clickFunc = (color, index) => {
      if (selectedColor)
        onClick(color)
      else
        this.setState({ slectedIndex: index })
    }

    const className = cn('color-picker', {
      'color-picker--fullw': fullWidth
    })
    
    return (
      <div className={styles.colorPicker}>
        {allColors.map((color, index) => {
           return <ColorWell color={color}
                             key={index}
                             onClick={clickFunc}
                             selected={selectedColor ? selectedColor == color : index == this.state.selectedIndex} />
         })}
      </div>
    )
  }
}
