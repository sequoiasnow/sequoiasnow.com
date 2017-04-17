import React from 'react'
import Button from './index'
import readme from './README.md'

/* --- Atoms --- */
import Card from '../Card'

/* --- Molecules --- */
import Markdown from '../../Molecules/Markdown'
import { colorPickerBound } from '../../Molecules/ColorPicker/example'

export const title = 'Button'


const Example = () => {
  return (
    <div>
      <Markdown content={readme} /> 
      <Button color="red" label="Hello World" />
    </div>
  ) 
}

export default Example
