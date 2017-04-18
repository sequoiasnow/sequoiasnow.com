import React from 'react'
import Button from './index'
import readme from './README.md'

/* --- Atoms --- */
import Card from '../Card'
import Heading from '../Heading'

/* --- Molecules --- */
import Markdown from '../../Molecules/Markdown'
import { ColorPickerBound } from '../../Molecules/ColorPicker/example'
import Code from '../../Molecules/Code'

export const title = 'Button'


const Example = () => {
  return (
    <div>
      <Markdown content={readme} />
      <Card>
        <Heading>Button Colors</Heading>
        <p>The button can come in a variety of colors as follows</p>
      </Card>
      <ColorPickerBound component={(color) => {
          return (
              <div> 
                <Card>
                  <Button color={color}>Hello World</Button> 
                  <Code language="jsx">{`<Button color="${color}">Hello world</Button>`}</Code>
                </Card>
                <Card dark><Button color={color}>Hello Dark!</Button></Card>
              </div>
            )
          }} />
    </div>
  ) 
}

export default Example
