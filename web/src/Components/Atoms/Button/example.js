import React from 'react'
import Button from './index'
import readme from './README.md'

/* --- Atoms --- */
import Card from '../Card'
import { allSizes } from '../Sizes'
import Heading from '../Heading'
import SidebarItem from '../SidebarItem'

/* --- Molecules --- */
import Markdown from '../../Molecules/Markdown'
import { StatefullColorPicker } from '../../Molecules/ColorPicker'
import Code from '../../Molecules/Code'
import { SimpleSelectionSizes } from '../../Molecules/SimpleSelection'
import Col from '../../Molecules/Col'

/* --- Grids --- */
import Grid from '../../Organisms/Grid'

export const title = 'Button'


const Example = () => {
  
  return (
    <div>
      <Markdown content={readme} />
      <Card>
        <Heading>Button Colors</Heading>
        <p>The button can come in a variety of colors as follows</p>
      </Card>
      <StatefullColorPicker renderComponent={(color) => {
          return (
            <div> 
              <Card>
                <Grid>
                  <Col width="4">
                    <Card><Button color={color}>Hello World</Button></Card>
                  </Col>
                  <Col width="4">
                    <Card dark><Button color={color}>Hello Dark!</Button></Card>
                  </Col>
                </Grid>
                <Code language="jsx">{`<Button color="${color}">Hello world</Button>`}</Code>
              </Card> 
            </div>
          )
        }} />
      <Card>
        <Heading>Button Sizes</Heading>
        <p>The button corresponds to the typical size constraints, as shown below.
          By default the size is medium</p>
      </Card>
      <SimpleSelectionSizes
          renderFunc={(size) => {
              return (
                <Card>
                  <Button size={size}>Hello Sizes!</Button> 
                  <Code language="jsx">
                    {`<Button size="${size}">Hello world</Button>`}
                  </Code>
                </Card>
              )
            }} />
    </div>
  ) 
}

export default Example
