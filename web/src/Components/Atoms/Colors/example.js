import React from 'react'

/* --- Local --- */
import readme    from './README.md'
import rawStyles from '!raw-loader!./styles.scss'

/* --- Atoms --- */
import Heading from '../Heading'
import Card    from '../Card'

/* --- Molecules --- */
import { StatefullColorPicker } from '../../Molecules/ColorPicker'
import Markdown                 from '../../Molecules/Markdown'
import Code                     from '../../Molecules/Code'

/** 
 * The name of the atom.
 */
export const title = "Colors"

/**
 * Simple example's with all the colors.
 */
export default function() {
  return (
    <div>
      <Markdown content={readme} />
      <Card>
        <Heading>Color Picker</Heading>
        <p>The color picker provides a good overview of the variety of colors available.</p>
        <StatefullColorPicker renderComponent={(color) => {
            return ( <p>Currently the color is <Code>$color--{color}</Code></p> )
          }} />
      </Card>
      <Card>
        <Heading>Full SCSS Color Pallete</Heading>
        <Code language="scss" flush>{rawStyles}</Code>
      </Card>
    </div>
  )
}
