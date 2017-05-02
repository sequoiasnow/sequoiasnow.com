import React from 'react'

/* --- Local --- */
import rawStyles from '!raw-loader!./styles.scss'

/* --- Atoms --- */
import Card    from './index'
import Heading from '../Heading'

/* --- Molecules --- */
import Code     from '../../Molecules/Code'


/**
 * The visible title of the Card Atom.
 */
export const title = 'Card'

/**
 * A simple example of the card.
 */
const Example = () => {
  return (
    <div>
      <Card dark>
        <Heading>Example Dark Card</Heading>
        <p>As you can see the background color changes and the text changes to a
          lighter color, aside from that there are few changes</p>
      </Card>
      <Card>
        <Heading>Card Styling Source</Heading>
        <Code language="scss" flush>{rawStyles}</Code>
      </Card>
    </div>
  )
}
export default Example
