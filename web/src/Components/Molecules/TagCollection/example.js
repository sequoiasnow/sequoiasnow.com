import React from 'react'

/* --- Atoms --- */
import Card from '../../Atoms/Card'

import TagCollection from './index'

export default () => (
  <Card>
    <TagCollection tags={['Hello', 'Goodbye', 'Stop', 'Go', 'Haskell', 'Other', 'Test']} />
  </Card>
)
