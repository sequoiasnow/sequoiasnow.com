import React from 'react'

/* --- Atoms --- */
import Card from '../../Atoms/Card'
import Tag from '../../Atoms/Tag'

import TagCollection from './index'

export default () => (
  <Card>
    <TagCollection>
      <Tag category="physics">Heisenberg</Tag>
      <Tag category="math">Divergence</Tag>
      <Tag category="programming">Typescript</Tag>
      <Tag category="web">CSS Variables</Tag>
      <Tag category="physics">Gausses Law</Tag>
      <Tag category="math">Stokes Theorem</Tag>
    </TagCollection>
  </Card>
)
