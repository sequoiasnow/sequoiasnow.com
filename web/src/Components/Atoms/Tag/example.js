import Tag from './index'
import React from 'react'

/* --- Atoms --- */
import Card from '../Card'

export default function() {
  return (
    <Card>
      <Tag category="physics">Other Things</Tag>
      <Tag category="math">Web</Tag>
      <Tag category="programming">Haskell</Tag>
      <Tag category="web">Programming</Tag>
      <Tag>Style Guide</Tag>
    </Card>
  )
}

