import React from 'react'
import Button from './index'
import allColors from '../Colors'
import allSizes from '../Sizes'
import Container from '../../../StyleGuide/Container'
import Section from '../../../StyleGuide/Section'
import readme from './README.md'

const Example = () => {
  const differentColors = allColors.map((c) => {
    return {
      label: 'Hello World',
      color: c
    }
  })
  const differntSizes = allSizes.map((size) => {
    return {
      label: 'Hello World', 
      size
    }
  }) 
  
  return (
    <Container preamble={readme}>
      <Section title="Example Colors" props={differentColors} component={Button} />
      <Section title="Example Sizes" props={differntSizes} component={Button} />
    </Container>
  ) 
}

export default Example
