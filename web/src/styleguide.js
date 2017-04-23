import React from 'react'
import { render } from 'react-dom'
import { AppContainer } from 'react-hot-loader'

/* @see https://webpack.github.io/docs/context.html#require-context */
const requireAll = (context) => context.keys().map((file) => {
  return {
    file, 
    module: context(file), 
    name: file.match(/\.\/(.+)\/(?:example\.js|index\.js|README\.md)/)[1]
  }
  
})

/**
* Get's the componetns from the two folders provided.
 * 
 * @param comps    These must be static to allow dependency simplification.
 * @param examples 
 */
const getComponent = (comps, examples = [], readmes= []) => comps.map((comp) => {
  const { file, name } = comp
  const example = examples.find((example) => example.name == name)
  const readme  = readmes.find((r) => r.name == name)
  return {
    ...comp, 
    example: example ? example.component : false,
    readme: readme ? readme.module : false
  } 
})


const atomComps         = requireAll(require.context('!raw!./Components/Atoms',     true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/index\.js/))
const atomReadmes       = requireAll(require.context('./Components/Atoms',          true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/README\.md/))
const atomExamples      = requireAll(require.context('./Components/Atoms',          true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/example\.js/))
const moleculeComps     = requireAll(require.context('!raw!./Components/Molecules', true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/index\.js/))
const moleculeReadmes   = requireAll(require.context('./Components/Molecules',      true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/README\.md/))
const moleculeExamples  = requireAll(require.context('./Components/Molecules',      true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/example\.js/))
const organismsComps    = requireAll(require.context('!raw!./Components/Organisms', true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/index\.js/))
const OrganismReadmes   = requireAll(require.context('./Components/Organisms',      true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/README\.md/))
const organismsExamples = requireAll(require.context('./Components/Organisms',      true, /^\.\/([A-Z][a-z]+(?:[A-Z][a-z]*)?)\/example\.js/))

const atoms = getComponent(atomComps, atomExamples, atomReadmes)
const molecules = getComponent(moleculeComps, moleculeExamples, atomReadmes)
const organisms = getComponent(organismsComps, organismsExamples, atomReadmes)

/* Import files relevant to the contianer */
import StyleGuide from './Components/Pages/StyleGuide'

const app = (
  <AppContainer>
    <StyleGuide atoms={atoms} molecules={molecules} organisms={organisms} />
  </AppContainer>
)
render(app, document.querySelector('#app'))
