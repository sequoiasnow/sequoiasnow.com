import React from 'react'
import PeriodicTable from './periodic-table'
import { elementColors
       , RawElement
       , SpaceElement } from './RawElement'
import { getNumbDigits } from '../utilities'

/* Re-export Raw element items for conveniance */
export { RawElement, elementColors, SpaceElement }

export const randomColor = () => {
  const colors = Object.values(elementColors)
  return colors[Math.floor(Math.random() * colors.length)]
}

/* For conveniance, a group of props all filled out to question marks... */
const questionElemData = {
  groupBlock: "custom",
  atomicNumber: "?",
  symbol: "?",
  name: "???????",
  atomicMass: "????",
  ionizationEnergy: "??",
  electronicConfiguration: "[?] 1? 2?",
  oxidationStates: "+?, -?"
}

/* The SVG component of the element. This is identicial to the Element component
   without the wrapping svg code. */
export const ElementSVG = ({ symbol = ' ', ...rest }) => {
  const symbols = PeriodicTable.symbols;
  const elms = {
    ' ': () => {
      return <SpaceElement {...rest} />
    },
    '?': () => {
      return <RawElement {...questionElemData} {...rest} /> 
    },
    'Me': () => {
      return (
        <RawElement groupBlock="custom" 
                    atomicNumber={3}
                    symbol="Me"
                    name="Sequoia"
                    atomicMass="4.8x10²⁸"
                    ionizationEnergy="458"
                    electronicConfiguration="1s²"
                    oxidationStates="+1"
                    {...rest} />
      ) 
    },
    'default': () => {
      if ( Object.keys(symbols).includes(symbol) ) {
        return <RawElement {...symbols[symbol]} {...rest} />
      }
      const elemData = {
        ...questionElemData,
        symbol,
        name: symbol + '?????'
      }
      return <RawElement {...elemData} {...rest} />
    }
  }
  return (elms[symbol] || elms['default'])()
}

/* The ElementSVG wrapped in an svg block. */
const Element = (args) => {
  return (
    <svg viewBox="0 2 59 67"  version="1.1" xmlns="http://www.w3.org/2000/svg">
      <ElementSVG {...args} />
    </svg>
  )
}
export default Element

/* Allows computation to occur only once instead of many times. */
const symbolsArr = Object.keys(PeriodicTable.symbols)

/* Convert the string into a series of elements... pretty cool, huh? */
export const stringToSymbols = (str) => {
  const chars = str.replace(/[^a-zA-Z \?]/i,'').toUpperCase().split('')
  let elements = []

  for ( let i = 0; i < chars.length; i++ ) {
    const s = chars[i]
    /* Default check for array length. */
    if ( i >= chars.length - 1 ) {
      elements.push(s)
      break
    }

    /* Next Symbol */
    const ns = chars[i+1]
    if ( symbolsArr.includes(s + ns.toLowerCase()) ) {
      ++i // Skip the next iteration
      elements.push( s + chars[i].toLowerCase() )
    } else {
      elements.push( s ) 
    }
  }
  return elements
}


/* Returns a random symbols. */
export const randomSymbol = () => {
  return symbolsArr[Math.floor(Math.random() * symbolsArr.length)]
}

/* Convert the sequence of element's into groups of words */
export const groupSymbolsByWord = (symbols) => {
  let groups = []
  let subgroup = []
  symbols.forEach((symbol) => {
    if ( symbol == ' ' ) {
      groups.push(subgroup)
      subgroup = []
    } else {
      subgroup.push(symbol)
    }
  })
  groups.push(subgroup)
  return groups
}

/* The number of symbols till a space. */
const nTillSpace = (start, array) => {
  let i = start;
  while ( i < array.length && array[i] != ' ' ) {
    i++
  }
  return i - start
}

/* Convert the sequence of element's into groups based off of a width. */
export const groupSymbolsByWidth = (elems, width = 500, symbWidth = 61) => {
  let groups = []
  let subgroup = []
  let xOffset = 0
  for (let i = 0; i < elems.length; i++) {
    const e = elems[i]
    if ( ( e == ' '  && (xOffset + nTillSpace(i + 1, elems) * symbWidth) > width ) ||
         ( xOffset > width )) {
      groups.push(subgroup)
      subgroup = []
      xOffset = 0
    } else {
      subgroup.push(e)
    }
    xOffset += symbWidth
  }
  groups.push(subgroup)
  return groups
}
