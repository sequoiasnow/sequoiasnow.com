import React from 'react'
import PeriodicTable from './periodic-table'
import { getNumbDigits } from '../utilities'

export const groupBlockColor = (group) => {
  switch (group) {
    case 'hydrogen':
      return '#a1aed6'
    case 'nonmetal':
      return '#a1afd6'
    case 'noble gas':
      return '#cda3ca'
    case 'metalloid':
      return '#aadfe7'
    case 'metal':
      return '#a9d59b'
    case 'actinoid':
      return '#e1aacc'
    case 'lanthanoid':
      return '#eca0c5'
    case 'alkaline earth metal':
      return '#fdd095'
    case 'alkali metal':
      return '#f7ac95'
    case 'custom':
      return 'tomato'
    default:
      return '#f8f5f0'
  }
}

/* Assembled from information given by the PeriodicTable library. */
export const RawElement = (data) => {
  return (
    <g className="atomic-element" x={data.x || 0} y={data.y || 0} transform={data.transform || ''}>
      <rect className="background" stroke="#000000" strokeOpacity="0.4" fill={groupBlockColor(data.groupBlock)} x="0" y="2" width="59" height="65"></rect>
      <text className="atomic-numb" fontFamily="Quicksand-Light, Quicksand" fontSize="16" fontWeight="300" fill="#000000">
        <tspan x={58 - 8 * getNumbDigits(data.atomicNumber)} y="16">{data.atomicNumber}</tspan>
      </text>
      <text className="chem-symbol" fontFamily="Quicksand-Light, Quicksand" fontSize="36" fontWeight="300" fill="#000000"  letterSpacing="-1.89999998">
        <tspan x="0" y="46">{data.symbol}</tspan>
      </text> 
      <text className="name" fontFamily="Quicksand-Light, Quicksand" fontSize="7" fontWeight="300" fill="#000000">
        <tspan x="2" y="59">{data.name}</tspan>
      </text>
      <text className="atomic-mass" fontFamily="Quicksand-Light, Quicksand" fontSize="9" fontWeight="300" fill="#000000">
        <tspan x="2" y="10">{(data.atomicMass + '').replace(/\(\d+\)/, '')}</tspan>
      </text>
      <text className="ionization-energy" fontFamily="Quicksand-Light, Quicksand" fontSize="5" fontWeight="300" fill="#000000">
        <tspan x="3" y="17">{data.ionizationEnergy}</tspan>
      </text>
      <text className="electron-conf" fontFamily="Quicksand-Light, Quicksand" fontSize="5" fontWeight="300" fill="#000000">
        <tspan x="2" y="65">{data.electronicConfiguration}</tspan> 
      </text>
      <text className="oxidation-states" fontFamily="Quicksand-Medium, Quicksand" fontSize="4" fontWeight="400" fill="#000000">
        {data.oxidationStates.toString().split(', ').map((n, i) => {
           return <tspan x="53" y={21 + i*5} key={i}></tspan>
         })}
      </text>
    </g>
  )
}

/* A plain background, server as a space. */
const SpaceElement = (data) => {
  return (
    <g className="atomic-element" {...data}>
      <rect className="background" stroke="white" fill="#f8f5f0" x="0" y="2" width="59" height="65"></rect>
    </g>
  )
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

const Element = ({ symbol = ' ', ...rest }) => {
  const symbols = PeriodicTable.symbols;
  const elms = {
    ' ': () => {
      return <SpaceElement {...rest} />
    },
    '?': () => {
      return <RawElement {...questionElemData} {...rest} /> 
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
export default Element

/* Allows computation to occur only once instead of many times. */
const symbolsArr = Object.keys(PeriodicTable.symbols)

/* Convert the string into a series of elements... pretty cool, huh? */
export const stringToSymbols = (str) => {
  const chars = str.replace(/[^a-zA-Z \?]/i,'').toUpperCase().split('')
  let elements = []

  for ( let i = 0; i < chars.length; i++ ) {
    const s = chars[i] 
    if (i < chars.length - 1 && symbolsArr.includes(s + chars[i + 1].toLowerCase()) ) {
      ++i // Skip the next iteration
      elements.push( s + chars[i].toLowerCase() )
    } else {
      elements.push( s ) 
    }
  }
  return elements
}
