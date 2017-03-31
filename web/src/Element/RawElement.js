import React from 'react'
import { getNumbDigits } from '../utilities'

/* Convert's the group to a prearanged color block. The colors are taken from 
the table at 
   https://sciencenotes.org/wp-content/uploads/2015/07/PeriodicTableWorks.png */
export const elementColors = {
  'hydrogen': '#a1aed6',
  'nonmetal': '#a1afd6',
  'noble gas': '#cda3ca',
  'metalloid': '#aadfe7',
  'metal': '#a9d59b',
  'actinoid': '#e1aacc',
  'lanthanoid': '#eca0c5',
  'alkaline earth metal': '#fdd095',
  'alkali metal': '#f7ac95',
  'transition metal': '#f6f29f',
  'halogen': '#b09eca',
  'custom': '#ea6550',
  'default': '#f8f5f0'
}
const groupBlockColor = (group) => {
  return (elementColors[group] || elementColors['default'])
}

/* All the basic placeholders for the periodic table element. */
export const RawElement = (data) => {
  const { groupBlock
        , atomicNumber
        , symbol
        , name
        , atomicMass
        , ionizationEnergy
        , electronicConfiguration
        , oxidationStates
        , x = null
        , y = null
        , id = ''
        , transform = null
        , animationDelay = '0s'
        , animationDuration = '1000ms'
        , animate = [] } = data
  return (
    <g className="atomic-element" x={x} y={y} transform={transform} id={id}>
    {animate.map(({ type = 'translate', from: fromAnimate, to }, index) => {
      return (
        <animateTransform key={type + '-' + index}
                          begin={animationDelay}
                          attributeName="transform"
                          type={type}
                          from={fromAnimate}
                          to={to}
                          dur={animationDuration}
                          fill="freeze" />
      )
    })}
    <rect className="background" fill={groupBlockColor(groupBlock)} x="0" y="2" width="59" height="65"></rect>
    <path className="border" d="M0.5 2.5 L58.5 2.5 L58.5 66.5 L0.5 66.5 Z" stroke="#000000" strokeOpacity="0.4" fill="none"></path>
    <text className="atomic-numb" fontFamily="Quicksand-Light, Quicksand" fontSize="16" fontWeight="300" fill="#000000">
    <tspan x={55 - 7 * getNumbDigits(atomicNumber)} y="16">{data.atomicNumber}</tspan>
    </text>
    <text className="chem-symbol" fontFamily="Quicksand-Light, Quicksand" fontSize="36" fontWeight="300" fill="#000000"  letterSpacing="-1.89999998">
    <tspan x="0" y="46">{symbol}</tspan>
    </text> 
    <text className="name" fontFamily="Quicksand-Light, Quicksand" fontSize="7" fontWeight="300" fill="#000000">
    <tspan x="2" y="59">{name}</tspan>
    </text>
    <text className="atomic-mass" fontFamily="Quicksand-Light, Quicksand" fontSize="9" fontWeight="300" fill="#000000">
    <tspan x="2" y="10">{(atomicMass + '').replace(/\(\d+\)/, '').substr(0, 7)}</tspan>
    </text>
    <text className="ionization-energy" fontFamily="Quicksand-Light, Quicksand" fontSize="5" fontWeight="300" fill="#000000">
    <tspan x="3" y="17">{ionizationEnergy}</tspan>
    </text>
    <text className="electron-conf" fontFamily="Quicksand-Light, Quicksand" fontSize="5" fontWeight="300" fill="#000000">
    <tspan x="2" y="65">{electronicConfiguration}</tspan> 
    </text>
    <text className="oxidation-states" fontFamily="Quicksand-Medium, Quicksand" fontSize="4" fontWeight="400" fill="#000000">
    {oxidationStates.toString().split(', ').map((n, i) => {
      return <tspan x="53" y={21 + i*5} key={i}></tspan>
    })}
        </text>
      </g>
  )
      }

  /* A plain background, server as a space. */
export const SpaceElement = ({animate, animatonDuration, animationDelay, ...data}) => {
  return (
    <g className="atomic-element" {...data}>
      <rect className="background" stroke="white" fill="#f8f5f0" x="0" y="2" width="59" height="65"></rect>
    </g>
  )
}


