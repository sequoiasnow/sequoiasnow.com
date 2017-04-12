import React from 'react'
import { connect } from 'react-redux'
import { elementColors
       , randomColor } from '../Element'
import { navigateTo } from '../actions'
import styles from './styles.scss'

const colors = Object.values(elementColors)

const Tags = ({ tags = [], navigateTo, nTags = 6, spaceBetween = 8 }) => {
  const tagHeight = (175 - (nTags - 1) * spaceBetween) / nTags 
  
  return (
    <svg className={styles.tagsContainer} viewBox="0 0 400 175" version="1.1" xmlns="http://www.w3.org/2000/svg">
      
      <g className={styles.pyramid} transform="translate(138.000000, 0.000000)" fillOpacity="0.5">
        <polygon className={[styles.triangle, styles.triangle1].join(' ')} fill="#E82549" points="125 0 250 175 0 175"></polygon>
        <path d="M141.428571,23 L250,175 L32.8571429,175 L141.428571,23 Z M207.609816,152.958282 C207.123926,157.640491 205.268712,159.937423 202,159.937423 C197.141104,159.937423 194.932515,154.460123 192.988957,145.360736 L189.852761,131.358282 C187.644172,121.37546 186.009816,114.838037 184.949693,111.70184 C182.961963,105.606135 180.002454,103 176.733742,103 C171.477301,103 168.120245,109.095706 167.943558,117.311656 L169.533742,117.311656 C169.931288,110.95092 173.465031,109.44908 176.026994,109.44908 C179.604908,109.44908 182.741104,113.292025 184.905521,124.953374 L166,165.679755 L174.348466,165.679755 L187.511656,135.819632 L191.310429,152.958282 C192.723926,159.053988 194.225767,162.808589 195.904294,164.222086 C197.626994,165.635583 199.393865,166.29816 201.24908,166.29816 C204.429448,166.29816 209.2,164.177914 209.2,152.958282 L207.609816,152.958282 Z" className={[styles.triangle, styles.triangle2].join(' ')} fill="#F17E61"></path>
        <path d="M160,49 L250,175 L70,175 L160,49 Z M207.609816,152.958282 C207.123926,157.640491 205.268712,159.937423 202,159.937423 C197.141104,159.937423 194.932515,154.460123 192.988957,145.360736 L189.852761,131.358282 C187.644172,121.37546 186.009816,114.838037 184.949693,111.70184 C182.961963,105.606135 180.002454,103 176.733742,103 C171.477301,103 168.120245,109.095706 167.943558,117.311656 L169.533742,117.311656 C169.931288,110.95092 173.465031,109.44908 176.026994,109.44908 C179.604908,109.44908 182.741104,113.292025 184.905521,124.953374 L166,165.679755 L174.348466,165.679755 L187.511656,135.819632 L191.310429,152.958282 C192.723926,159.053988 194.225767,162.808589 195.904294,164.222086 C197.626994,165.635583 199.393865,166.29816 201.24908,166.29816 C204.429448,166.29816 209.2,164.177914 209.2,152.958282 L207.609816,152.958282 Z" className={[styles.triangle, styles.triangle3].join(' ')} fill="#FEEDB3"></path>
        <path d="M180,77 L250,175 L110,175 L180,77 Z M207.609816,152.958282 C207.123926,157.640491 205.268712,159.937423 202,159.937423 C197.141104,159.937423 194.932515,154.460123 192.988957,145.360736 L189.852761,131.358282 C187.644172,121.37546 186.009816,114.838037 184.949693,111.70184 C182.961963,105.606135 180.002454,103 176.733742,103 C171.477301,103 168.120245,109.095706 167.943558,117.311656 L169.533742,117.311656 C169.931288,110.95092 173.465031,109.44908 176.026994,109.44908 C179.604908,109.44908 182.741104,113.292025 184.905521,124.953374 L166,165.679755 L174.348466,165.679755 L187.511656,135.819632 L191.310429,152.958282 C192.723926,159.053988 194.225767,162.808589 195.904294,164.222086 C197.626994,165.635583 199.393865,166.29816 201.24908,166.29816 C204.429448,166.29816 209.2,164.177914 209.2,152.958282 L207.609816,152.958282 Z" className={[styles.triangle, styles.triangle4].join(' ')} fill="#FFE99E"></path>
      </g>
      <g className={styles.tagGroup}>
        {tags.sort((a, b) => b.length - a.length).map((tag, i) => {
           const width = tag.length * 8 // based of the width of source code pro. 
           const yStart = 175 - i * spaceBetween - tagHeight * i - tagHeight
           const yEnd   = yStart + tagHeight
           const xEnd1  = 138 - spaceBetween + (175 - yStart) * 150 / 210
           const xEnd2  =  138 - spaceBetween + (175 - yEnd) * 150 / 210
           const xStart = xEnd2 - width - 24; 
           return (
             <g className={styles.tag} key={tag} onClick={() => navigateTo(`/tags/${tag}`)}>
               <polygon fillOpacity="0.5" fill={colors[i]}  points={`${xStart},${yStart} ${xEnd1},${yStart} ${xEnd2},${yEnd} ${xStart},${yEnd}`} />
               <text fontFamily="Source Code Pro" fontSize="14" fontWeight="normal" fill="#000000" transform={`translate(${xStart}, ${yStart})`}> 
                 <tspan x="12" y="16">{tag}</tspan>
               </text>
             </g>
           )
         })}
      </g>
    </svg>
  )
}

export default connect(
  () => { return {} },
  (dispatch) => {
    return {
      navigateTo: (where) => dispatch(navigateTo(where))
    }
  }
)(Tags)



const NavToSimple = ({ tag, navigateTo }) => {
  return (
    <span className={styles.mininalTag}
          style={{backgroundColor: randomColor() }}
          onClick={navigateTo(`/tags/${tag}`)}>{tag}</span>
  )
}

export const MinimalTag = connect(
  () => { return {} },
  (dispatch) => {
    return {
      navigateTo: (where) => dispatch(navigateTo(where))
    }
  }
)(NavToSimple)
