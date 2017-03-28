import React from 'react'
import Element, { stringToSymbols } from '../Element'
import styles from './styles.scss'

export default class ElementPage extends React.Component {
  constructor(props) {
    super(props)
    this.state = { letters: 'Type Something' }
  }

  render() {
    const elems = stringToSymbols(this.state.letters)
    let elemGroups = ((elems) => {
      const nTillSpace = (start, array) => {
        let i = start;
        while ( i < array.length && array[i] != ' ' ) {
          i++
        }
        return i - start
      }
      
      /* Split based off of spaces. */
      let groups = []
      let subgroup = []
      let xOffset = 0
      for (let i = 0; i < elems.length; i++) {
        const e = elems[i]
        if ( ( e == ' '  && (xOffset + nTillSpace(i + 1, elems) * 61) > 500 ) || ( xOffset > 500 )) {
          groups.push(subgroup)
          subgroup = []
          xOffset = 0
        } else {
          subgroup.push(e)
        }
        xOffset += 61
      }
      groups.push(subgroup)

      
      return groups
    })(elems)
    
    return (
      <div className={styles.page}>
        <div className={styles.inputContainer}>
          <input type="text" value={this.state.letters} onChange={(e) => { this.setState({ letters: e.target.value }) }} />
        </div>
        <div className={styles.lettersContainer}>
          <svg viewBox="0 0 600 100" overflow="visible" version="1.1" xmlns="http://www.w3.org/2000/svg"> 
            {elemGroups.map((group, index) => {
               return (
                 <g key={group+'-'+index} transform={`translate(0, ${index * 68})`}>
                   {group.map((s, i) => {
                      return <Element symbol={s} key={group+'-'+i} transform={`translate(${61 * i}, 0)`}/>
                    })}
                 </g>
               )
             })}
          </svg>
        </div>
      </div>
    )
  }
}
