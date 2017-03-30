import React from 'react'
import { stringToSymbols
       , groupSymbolsByWidth
       , ElementSVG } from '../Element'
import styles from './styles.scss'

export default class ElementPage extends React.Component {
  constructor(props) {
    super(props)
    console.log('props are...')
    console.log(this.props.location)
    this.state = { letters: 'Type Something' }
  }

  render() {
    const elems = stringToSymbols(this.state.letters)
    let elemGroups = groupSymbolsByWidth(elems)
    
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
                      return <ElementSVG symbol={s} key={group+'-'+i} transform={`translate(${61 * i}, 0)`}/>
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
