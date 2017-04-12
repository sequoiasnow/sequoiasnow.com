import React from 'react'
import Element, { groupSymbolsByWord
                , stringToSymbols } from '../Element'
import styles from './styles.scss'

const Title = ({ title }) => {
  const titleSymbs = groupSymbolsByWord(stringToSymbols(title), true)
  return (
    <div className={styles.titleContainer}>
      {titleSymbs.map((group, i) => {
         return (
           <div className={styles.titleWord} key={i}>
             {group.map((s, n) => {
                return <Element symbol={s} key={s+n} />
              })} 
           </div>
         )
       })}
    </div>
  )
}
export default Title
