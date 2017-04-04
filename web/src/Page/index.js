import React from 'react'
import { connect } from 'react-redux'
import { navigateTo } from '../actions'
import Element, { randomSymbol
                , randomColor } from '../Element'
import { times
       , hexToRgba } from '../utilities' 
import styles from './styles.scss'
import NavSVG from './NavSVG'
import ExpanseSVG from './ExpanseSVG'

const mapStateToProps = () => { return {} };
const mapDispatchToProps = (dispatch) => {
  return { navigateTo: (route) => dispatch(navigateTo(route)) }
}

@connect (mapStateToProps, mapDispatchToProps)
export default class Page extends React.Component {
  constructor(props) {
    super(props)
    this.state = { expanded: false } 
  }
  
  render() {
    const bgColor = hexToRgba(randomColor(), 0.1)
    const { navigateTo
          , title = 'Title Not Found Or something longer'
          , children } = this.props
    
    const onHomeClick = () => navigateTo('/')
    const onResumeClick = () => navigateTo('/resume')
    const onPyramidClick = () => { this.setState({ expanded: true }) } 
    return (
      <section className={styles.page +  (this.state.expanded ? ' ' + styles.isExpanded : '')}> 
        <section className={styles.expance}>
          <div className={styles.svgContainer}>
            <ExpanseSVG onPyramidClick={() => this.setState({ expanded: false })} />
          </div>
        </section>
        <section className={styles.wrapper}>
          <section className={styles.background} style={{backgroundColor: bgColor}}>
            {times(40)((n) => {
               const left = Math.random() * 100
               const top = Math.random() * 30 + n * 30
               const width = Math.random() * 300 + 100
               return (
                 <div className={styles.bgElement} style={{ left: left+'vw', top: top+'vw', width: width+'px'  }}>
                   <Element key={n} symbol={randomSymbol()} />
                 </div>
               )
             })}
          </section>
          <section className={styles.header}>
            <NavSVG onHomeClick={onHomeClick} onResumeClick={onResumeClick} onPyramidClick={onPyramidClick} />
          </section>
          <section className={styles.titleContainer}>
            
          </section>
          <section className={styles.content}>
            {children}
          </section>
        </section>
      </section>
    ) 
  }
}
