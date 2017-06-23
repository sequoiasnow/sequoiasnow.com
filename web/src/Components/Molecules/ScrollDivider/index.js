import React     from 'react'
import PropTypes from 'prop-types'

/* --- Atoms --- */
import Divider from '../../Atoms/Divider'

/**
 * Ties the divider to the window scroll.
 */
export default class ScrollDivider extends React.Component {
  constructor(props) {
    super(props)
    this.onScroll = this.onScroll.bind(this)
    this.state = { value: 0 } 
  }

  componentDidMount() {
    window.addEventListener('scroll', this.onScroll)
  }

  componentWillUnmount() {
    window.removeEventListener('scroll', this.onScroll)
  }

  onScroll() {
    let value = window.scrollY / (document.body.clientHeight - window.innerHeight)
    if ( value > 1 ) value = 1
    if ( value < 0 ) value = 0
    this.setState({ value })
    console.log(value)
  }

  render() {
    return ( <Divider value={this.state.value} /> )
  } 
}
