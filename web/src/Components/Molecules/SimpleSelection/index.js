import React from 'react'

/* --- Atoms --- */
import SidebarItem from '../../Atoms/SidebarItem'
import Card from '../../Atoms/Card'

import './styles.scss'

/**
 * @molecule SimpleSelection 
 * @description
 *   The SimpleSelection looks similair to a sidebar, turned sideways, and 
 *   this is exactly the role it takes.
 */
const SimpleSelection = ({ children }) => {
  return (
    <Card>
      <div className="simple-selection">
        {children}
      </div>
    </Card>
  )
}
export default SimpleSelection

/* --- Atoms --- */
import { allSizes } from '../../Atoms/Sizes'

/**
 * Simple selection object with all sizes displayed.
 */
export class SimpleSelectionSizes extends React.Component {
  constructor(props) {
    super(props)
    this.state = { index: 0 }
  }
  render() {
    const { index } = this.state;
    console.log(this.state)
    return (
      <div> 
        <Card> 
          <SimpleSelection>
            {allSizes.map((size, i) => {
               return <SidebarItem key={i}
                                   selected={i == index}
                                   onClick={() => this.setState({ index: i })}>{size}</SidebarItem>
             })}
          </SimpleSelection> 
        </Card>
        {this.props.renderFunc(allSizes[index])}
      </div>
    )
  }
}
