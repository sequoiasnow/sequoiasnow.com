import React     from 'react'
import PropTypes from 'prop-types'

/* --- Local --- */
import ScrollDivider from './index'

/* --- Atoms --- */
import Card from '../../Atoms/Card'

export default () => (
  <Card>
    <div style={{ backgroundColor: 'red', width: '100%', height: '800px' }} />
    <ScrollDivider />
    <div style={{ backgroundColor: 'orange', width: '100%', height: '500px' }} />
    <ScrollDivider />
    <div style={{ backgroundColor: 'tomato', width: '100%', height: '700px' }} />
  </Card>
)

