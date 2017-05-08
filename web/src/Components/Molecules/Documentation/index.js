import React from 'react'
import { parse } from 'react-docgen'

/* --- Atoms --- */
import Card    from '../../Atoms/Card'
import Heading from '../../Atoms/Heading'

/* --- Molecules --- */
import Code     from '../Code'
import Markdown from '../Markdown'

/**
 * Get the description from the parsed content.
 */
const getDescription = (docs) => {
  const { description = '' } = docs
  const results = description.match(/@description\n([a-zA-Z\s0-9,\.\n]+)/i)
  if ( results )
    return results[1].replace(/[\s\n]+/i, ' ')
  return description
}

/**
 * Gets the markup form the parsed conent.
 */
const getMarkup = (docs) => {
  const { description = '' } = docs
  const results = description.match(/@markup\n([\d\D\n]+)/i)
}

/**
 * Get's the type's name from the type object.
 */
const getTypeName = ({ name, value }) => {
  const options = {
    'instanceOf': (value) => value,
    'enum': (value = []) =>
      Array.isArray(value) ? value.map((a) => a.value.replace(/'/g, '')).join(' | ') : 'custom', 
    'arrayOf': (value) => '[' + getTypeName(value) + ']', 
    'shape': (value) => {
      let arr =[]
      for (let key in value) {
        arr.push(`  ${key}: ${getTypeName(value[key])}`)
      }
      return "{ \n" + arr.join(", \n") + "\n}"
    }
  }
  return value ? (options[name] || (() => name))(value) : name
} 

/**
 * Generate a table from the raw data.
 */
const propsTable = (docs) => {
  let data = []
  const { props = [] } = docs
  for (let name in props) {
    const { type = { name: 'custom' } 
          , defaultValue = { value: '' }
          , description = '' } = docs.props[name]
    let typeName = getTypeName(type)
    data.push({ name, type: typeName, defaultValue: defaultValue.value, description })
  }
  
  return (
    <table>
      <thead>
        <tr><th>Name</th><th>Type</th><th>Default</th><th>Description</th></tr>
      </thead>
      <tbody>
        {data.map(({ name, type, defaultValue, description }, index) => {
           return (
             <tr key={index} >
               <td>{name}</td>
               <td>
                 {Array.isArray(type) ?
                  type.map((a, i) => <Code key={i}>{a}</Code>) : <Code>{type}</Code>}
               </td>
               <td>{defaultValue && <Code>{defaultValue}</Code>}</td>  
               <td><Markdown content={description} /></td>
             </tr>
           )
         })}
      </tbody>
    </table>
  )
}

/**
 * @molecule
 * @description
 *  The molecule parses the source file for a react component and 
 *  generates an entire host of documentation for it.
 */
const Documentation = ({ raw, name }) => {
  const noDoc = null 
  if ( ! raw.includes('react') ) {
    /* This file doesn't deal with react... */ 
    return noDoc
  }

  let docs
  try {
    docs = parse(raw)
  } catch (e) {
    /* Something wrong, maybe a lack of react definitions */
    return noDoc
  }

  const description = getDescription(docs)
  const props = propsTable(docs)
  
  return (
    <div>
      <Card>
        <Heading size='large'>{name}</Heading>
        <Markdown content={description} />
      </Card>
      <Card>
        {props}
      </Card>
    </div>
  )
}

Documentation.propTypes = {
  /**
   * The source file of a react component.
   */
  raw: React.PropTypes.string,
  /**
   * The name of the component to be rendered.
   */
  name: React.PropTypes.string,
}

export default Documentation
