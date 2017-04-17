import React from 'react'
import { Router
       , Route
       , browserHistory } from 'react-router'
import loremIpsum from 'lorem-ipsum'

/* --- Templates --- */
import StylePage from '../../Templates/StylePage'

/** 
 * Convert's a title to a url friendly name.
 */
const makeUrlFriendly = (name) => name.toLowerCase().replace(' ', '-')

/**
 * Function to find component based on type and name, modifyinf the navgiation
 * and page links appropriatly.
 */
const createStylePage = (elements, index) => ({ params }) => {
  const navLinks = Object.keys(elements).map((type) =>  {
    return {
      label: type.charAt(0).toUpperCase() + type.slice(1),
      href: '/' + type,
      selected: type == params.type
    }
  })
  const pageLinks = elements[params.type].map(({ title }) =>  {
    const url = makeUrlFriendly(title)
    return {
      label: title,
      href: `/${params.type}/${url}`, 
      selected: url == params.name
    }
  })
  const child = elements[params.type].find(({ title }) => {
    return makeUrlFriendly(title) == params.name
  })
  const ChildComponent = child ? child.component : () => {}
                         
  return (
    <StylePage pageLinks={pageLinks} navLinks={navLinks}>
      {child && <ChildComponent />}
    </StylePage>
  )
}

const StyleGuide = (props) => {
  return (
    <Router history={browserHistory}>
      <Route path="/:type" component={createStylePage(props, true)} />
      <Route path="/:type/:name" component={createStylePage(props)} />
    </Router>
  ) 
}
export default StyleGuide 

