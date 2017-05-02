import React from 'react'
import { Router
       , Route
       , browserHistory } from 'react-router'
import loremIpsum from 'lorem-ipsum'

/* ----- README ----- */
import AtomsReadme     from '../../Atoms/README.md'
import MoleculesReadme from '../../Molecules/README.md'
import OrganismsReadme from '../../Organisms/README.md'
import TemplatesReadme from '../../Templates/README.md'
import PagesReadme     from '../README.md'

/* --- Molecules -- */
import Markdown      from '../../Molecules/Markdown'
import Documentation from '../../Molecules/Documentation'

/* --- Templates --- */
import StylePage from '../../Templates/StylePage'

/** 
 * Simple navigation temp
 */
const NotFoundPage = () => ( <h1>Page not found</h1> )

/**
 * Function to find component based on type and name, modifyinf the navgiation
 * and page links appropriatly.
 */
const createStylePage = (elements, readme = false) => ({ params }) => {
  const navLinks = [
    { href: '/atoms', label: 'Atoms', selected: params.type == 'atoms' },
    { href: '/molecules', label: 'Molecules', selected: params.type == 'molecules' },
    { href: '/organisms', label: 'Organisms', selected: params.type == 'organisms' },
    { href: '/templates', label: 'Templates', selected: params.type == 'templates' }
  ]

  const pageLinks = elements[params.type].map(({ name }) =>  {
    return {
      label: name,
      href: `/${params.type}/${name}`, 
      selected: name.toLowerCase() == (params.name || '').toLowerCase()
    }
  })
  
  const relevant = elements[params.type].find(({ name }) =>
    name.toLowerCase() == (params.name || '').toLowerCase()
  ) 

  if ( ! relevant ) {
    if ( readme ) {
      const rm = ({ atoms: AtomsReadme
                  , molecules: MoleculesReadme
                  , organisms: OrganismsReadme
                  , templates: TemplatesReadme })[params.type]
      return (
        <StylePage pageLinks={pageLinks} navLinks={navLinks}>
          <Markdown content={rm} />
        </StylePage>
      )
    } 
    return <NotFoundPage />
  }

  /* Eventually we could just use the component as a defualt instance. */
  const Example = relevant.example || relevant.instance || (() => null)
  
  return (
    <StylePage pageLinks={pageLinks} navLinks={navLinks}>
      <div>
        <Documentation raw={relevant.module} name={relevant.name} file={relevant.file} />
        {relevant.readme && <Markdown content={relevant.readme} />}
        <Example />
      </div>
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

