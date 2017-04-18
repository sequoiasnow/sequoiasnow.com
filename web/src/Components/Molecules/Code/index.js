import React from 'react'
import Prism from 'prismjs'
import PrismLanguages from 'prism-languages'
import './styles.scss'

/**
 * @molecule Code
 * @description
 *   The code block contains all the code to create a beautiffuly syntax 
 *   highlughted area. It's actually quite fantastic, using the prismjs
 *   library also used to render markdown.
 * @modifiers
 *   language   The prism langauge highlighter to use.
 */
const Code = ({ children, language = false }) => {
  // If there is language, use prism syntax highlighting
  if ( language ) {
    const html = Prism.highlight(children, PrismLanguages[language])
    return (
      <pre>
        <code className={`language--${language}`}
              dangerouslySetInnerHTML={{ __html: html }} />
      </pre>
    )
  }

  return (
    <pre>
      <code>
        {children}
      </code>
    </pre>
  )
}
export default Code
