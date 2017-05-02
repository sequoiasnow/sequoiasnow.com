import React from 'react'
import PropTypes from 'prop-types'
import Prism from 'prismjs'
import cn from 'classNames'
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
const Code = ({ children, language, flush = false }) => {
  // If there is language, use prism syntax highlighting
  if ( language || children.length > 40 ) {
    const lang = language || 'javascript' 
    const html = Prism.highlight(children, PrismLanguages[lang])
    return (
      <pre className={cn(`language-${lang}`, { 'code--flush': flush })}> 
        <code className={`language-${lang}`}
              dangerouslySetInnerHTML={{ __html: html }} />
      </pre>
    )
  }

  return (
    <code>{children}</code>
  )
}

Code.propTypes = {
  /**
   * A known prism language for which to highlight the following code.
   */
  language: PropTypes.string,
  /**
   * Make flush **with a card**, this will only be flush within an instance of 
   * [Card](/atoms/Card).
   */
  flush: PropTypes.bool,
  /**
   * The code to be highlighted.
   */
  children: PropTypes.string
}

export default Code
