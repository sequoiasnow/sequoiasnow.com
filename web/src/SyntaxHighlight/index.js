import React from 'react'
import Prism from 'prismjs'
import PrismLanguages from 'prism-languages'
import './styles.scss'

const SyntaxHighlight = ({ code, language }) => {
  var html = Prism.highlight(code, PrismLanguages[language] || Prism.languages.javascript)
    
  return (
    <pre className={`language-${language} syntax-highlight`}>
      <code className={`language-${language}`} dangerouslySetInnerHTML={{ __html: html }}> 
      </code>
    </pre>
  )
}

export default SyntaxHighlight
