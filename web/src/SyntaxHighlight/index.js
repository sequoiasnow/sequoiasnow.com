import React from 'react'
import Prism from 'prismjs'
import PrismLanguages from 'prism-languages'
import styles from './styles.scss'

const SyntaxHighlight = ({ code, language }) => {
  var html = Prism.highlight(code, PrismLanguages[language] || Prism.languages.javascript)
    
  return (
    <div className={styles.syntaxHighlight}>
      <pre className={`language-${language}`}>
        <code className={`language-${language}`} dangerouslySetInnerHTML={{ __html: html }}> 
        </code>
      </pre>
    </div>
  )
}

export default SyntaxHighlight
