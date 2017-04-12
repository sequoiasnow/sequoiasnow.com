import React from 'react'
import styles from './styles.scss'
import SyntaxHighlight from '../../SyntaxHighlight'
import reactElementToJSXString from 'react-element-to-jsx-string';

const Section = ({ title, props, component: Comp }) => {
  return (
    <article className={styles.section}>
      <header>
        {title}
      </header>
      {props.map((args, index) => {
         const c = <Comp {...args} />
         return (
         <div key={index}>
           <div className={styles.exampleContainer}> 
             {c}
           </div>
           <div className={styles.jsxContainer}>
             <SyntaxHighlight language="html" code={reactElementToJSXString(c, {
                 maxInlineAttributesLineLength: 60
               })} />
           </div>
         </div>
         )
       })}
    </article>
  )
}
export default Section
