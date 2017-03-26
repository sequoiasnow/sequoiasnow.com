import React from 'react';
import SyntaxHighlight from '../SyntaxHighlight';
import styles from './styles.scss';

const c = `main = print "Hi, I'm Sequoia, Who are you?" >> \n    getLine >>=  print . (++)  "Nice to meet you, "`

/* A little introduction to who I am and what I do. */
const Hello = () => {
  return (
    <div className={styles.syntaxContainer}>
      <SyntaxHighlight code={c} language="haskell" />
    </div>
  )
};

export default Hello;
