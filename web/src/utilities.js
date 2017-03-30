import React from 'react';


/*******************************************************************************
* getRandom
- Returns a random element from an array or an object. If it uses an object it
   picks a random key.
 *******************************************************************************/
export const getRandom = (a) => {
  if ( Array.isArray(a) ) {
    return a[Math.floor(Math.random() * a.length)]
  } else if ( typeof a == 'object' ) {
    const key = getRandom(Object.keys(a))
    return a[key]
  }
  return a
}

/*******************************************************************************
* sanitizeLineBreaks
   - Changes line breaks to <br> tags, use only with react.
 *******************************************************************************/
export function sanitizeLineBreaks(a) {
  return a.split("\n").map((item, index) => {
    return (<span key={index}>{item}<br /></span>)
  })
}

/*******************************************************************************
* getNumbDigits
   - Get's the number of digits in a real number. Not a string, a number.
 *******************************************************************************/
export function getNumbDigits(n) {
  return n.toString().length
}

/*******************************************************************************
* times
   - Repeat's a function n times. It is curried, and similairly to map returns an 
   array.
 *******************************************************************************/
export function times(n, start = 0) {
  return (f) => {
    let results = []
    for (let i = start; i < (n + start); i++) {
      results.push(f(i))
    }
    return results
  }
}
