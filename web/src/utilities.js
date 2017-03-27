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
export const sanitizeLineBreaks = (a) => {
  return a.split("\n").map((item, index) => {
    return (<span key={index}>{item}<br /></span>)
  })
}
