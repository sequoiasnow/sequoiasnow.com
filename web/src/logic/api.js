/* Uses the plugin for isomorphic fetch. This bypasses a Cross Origin Request
   Sharing Setup, that is very scantily implemnetd in haskell. Let this be a
   warning to those who would rely on its early ideas of success.  */

/* The assumed api location. CHANGE THIS TO AN ENV VARIABLE.. */
const API_URL = 'http://localhost:3000'

const Api = (args) => {
  /* Default values  */
  let request = {
    method: 'GET'
  }
  
  let url 
  /* If a string is the only argument it is assumed that it is a get reuquest to that
     address */
  if ( typeof args == 'string' ) {
    url = args
  } else {
    const { body = null, method = 'GET', url } = args
    request.method = method

    /* If there is a body, provide the appropriate headers... */
    if ( body ) {
      request.headers = newHeaders({ 'Content-Type': 'application/json' })
      request.body = JSON.stringify(body)
    } 
  }
  
  return fetch(API_URL + url, request).then((resp) => {
    return resp.json()
  })
}
export default Api
