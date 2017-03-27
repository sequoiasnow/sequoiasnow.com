/*******************************************************************************
* NAVIGATION
 *******************************************************************************/
import { push } from 'react-router-redux';

/* Wrapper for react-router redux */
export const navigateTo = push;

/*******************************************************************************
* POSTS
   - Simple rest setup for all posts.
 *******************************************************************************/
import Api from './Api'

/* Constants for the fetching process. */
export const POST_BEGIN_FETCH_ALL = 'POST_BEGIN_FETCH_ALL'
export const POST_FAIL_FETCH_ALL  = 'POST_FAIL_FETCH_ALL'
export const POST_DONE_FETCH_ALL  = 'POST_DONE_FETCH_ALL'

export const refreshAllPosts = () => {
  return (dispatch, getState) => {
    const { posts } = getState() 
    if ( (new Date()) - posts._lastFetch < 60 * 60 * 5 ) {
      /* Last update was less than five minutes ago... */
      return
    }
    
    dispatch({ type: POST_BEGIN_FETCH_ALL })
    return Api('/posts').then((data) => {
      dispatch({
        type: POST_DONE_FETCH_ALL,
        data
      })
    }).catch((error) => {
      console.warn('Error retrieving all posts, ' + error)
      dispatch({
        type: POST_FAIL_FETCH_ALL,
        error
      });
    })
  }
}
