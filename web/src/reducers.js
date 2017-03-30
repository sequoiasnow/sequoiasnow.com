/*******************************************************************************
 * NAVIGATION
 ******************************************************************************/
import { routerReducer } from 'react-router-redux'

/* Export directly, this is simply a conveniance segment. */
export const routing = routerReducer

/*******************************************************************************
 * POSTS
   - Simple rest setup for all posts.
 *******************************************************************************/
import { POST_BEGIN_FETCH_ALL
       , POST_FAIL_FETCH_ALL
       , POST_DONE_FETCH_ALL } from './actions'
const initialPostState = {
  all: [],
  cached: [],
  current: {},
  _fetching: false,
  _lastFetch: new Date(0) /* The year, was 1970... */
}

export const posts = (state = initialPostState, action) => {
  switch (action.type) {
    case POST_BEGIN_FETCH_ALL:
      return {
        ...state,
        _fetching: true
      }
    case POST_DONE_FETCH_ALL:
      return {
        ...state,
        all: action.data,
        _fetching: false,
        _lastFetch: new Date()
      }
    case POST_FAIL_FETCH_ALL:
      return {
        ...state,
        _fetching: false
      }
    default:
      return state
  }
}

/*******************************************************************************
* BREAKPOINTS
 *******************************************************************************/
import { responsiveStateReducer } from 'redux-responsive'
export const browser = responsiveStateReducer
