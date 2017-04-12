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
import { POST_BEGIN_FETCH_EXCERPTS
       , POST_FAIL_FETCH_EXCERPTS
       , POST_DONE_FETCH_EXCERPTS
       , POST_BEGIN_FETCH_SINGLE
       , POST_FAIL_FETCH_SINGLE
       , POST_DONE_FETCH_SINGLE
       , POST_BEGIN_FETCH_TAGS
       , POST_FAIL_FETCH_TAGS
       , POST_DONE_FETCH_TAGS }  from './actions'
const initialPostState = {
  excerpts: [],
  all: {},
  _fetching: false,
  _error: null,
  _lastFetch: new Date(0) /* The year, was 1970... */
}

export const posts = (state = initialPostState, action) => {
  switch (action.type) {
    case POST_DONE_FETCH_TAGS:
      let excerpts = state.excerpts
      let data = action.data
      /* Update excerpts */
      data.forEach((post) => {
        const index = excerpts.find((e) => e.id == post.id)
        if ( index ) {
          excerpts[index] = post
        } else {
          excerpts.push(post)
        }
      })
      return {
        ...state,
        excerpts: excerpts,
        _fetching: false,
        _error: null,
        _lastFetch: new Date()
      }
    case POST_DONE_FETCH_EXCERPTS: 
      let allPosts = state.all
      action.data.forEach((post) => {
        if ( ! allPosts[post.id] ) {
          allPosts[post.id] = post
        }
      })
      
      return {
        ...state,
        excerpts: action.data,
        all: allPosts,
        _fetching: false,
        _error: null,
        _lastFetch: new Date()
      }
    case POST_DONE_FETCH_SINGLE: 
      return {
        ...state,
        all: {
          ...state.all,
          [action.data.id]: action.data 
        },
        _fetching: false,
        _error: null,
        _lastFetch: new Date()
      }
    case POST_BEGIN_FETCH_TAGS:
    case POST_BEGIN_FETCH_EXCERPTS:
    case POST_BEGIN_FETCH_SINGLE:
      return {
        ...state,
        _fetching: true
      }
    case POST_FAIL_FETCH_TAGS:
    case POST_FAIL_FETCH_EXCERPTS:
    case POST_FAIL_FETCH_SINGLE:
      return {
        ...state,
        _error: action.error,
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
