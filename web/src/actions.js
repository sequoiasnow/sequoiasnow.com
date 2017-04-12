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

const postActionGenerator = ({ constants, api, proceed = () => true }) => (...args) => {
  return (dispatch, getState) => {
    const { posts } = getState()
    if ( ! proceed(posts, dispatch, ...args) ) {
      return
    }
    dispatch({ type: constants.begin })
    return api(posts, dispatch, ...args).then((data) => {
      console.log('The api posts have been received')
      console.log(data)
      console.log('---end---')
      dispatch({ type: constants.done, data })
      return data
    }).catch((error) => {
      console.warn('Error retrieving all posts, ' + error)
      dispatch({ type: constants.fail, error })
    })
  }
}

/* Constants for the fetching process. */
export const POST_BEGIN_FETCH_EXCERPTS = '[s]POST_BEGIN_FETCH_EXCERPTS'
export const POST_FAIL_FETCH_EXCERPTS  = '[f]POST_FAIL_FETCH_EXCERPTS'
export const POST_DONE_FETCH_EXCERPTS  = '[f]POST_DONE_FETCH_EXCERPTS'

export const refreshAllExcerpts = postActionGenerator({
  constants: {
    begin: POST_BEGIN_FETCH_EXCERPTS,
    fail: POST_FAIL_FETCH_EXCERPTS,
    done: POST_DONE_FETCH_EXCERPTS
  }, 
  api: () => Api('/posts'),
  proceed: (posts) => {
    /* The last update was more than twenty minutes ago. */
    return (new Date() - posts._lastFetch) > 1200000 ||
           (posts.excerpts.length < 1 && ! posts._error)
  }
})

export const POST_BEGIN_FETCH_SINGLE = '[s]POST_BEGIN_FETCH_SINGLE'
export const POST_FAIL_FETCH_SINGLE  = '[f]POST_FAIL_FETCH_SINGLE'
export const POST_DONE_FETCH_SINGLE  = '[f]POST_DONE_FETCH_SINGLE'

export const getPostInfo = postActionGenerator({
  constants: {
    begin: POST_BEGIN_FETCH_SINGLE,
    fail: POST_FAIL_FETCH_SINGLE,
    done: POST_DONE_FETCH_SINGLE
  },
  proceed: (posts, dispatch, url) => {
    if ( ! posts.excerpts.find((p) => { return p.url == url }) ) {
      dispatch(refreshAllExcerpts()).then((data) => {
        const p = data.find((p) => p.url == url)
        if ( p ) {
          return dispatch(getPostInfo(p.url))
        } 
        return dispatch(navigateTo('/notfound'))
      }) 
      return false
    }
    console.log('proceeding')
    return true
  },
  api: (posts, dispatch, url) => {
    /* Must exist, due to proceed function */
    const post = posts.excerpts.find((p) => { return p.url == url })
    const { id } = post 
    return Api(`/posts/${id}`)
  }
})

export const POST_BEGIN_FETCH_TAGS = '[s]POST_BEGIN_FETCH_TAGS'
export const POST_DONE_FETCH_TAGS  = '[f]POST_DONE_FETCH_TAGS'
export const POST_FAIL_FETCH_TAGS  = '[f]POST_FAIL_FETCH_TAGS'

export const getPostsWithTag = postActionGenerator({
  constants: {
    begin: POST_BEGIN_FETCH_TAGS,
    fail: POST_FAIL_FETCH_TAGS,
    done: POST_DONE_FETCH_TAGS
  },
  proceed: () => true,
  api: (posts, dispatch, tag) => {
    const encodedTag = encodeURIComponent(tag)
    return Api(`/posts?tag=${encodedTag}`)
  }
})
