import { call
       , put
       , fork
       , take
       , takeEvery
       , takeLatest } from 'redux-saga/effects'

/* --- Local --- */
import Api from './api'

/**
 * Performs the Api request to get the most recent posts, or returns
 * an error object, by which saga can tell if something has gone wrong.
 * By default a request to the api for posts will return n recent posts,
 * we can specify how many using the limit paramter
 * @param int limit
 */
function fetchRecentPostsApi(limit = 15) {
  return Api(`/posts?limit=${limit}`)
    .then((posts) => { posts })
    .catch((error) => { error })
}

/**
 * Performes an Api request to get a single post or return an error object if
 * something has gone wrong.
 * @param int id
 */
function fetchPostApi(id) {
  return Api(`/posts/${id}`)
    .then((post) => { post })
    .catch((error) => { error })
}

/**
 * Send's the reducer information that the posts are being sent or notfiys
 * of a failure.
 */
function* fetchRecentPosts() {
  const { posts, error } = yield call(fetchRecentPostsApi)
  if ( posts )
    yield put({ type: 'POSTS_RECIEVED', posts })
  else
    yield put({ type: 'POSTS_REQUEST_FAILED', error })
}

/**
 * Fetches a new post from the api. This function is used in corridnation with the fetchPost
 * listener.
 */
function* fetchPost(id) {
  const { post, error } = yield call(fetchPostApi, id)
  if ( post )
    yield put({ type: 'POST_RECIEVED', post })
  else
    yield put({ type: 'PPOST_REQUEST_FAILED', error })
}


/*******************************************************************************
* Watchers
   - Who knows what you have spoken to the darkness alone
 *******************************************************************************/
function* watchFetchPost() {
  while (true) {
    const { id } = yield take('POST_FETCH')
    yield fork(fetchPost, id)
  }
}

function* watchFetchPostRecent() {
  while (true) {
    yield take('POST_FETCH_RECENT')
    yield fork(fetchRecentPosts)
  }
}

/**
 * Combines all functions together into one root saga.
 */
export default function* root() {
  yield [
    fork(watchFetchPost),
    fork(watchFetchPostRecent)
  ]
}
