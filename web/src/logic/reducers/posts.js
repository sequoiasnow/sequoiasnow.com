
/**
 * Determines if a post is newer or not than another post. It is 
 * akin to the fitness of one post to another, thus the fitness 
 * function.
 */
function fitness(post) {
  let f = 0
  if ( post._complete ) f += 1000
  if ( post._loadedDate ) f += (posts._loadedDate / Math.pow(10, 10))
  return f
}

/**
 * Takes an array of posts, and adds a _loadedDate to them.
 */
const addDate = (posts) => posts.map((p) => {...p, _loadedDate: new Date() })


/**
 * Handles received posts, mapping the new posts amoung the old 
 * posts. By definition a new post is incomplete, meaning it has 
 * only an excerpt, so they have less priority than existing full
 * posts. However, if they are newer they still overwrite.
 */
function postsReceived(state, posts) {
  let newState = state
  posts.forEach(({ id, ...post }) => {
    if ( newState[id] && fitness(post) > fitness(newState[id]) )
      newState = { ...newState, [id]: post }
  })
  return newState
}

/**
 * The posts reducer which automatially handles all incoming async requests.
 */
function posts(state = {}, action) {
  const options =  {
    'POSTS_RECIEVED': () => postsReceived(state, addDate(action.posts)),
    'POST_RECIEVED': () => postsReceived(state, addDate([{
      ...action.post,
      _complete: true
    }]))
  }
  return (options[action.type] || () => state)()
}
}
