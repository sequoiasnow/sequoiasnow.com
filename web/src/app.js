import React from 'react'
import { Provider } from 'react-redux'
import { Route
       , Router 
       , Match
       , IndexRoute
       , browserHistory } from 'react-router'
import { syncHistoryWithStore  } from 'react-router-redux'
import { getPostInfo
       , refreshAllExcerpts
       , getPostsWithTag } from './actions'
import store from './configureStore'
import NotFound from './NotFound'
import Home from './Home'
import SiteContainer from './SiteContainer'
import Post from './Post'
import TagPage from './TagPage'
import ElementPage from './ElementPage'
import './app.scss'

/* Create the browser history. */
const history = syncHistoryWithStore(browserHistory, store)

const onBlogEnter = ({ params }) => {
  store.dispatch(getPostInfo(params.name))
}

const onTagEnter = ({ params }) => {
  store.dispatch(getPostsWithTag(params.tag))
}

const App = () => {
  return (
    <Provider store={store}>
      <div> 
        <Router history={history}>
          <Route path="/" component={SiteContainer}>
            <IndexRoute component={Home} onEnter={() => store.dispatch(refreshAllExcerpts())}/>
            <Route path="elements" component={ElementPage} />
            <Route path="/:name" component={Post} onEnter={onBlogEnter} />
            <Route path="/tags/:tag" component={TagPage} onEnter={onTagEnter} />
            <Route path="/notfound" component={NotFound} />
          </Route> 
        </Router>
      </div>
    </Provider>
  )
}

export default App
