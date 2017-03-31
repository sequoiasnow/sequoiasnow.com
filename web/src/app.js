import React from 'react'
import { Provider } from 'react-redux'
import { Route
       , Router 
       , Match
       , IndexRoute
       , browserHistory } from 'react-router'
import { syncHistoryWithStore  } from 'react-router-redux'
import store from './configureStore'
import NotFound from './NotFound'
import Home from './Home'
import SiteContainer from './SiteContainer'
import ElementPage from './ElementPage'
import './app.scss'

/* Create the browser history. */
const history = syncHistoryWithStore(browserHistory, store)

const App = () => {
  return (
    <Provider store={store}>
      <div> 
        <Router history={history}>
          <Route path="/" component={SiteContainer}>
            <IndexRoute component={Home} />
            <Route path="elements" component={ElementPage} />
            <Route path="home" component={Home} />
            <Route path="**" component={NotFound} />
          </Route> 
        </Router>
      </div>
    </Provider>
  )
}

export default App
