import { createStore
       , applyMiddleware
       , combineReducers
       , compose } from 'redux'
import { responsiveStoreEnhancer } from 'redux-responsive'
import * as reducers from './reducers'
import * as listeners from './listeners'
import thunk from 'redux-thunk'
import createLogger from 'redux-logger'
import { browserHistory } from 'react-router'
import { routerMiddleware } from 'react-router-redux'

const loggerMiddleware = createLogger()
const store = createStore(
  combineReducers(reducers),
  compose(
    responsiveStoreEnhancer,
    applyMiddleware(thunk, routerMiddleware(browserHistory), loggerMiddleware)
  )
)

/* Handle Webpack weirdness */
if ( module.hot ) {
  module.hot.accept('./reducers', () => {
    const nextRootReducer = require('./reducers')
    store.replaceReducer(nextRootReducer)
  })
}

export default store
