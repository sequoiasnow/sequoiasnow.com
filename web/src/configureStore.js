import { createStore
       , applyMiddleware
       , combineReducers } from 'redux'
import * as reducers from './reducers'
import thunk from 'redux-thunk'
import createLogger from 'redux-logger'
import { routerMiddleware } from 'react-router-redux'

const loggerMiddleware = createLogger()
const store = createStore(
  combineReducers(reducers),
  applyMiddleware(thunk, loggerMiddleware)
);

/* Handle Webpack weirdness */
if ( module.hot ) {
  module.hot.accept('./reducers', () => {
    const nextRootReducer = require('./reducers')
    store.replaceReducer(nextRootReducer)
  })
}

export default store
