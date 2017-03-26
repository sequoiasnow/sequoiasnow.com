import React from 'react'
import { createStore
       , combineReducers
       , applyMiddleware } from 'redux'
import { Provider } from 'react-redux'
import { Route
       , Match } from 'react-router'
import { createBrowserHistory } from 'history'
import { ConnectedRouter
       , routerReducer
       , routerMiddleware
       , push } from 'react-router-redux'
import Routes from './Routes'
import './app.scss'

/* Create the browser history. */
const history = createBrowserHistory()

/* Create the initial store */
const store = createStore(
  combineReducers({
    routing: routerReducer
  }),
  applyMiddleware(routerMiddleware(history))
)


const App = () => {
  return (
    <Routes />
  )
}

export default App
