import { createStore
       , applyMiddleware
       , combineReducers }  from 'redux'
import createSagaMiddleware from 'redux-saga'

/* --- Local --- */
import * as reducer from  './reducers'
import rootSaga     from  './sagas'

// create the saga middleware
const sagaMiddleware = createSagaMiddleware()
// mount it on the Store
const store = createStore(
  reducer,
  applyMiddleware(sagaMiddleware)
)

// then run the saga
sagaMiddleware.run(mySaga)

// Do something cool with the store
export default store
