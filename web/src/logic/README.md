# Logic 

Even though css modules are used thouroughly for all matters design, the 
issues of asyncronous data flow must still be addressed. To do this I've
decided to use `redux-saga`. I'll admit half the reason I've decided to 
do this is because of Star Wars, but that's beside the point. Contained
within this folder is all the code necessary to create the io logic for 
the app as well as the standard redux reducer's logic.

## Design choices

One of the primary design choices made was not to use constants, simply
writing the boilerplate for a name didn't seem worth the trouble, so 
plain strings are used. For example, in place of 

```javascript
import { TODO_ADD_ITEM } from './actions'

const todoReducer(state, { type, name }) => {
    if ( type == TODO_ADD_ITEM)
            return [...state, name]
    return state
}
```
One can simply write.

```javascript
const todoReducer(state, { type, name }) => {
    if ( type == 'TODO_ADD_ITEM')
            return [...state, name]
    return state
}
```

Not only does this save an import, but is is more imediatly clear what
the type is to someone who doesn't regularly use redux.
