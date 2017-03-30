/* Listeners that look for event's independently of react. Such as websockets
   or scroll event's and report them to the store. Each function takes dispatch
   as an argument. */


/*******************************************************************************
* BREAKPOINTS
 *******************************************************************************/
import { windowResize } from './actions'
export const onResize = (dispatch) => {
  window.onresize = () => {
    dispatch(windowResize(window.innerWidth))
  }
}
