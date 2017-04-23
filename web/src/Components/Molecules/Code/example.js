
/******** GENERATED Tue Apr 18 2017 19:03:53 GMT-0700 (PDT) ********/
import React from 'react'

/* --- Molecules --- */
import Documentation from '../Documentation'

/* --- Local Imports --- */
import rawFile from '!raw-loader!./index.js'

/**
 * The title of the current note
 */
export const title = "Code"

/**
 * The example to be shown.
 */
const Example = () => {
    return <Documentation name="Code" raw={rawFile} />
}
export default Example
