
/******** GENERATED Tue Apr 18 2017 19:24:32 GMT-0700 (PDT) ********/
import React from 'react'

/* --- Molecules --- */
import Documentation from '../Documentation'

/* --- Local Imports --- */
import rawFile from '!raw-loader!./index.js'

/**
 * The title of the current note
 */
export const title = 'Sidebar'

/**
 * The example to be shown.
 */
const Example = () => {
    return <Documentation name="Sidebar" raw={rawFile} />
}
export default Example