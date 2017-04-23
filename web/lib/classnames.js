/**
 * The classnames function should be used simply with babel. It provides 
 * the same api as the `classNames` module, without the case change, which
 * is anoying. It is also more simply defined using modern javascript
 * polyfills
 */
function classNames(...args) {
    /* Recursive until only one argument is left. */
    if (args.length > 1) {
        return classNames(args[0]) + ' ' + classNames(args.slice(1))
    }
    const arg = args[0]
    if ( Object.isArray(arg) ) {
        return arg.join(' ')
    } else if ( typeof arg == 'object' ) {
        let arr = []
        for (let k in arg)
            if ( arg[k] )
                arr.push(k)
        return arr.join(' ')
    } else {
        return arg
    } 

}
