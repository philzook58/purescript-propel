# purescript-propel

Purescript bindings to the Propel ML tensor library. Closely follows the API to be found here http://propelml.org/docs/.
If shapes don't make sense, will throw errors. Buyer beware.
Check out test/Main.purs for some example usage

This may be useful for dealing with Typed Array instances:
https://github.com/jutaro/purescript-typedarray

asArray in particular.
Lots of other possibly useful stuff not implemented (maps, filters, folds of typed array):
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

Propel plot is not working. I do not understand how it possibly could actually.

There is a small Plotly shim for plotting.

Checkout index.html with
```pulp --psc-package serve```

