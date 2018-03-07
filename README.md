# purescript-propel

Purescript bindings to the Propel ML tensor library. Closely follows the API to be found here http://propelml.org/docs/.

If shapes don't make sense, will throw errors. Buyer beware. A type indexed library is a WIP.

npm install appropriate version of propel for your computer for tensor bindings. See propel documentation [here](https://github.com/propelml/propel). For example.

```npm install propel_linux_gpu```

Check out test/Main.purs for some example usage

This may be useful for dealing with Typed Array instances:
https://github.com/jutaro/purescript-typedarray

Lots of other possibly useful stuff not implemented (maps, filters, folds of typed array):
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

Propel plot is not working. I do not understand how it possibly could actually.

I have had difficulty browserifying. One can use the Propel cdn instead (see examples folder) or perhaps webpack.
