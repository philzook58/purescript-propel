// src/Math.js
"use strict";

const propel = require("propel");

exports._data = function (t) { // accepts a request
  return function (onError, onSuccess) { // and callbacks
  	console.log(t.dispose);
  	console.log(propel);
  	var promdata = t.data;
  	console.log(promdata);
  	promdata.onComplete(onSuccess);
  	//promdata.then(onSuccess).err(onError);
    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};