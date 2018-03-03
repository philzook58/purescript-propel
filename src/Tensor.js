// src/Math.js
"use strict";

const propel = require("propel");

// this is not documented correctly. There is no tensor objct in propel
// or something isn't trnalsating between typecript right? Or I installed an old package?
exports.tensor = propel.T;
exports.tensor2 = propel.T;
//exports.tensor = function(a){ return function() {return propel.tensor(a)}};
//exports.tensor = function(a){ return propel.tensor(a); };
//console.log(propel);
exports.zeros = propel.zeros; // function(shape) { return propel.zeros(shape) } ;
exports.ones = propel.ones;
exports.eye = propel.eye;

exports.linspaceImpl = propel.linspace;
exports.rangeImpl = propel.range;
exports.fillImpl = propel.fill;
exports.randn = propel.randn;

exports.reshapeImpl = function(shape, t) { return t.reshape(shape) };

exports.toString = function(t) { return t.toString(); };
exports.shape = function(t) { return t.shape; };
exports.dtypeImpl = function(t) { return t.dtype; };
exports.rank = function(t) { return t.rank; };
exports.dataSync = function(t) { return t.dataSync(); };

exports.zerosLike = function(t) { return t.zerosLike() };
exports.onesLike = function(t) { return t.onesLike(); };

exports.transpose = function(t) { return t.transpose(); };

exports.reduceSumImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceSum(axes,keepdims) }; };
exports.reduceMeanImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceMean(axes,keepdims) }; };
exports.reduceMaxImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceMax(axes,keepdims) }; };
exports.reduceLogSumExpImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceLogSumExp(axes,keepdims) }; };


exports.addImpl = function(semiring){ return function(x,y) { return x.add(y) }; };
exports.mulImpl = function(semiring){ return function(x,y) { return x.mul(y) }; };
exports.dotImpl = function(semiring){ return function(x,y) { return x.dot(y) }; };

