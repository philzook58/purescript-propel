// src/Math.js
"use strict";


const propel = require("propel");
/*
try {
	var propel = require("propel-linux-gpu");
}
catch(err){
	try {

	var propel = require("propel-mac");
}
catch(err){
	console.log("Falling back to basic propel. Install appropriate propel for your system for better performance.");
	var propel = require("propel");
}
}
*/

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

exports.grad = propel.grad

exports.absImpl = function(t) { return t.abs(); };



exports.addImpl = function(semiring){ return function(x,y) { return x.add(y) }; };
exports.argminImpl = function(axis,t) { return t.argmin(axis); };
exports.argmaxImpl = function(axis,t) { return t.argmax(axis); };
exports.concatImpl = function(axis,t1,t2) { return t1.concat(axis, t2); };
exports.cosh = function(t) { return  t.cosh(); };
exports.dataSync = function(t) { return t.getData(); }; // This is not according to API

exports.asArray = function(t) {return Array.prototype.slice.call(exports.dataSync(t))};

exports.divImpl = function(divisionring){ return function(x,y) { return x.div(y) }; };

exports.equalImpl = function(t1, t2) { return t1.equal(t2); };
exports.exp = function(t) { return  t.exp(); };
exports.flatten = function(t) { return t.flatten(); };

exports.greaterImpl = function(t1, t2) { return t1.greater(t2); };
exports.greaterEqualImpl = function(t1, t2) { return t1.greaterEqual(t2); };
exports.lessImpl = function(t1, t2) { return t1.less(t2); };
exports.lessEqualImpl = function(t1, t2) { return t1.lessEqual(t2); };

exports.log = function(t) { return t.log(); };
exports.logSoftMax = function(t) { return t.logSoftMax(); };

exports.mulImpl = function(semiring){ return function(x,y) { return x.mul(y) }; };

exports.neg = function(ring) {return function(t) { return t.neg(); }; };

exports.onesLike = function(t) { return t.onesLike(); };


exports.relu = function(t) { return  t.relu(); };

exports.reshapeImpl = function(shape, t) { return t.reshape(shape) };

exports.reverseImpl = function(dims,t) { return function() { t.reverse(dims); }; };

exports.sinh = function(t) { return  t.sinh(); };
exports.sigmoid = function(t) { return  t.sigmoid(); };
exports.sign = function(t) { return  t.sign(); };

exports.square = function(semiring) { return function(t) {return t.square(); }; };

exports.squeeze = function(t) { return  t.squeeze(); };
exports.shape = function(t) { return t.shape; };

exports.tanh = function(t) { return  t.tanh(); };
exports.toString = function(t) { return t.toString(); };

exports.dtypeImpl = function(t) { return t.dtype; };
exports.rank = function(t) { return t.rank; };


exports.zerosLike = function(t) { return t.zerosLike() };




exports.sliceImpl = function(begin, end, t) { return t.slice(begin,end); };
exports.transpose = function(t) { return t.transpose(); };


exports.reduceSumImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceSum(axes,keepdims) }; };
exports.reduceMeanImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceMean(axes,keepdims) }; };
exports.reduceMaxImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceMax(axes,keepdims) }; };
exports.reduceLogSumExpImpl = function(semiring){ return function(axes, keepdims, t) { return t.reduceLogSumExp(axes,keepdims) }; };


exports.subImpl = function(ring){ return function(x,y) { return x.sub(y) }; };

exports.dotImpl = function(semiring){ return function(x,y) { return x.dot(y) }; };

exports.castImpl = function(dtype,t) { return function() { t.cast(dtype); }; };

