// src/Math.js
"use strict";

const propel = require("propel");

exports.square = function(semiring) { return function(t) {return function() { t.square(); }; }; };
exports.neg = function(ring) {return function(t) { return function() { t.neg(); }; }; };
exports.log = function(t) { return function() { t.log(); }; };
exports.exp = function(t) { return function() { t.exp(); }; };
exports.sinh = function(t) { return function() { t.sinh(); }; };
exports.flatten = function(t) { return function() { t.flatten(); }; };

//exports.reshapeImpl = function(shape,t) { return function() { t.reshape(shape); }; };
exports.reverseImpl = function(dims,t) { return function() { t.reverse(dims); }; };


exports.copyImpl = function(t) { return function() {return t.copy(); }; };