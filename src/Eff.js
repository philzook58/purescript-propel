"use strict";

const propel = require("propel");

/*
exports.plot = function(ts) {  return function () { propel.plot(ts[0], ts[1]); console.log(ts); }; };

exports.imshow = function(t) {  return function () { propel.imshow(t); }; };
*/

exports.dispose = function(t) {  return function () { t.dispose(); }; };