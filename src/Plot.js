// src/Math.js
"use strict";

const propel = require("propel");

exports.plot = function(ts) {  return function () { propel.plot.apply(this, ts); }; };

exports.imshow = function(t) {  return function () { propel.imshow(t); }; };

exports.dispose = function(t) {  return function () { t.dispose(); }; };