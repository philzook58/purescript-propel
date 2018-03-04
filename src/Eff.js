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
exports.plot = function(ts) {  return function () { propel.plot.apply(this, ts); }; };

exports.imshow = function(t) {  return function () { propel.imshow(t); }; };

exports.dispose = function(t) {  return function () { t.dispose(); }; };