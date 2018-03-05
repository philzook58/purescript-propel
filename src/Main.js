
exports.plotlyImpl = function(divstr, x , y){ return function() { Plotly.plot( divstr, [{
	x: x,
	y: y}] );
};
};

// https://stackoverflow.com/questions/12760643/how-to-convert-a-javascript-typed-array-into-a-javascript-array
// var array =  Array.prototype.slice.call(floatarr);
