
exports.plotlyImpl = function(divstr, x , y){ return function() { Plotly.plot( divstr, [{
	x: x,
	y: y}] );
};
};


exports.heatmapImpl = function(divstr, z) { return function(){
	var data = [
  {
    z: z,
    type: 'heatmap'
  }];
  Plotly.newPlot(divstr, data);
}
}


exports.surfaceImpl = function(divstr, z) { return function(){
	var data = [
  {
    z: z,
    type: 'surface'
  }];
  Plotly.newPlot(divstr, data);
}
}

exports.zplotImpl = function(divstr, z, options) { return function(){
	var object2 = Object.assign({}, options);
	object2.z = z

	var data = [object2];
  Plotly.newPlot(divstr, data);
}
}



// https://stackoverflow.com/questions/12760643/how-to-convert-a-javascript-typed-array-into-a-javascript-array
// var array =  Array.prototype.slice.call(floatarr);

