"use strict";

exports._init = function (o) {
    var regl = require('regl');
    return function() {
	return regl(o);
    };
};

exports.frame = function(c) {
    return function(cb) {
	return function() {
	    return c.frame(function(st){ cb(st)(); });
	};
    };
}

exports.clear = function(c) {
    return function() {
	return function(o) {
	    return function() {
		return c.clear(o);
	    };
	};
    };
}

exports._draw = function(e) {
    return function (c) {
	return function(o) {
	    o.uniforms = o.staticUniforms;
	    delete o.staticUniforms;
	    for (var k in o.dynamicUniforms) {
		o.uniforms[k] = c.prop(k);
	    }
	    delete o.dynamicUniforms;
	    
	    o.attributes = o.staticAttributes;
	    delete o.staticAttributes;
	    for (var k in o.dynamicAttributes)
		o.attributes[k] = c.prop(k);
	    delete o.dynamicAttributes;
	    var f = c(o);
	    return function () {
		return function(r) {
		    var x = {};
		    for (var k in r.uniforms)
			x[k] = r.uniforms[k];
		    for (var k in r.attributes)
			x[k] = r.attributes[k];
		    return function() {
			//			return f(e(r));
			return f(x);
		    };
		};
	    };
	};
    };
}

exports.buffer = function(c) {
    return function(a) {
	return function() {
	    return c.buffer(a);
	};
    };
}

exports._texture = function(c) {
    return function(so) {
	return function(bo) {
	    var o = {};
	    for (var k in so)
		o[k] = so[k];
	    for (var k in bo)
		o[k] = bo[k];
	    return function() {
		return c.texture(o);
	    };
	};
    };
}

exports._cube = function(c) {
    return function (so) {
	return function(co) {
	    var o = {};
	    for (var k in so)
		o[k] = so[k];
	    o.faces = [co.posX, co.negX, co.posY, co.negY, co.posZ, co.negZ];
	    return function() {
		return c.cube(o);
	    };
	};
    };
}

