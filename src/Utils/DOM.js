// module Utils.DOM

exports.waitLoaded = function(cb, eb) {
    if (window.document.readyState === "complete") {
        return cb({});
    } else {
        return window.addEventListener("load", cb);
    }
};

exports.onLoad = function(action) {
    return function() {
        window.addEventListener("load", function() {
            action();
        });
    };
};

exports.blur = function(el) {
    return function() {
        return el.blur();
    };
};

exports.focus = function(el) {
    return function() {
        return el.focus();
    };
};

exports.getBoundingClientRect = function(el) {
    return function() {
        return el.getBoundingClientRect();
    };
};

exports.offsetLeft = function(el) {
    return function() {
       return el.offsetLeft;
    };
};

exports.getTextWidth = function(text, font) {
    return function() {
        var canvas = document.createElement("canvas");
        var context = canvas.getContext("2d");
        context.font = font;
        return context.measureText(text).width;
    };
};

exports.elementEq = function(a) {
    return function(b) {
        return function() {
            return a == b;
        };
    };
};

exports.scrollTop = function(el) {
    return function() {
        return el.scrollTop;
    };
};

exports.scrollLeft = function(el) {
    return function() {
        return el.scrollLeft;
    };
};

exports.getOffsetClientRect = function(el) {
  return function() {
    var rect = el.getBoundingClientRect();
    return {
      top: rect.top + document.body.scrollTop,
      left: rect.left + document.body.scrollLeft,
      width: rect.width,
      height: rect.height
    }
  }
}
