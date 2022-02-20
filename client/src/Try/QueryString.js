"use strict";

exports.getQueryString = function() {
  return window.location.search;
};

exports.setQueryParameters = function(params) {
  var encodedParams = Object.keys(params).map(function(key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  window.location.search = '?' + encodedParams;
};

exports.getQueryParams = function () {
  var qs = (function(a) {
    if (a == "") return {};
    var b = {};
    for (var i = 0; i < a.length; ++i)
    {
        var p=a[i].split('=', 2);
        if (p.length == 1)
            b[p[0]] = "";
        else
            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
    }
    return b;
})(window.location.search.substr(1).split('&'));
return qs;
}