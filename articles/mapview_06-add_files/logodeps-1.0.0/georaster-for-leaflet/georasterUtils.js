/*
function prepareArray(mins, maxs) {
  let out = [];
  for (let i = 0; i < mins.length; i++) {
    out.push([mins[i], maxs[i]]);
  }
  return out;
}


function combine(x, y) {
  let out = [];
  for (let i = 0; i < x.length; i++) {
    for (let j = 0; j < x.length; j++) {
      out.push([x[i], y[j]]);
    }
  }
  return out;
}

function wrapArrays(x, len) {
  let out = [];
  for (let h = len - 2; h >= 0; h--) {
    if (h === len - 2) {
      out.push(combine(x[h], x[h + 1]));
    } else {
      out.push(combine(x[h], out));
    }
  }
  return out;
}
*/


function prepareArray(x, y) {
  let out = [];
  for (let i = 0; i < x.length; i++) {
    out.push([x[i], y[i]]);
  }
  return out;
}

function combiner(x, y) {
    let out = new Array(x.length);
    let n = 0;
    for (let i = 0; i < x.length; i++) {
        for (let j = 0; j < y.length; j++) {
            out[n] = [x[i], y[j]];
            n = n + 1;
        }
    }
    return out;
}

function wrapArrays(x, len) {
    let out = [];
    for (let h = len - 2; h >= 0; h--) {
        if (h === len - 2) {
            out = combiner(x[h], x[h + 1]);
        } else {
            out = combiner(x[h], out);
        }
        for (let k = 0; k < out.length; k++) {
            out[k] = out[k].flat();
        }
    }
    return out;
}

function evalDomain(arr, arith) {
  var out = [];
  for (let i = 0; i < arr.length; i++) {
    values = arr[i];
    let res = evalMath(arith, values);
    if (!isNaN(res)) {
      out.push(res);
    }
  }
  return [Math.min(...out), Math.max(...out)];
}

/*
function evalMath(a, values) {
    return Function('values', 'with(Math) return ' + a)(values);
} */

const compiledExpressions = {}
function evalMath(rawExpression, values) {
  if (!(rawExpression in compiledExpressions)) {
    compiledExpressions[rawExpression] = safeCompile(rawExpression).evaluate;
  }
  return compiledExpressions[rawExpression]({values});
}

// helpers from https://stackoverflow.com/questions/5623838/rgb-to-hex-and-hex-to-rgb
function componentToHex(c) {
  var hex = c.toString(16);
  return hex.length == 1 ? '0' + hex : hex;
}

function rgbToHex(r, g, b) {
  return '#' +
    componentToHex(r) +
    componentToHex(g) +
    componentToHex(b);
}

// https://gist.github.com/fpillet/993002
function scaleValue(value, from, to) {
  if (isNaN(value)) return NaN;
	var scale = (to[1] - to[0]) / (from[1] - from[0]);
	var capped = Math.min(from[1], Math.max(from[0], value)) - from[0];
	return ~~(capped * scale + to[0]);
}

// https://stackoverflow.com/questions/35325767/map-an-array-of-arrays
function deepMap(input,callback) {
  return input.map(entry=>entry.map?deepMap(entry,callback):callback(entry));
}

function rescale(value, to_min, to_max, from_min, from_max) {
  if (value === undefined) {
    value = from_min;
  }
  return (value - from_min) / (from_max - from_min) * (to_max - to_min) + to_min;
}

function naExclude(n) { return !isNaN(n); }


const combinations = ([head, ...tail]) => tail.length > 0 ? [...tail.map(tailValue => [head, tailValue]), ...combinations(tail)] : [];

