(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$MenuHidden = {$: 'MenuHidden'};
var $author$project$Main$NormalSplitsView = {$: 'NormalSplitsView'};
var $author$project$Main$Uninitialized = {$: 'Uninitialized'};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $xarvh$elm_gamepad$Gamepad$A = {$: 'A'};
var $author$project$Main$UIReset = {$: 'UIReset'};
var $author$project$Main$UISkip = {$: 'UISkip'};
var $author$project$Main$UISplit = {$: 'UISplit'};
var $xarvh$elm_gamepad$Gamepad$X = {$: 'X'};
var $xarvh$elm_gamepad$Gamepad$Y = {$: 'Y'};
var $author$project$Main$defaultGamepadMap = _List_fromArray(
	[
		_Utils_Tuple2($author$project$Main$UISplit, $xarvh$elm_gamepad$Gamepad$A),
		_Utils_Tuple2($author$project$Main$UISkip, $xarvh$elm_gamepad$Gamepad$X),
		_Utils_Tuple2($author$project$Main$UIReset, $xarvh$elm_gamepad$Gamepad$Y)
	]);
var $ohanhi$keyboard$Keyboard$ArrowLeft = {$: 'ArrowLeft'};
var $ohanhi$keyboard$Keyboard$ArrowRight = {$: 'ArrowRight'};
var $ohanhi$keyboard$Keyboard$Character = function (a) {
	return {$: 'Character', a: a};
};
var $ohanhi$keyboard$Keyboard$Escape = {$: 'Escape'};
var $ohanhi$keyboard$Keyboard$Spacebar = {$: 'Spacebar'};
var $author$project$Main$UIMenu = {$: 'UIMenu'};
var $author$project$Main$UIUnsplit = {$: 'UIUnsplit'};
var $author$project$Main$defaultKeyboardMap = _List_fromArray(
	[
		_Utils_Tuple2($author$project$Main$UISplit, $ohanhi$keyboard$Keyboard$Spacebar),
		_Utils_Tuple2($author$project$Main$UIUnsplit, $ohanhi$keyboard$Keyboard$ArrowLeft),
		_Utils_Tuple2($author$project$Main$UISkip, $ohanhi$keyboard$Keyboard$ArrowRight),
		_Utils_Tuple2(
		$author$project$Main$UIReset,
		$ohanhi$keyboard$Keyboard$Character('R')),
		_Utils_Tuple2($author$project$Main$UIMenu, $ohanhi$keyboard$Keyboard$Escape)
	]);
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Timer$Stopped = function (a) {
	return {$: 'Stopped', a: a};
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$Timer$epoch = $elm$time$Time$millisToPosix(0);
var $author$project$Timer$SingleCategory = function (a) {
	return {$: 'SingleCategory', a: a};
};
var $author$project$Timer$emptySegment = {average: $elm$core$Maybe$Nothing, entityID: $elm$core$Maybe$Nothing, gold: $elm$core$Maybe$Nothing, icon: $elm$core$Maybe$Nothing, name: '---', pb: $elm$core$Maybe$Nothing, worst: $elm$core$Maybe$Nothing};
var $author$project$Timer$emptySplitSet = {
	current: $elm$core$Maybe$Nothing,
	previous: _List_Nil,
	upcoming: _List_fromArray(
		[
			{endTime: $elm$core$Maybe$Nothing, segment: $author$project$Timer$emptySegment, segmentTime: $elm$core$Maybe$Nothing}
		])
};
var $author$project$Timer$noCategory = {entityID: $elm$core$Maybe$Nothing, name: '(No Category)', offset: 0};
var $author$project$Timer$noGame = {entityID: $elm$core$Maybe$Nothing, icon: $elm$core$Maybe$Nothing, name: '(No Game)', offset: 0};
var $author$project$Timer$noRun = {category: $author$project$Timer$noCategory, game: $author$project$Timer$noGame, runEnded: $elm$core$Maybe$Nothing, runStarted: $elm$core$Maybe$Nothing, splits: $author$project$Timer$emptySplitSet};
var $author$project$Timer$noSingleRun = $author$project$Timer$SingleCategory($author$project$Timer$noRun);
var $author$project$Timer$empty_ = {currentTime: $author$project$Timer$epoch, runEnded: $elm$core$Maybe$Nothing, runStarted: $elm$core$Maybe$Nothing, runTracker: $author$project$Timer$noSingleRun, subtitle: '', title: ''};
var $author$project$Timer$empty = $author$project$Timer$Stopped($author$project$Timer$empty_);
var $xarvh$elm_gamepad$Gamepad$Advanced$UserMappings = function (a) {
	return {$: 'UserMappings', a: a};
};
var $xarvh$elm_gamepad$Gamepad$Advanced$emptyUserMappings = $xarvh$elm_gamepad$Gamepad$Advanced$UserMappings(
	{byId: $elm$core$Dict$empty, byIndexAndId: $elm$core$Dict$empty});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$GamepadPort$loadMappings = _Platform_outgoingPort('loadMappings', $elm$json$Json$Encode$string);
var $author$project$GamepadPort$load = $author$project$GamepadPort$loadMappings('');
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Websocket$createWS = _Platform_outgoingPort(
	'createWS',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					$elm$json$Json$Encode$string(a),
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string)(b)
				]));
	});
var $author$project$Websocket$open = function (url) {
	return $author$project$Websocket$createWS(
		_Utils_Tuple2(url, _List_Nil));
};
var $author$project$Main$init = function (url) {
	return _Utils_Tuple2(
		{backupTimer: $elm$core$Maybe$Nothing, categoryList: _List_Nil, configStore: $elm$core$Dict$empty, editMulticategory: false, fullCategoryList: _List_Nil, gameList: _List_Nil, gamepadMap: $author$project$Main$defaultGamepadMap, gamepadState: $author$project$Main$Uninitialized, keyboardMap: $author$project$Main$defaultKeyboardMap, keyboardStatus: _List_Nil, maxSegmentsShown: 10, menu: $author$project$Main$MenuHidden, multiCategoryList: _List_Nil, socket: $elm$core$Maybe$Nothing, splitsMode: $author$project$Main$NormalSplitsView, timer: $author$project$Timer$empty, userMappings: $xarvh$elm_gamepad$Gamepad$Advanced$emptyUserMappings},
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					$author$project$Websocket$open(url),
					$author$project$GamepadPort$load
				])));
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$GamepadFrame = function (a) {
	return {$: 'GamepadFrame', a: a};
};
var $author$project$Main$GamepadMappingLoad = function (a) {
	return {$: 'GamepadMappingLoad', a: a};
};
var $author$project$Main$GamepadRemappingTool = function (a) {
	return {$: 'GamepadRemappingTool', a: a};
};
var $author$project$Main$KeyboardEvent = function (a) {
	return {$: 'KeyboardEvent', a: a};
};
var $author$project$Main$SocketNotOpened = function (a) {
	return {$: 'SocketNotOpened', a: a};
};
var $author$project$Main$SocketOpened = function (a) {
	return {$: 'SocketOpened', a: a};
};
var $author$project$Main$SocketReceived = function (a) {
	return {$: 'SocketReceived', a: a};
};
var $author$project$Main$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$map = _Platform_map;
var $xarvh$elm_gamepad$Gamepad$Advanced$OnGamepad = function (a) {
	return {$: 'OnGamepad', a: a};
};
var $xarvh$elm_gamepad$Gamepad$Advanced$onBlob = $xarvh$elm_gamepad$Gamepad$Advanced$OnGamepad;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$array = _Json_decodeArray;
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$GamepadPort$onBlob = _Platform_incomingPort(
	'onBlob',
	A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (_v1) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (_v2) {
							return $elm$json$Json$Decode$succeed(
								_Utils_Tuple3(_v0, _v1, _v2));
						},
						A2(
							$elm$json$Json$Decode$index,
							2,
							A2(
								$elm$json$Json$Decode$andThen,
								function (userMappings) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (languages) {
											return $elm$json$Json$Decode$succeed(
												{languages: languages, userMappings: userMappings});
										},
										A2(
											$elm$json$Json$Decode$field,
											'languages',
											$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
								},
								A2($elm$json$Json$Decode$field, 'userMappings', $elm$json$Json$Decode$string))));
				},
				A2(
					$elm$json$Json$Decode$index,
					1,
					A2(
						$elm$json$Json$Decode$andThen,
						function (timestamp) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (gamepads) {
									return $elm$json$Json$Decode$succeed(
										{gamepads: gamepads, timestamp: timestamp});
								},
								A2(
									$elm$json$Json$Decode$field,
									'gamepads',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (mapping) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (index) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (id) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (buttons) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (axes) {
																				return $elm$json$Json$Decode$succeed(
																					{axes: axes, buttons: buttons, id: id, index: index, mapping: mapping});
																			},
																			A2(
																				$elm$json$Json$Decode$field,
																				'axes',
																				$elm$json$Json$Decode$array($elm$json$Json$Decode$float)));
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'buttons',
																		$elm$json$Json$Decode$array(
																			A2(
																				$elm$json$Json$Decode$andThen,
																				function (_v0) {
																					return A2(
																						$elm$json$Json$Decode$andThen,
																						function (_v1) {
																							return $elm$json$Json$Decode$succeed(
																								_Utils_Tuple2(_v0, _v1));
																						},
																						A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
																				},
																				A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$bool)))));
															},
															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
													},
													A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int));
											},
											A2($elm$json$Json$Decode$field, 'mapping', $elm$json$Json$Decode$string)))));
						},
						A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$float))));
		},
		A2(
			$elm$json$Json$Decode$index,
			0,
			A2(
				$elm$json$Json$Decode$andThen,
				function (timestamp) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (gamepads) {
							return $elm$json$Json$Decode$succeed(
								{gamepads: gamepads, timestamp: timestamp});
						},
						A2(
							$elm$json$Json$Decode$field,
							'gamepads',
							$elm$json$Json$Decode$list(
								A2(
									$elm$json$Json$Decode$andThen,
									function (mapping) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (index) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (id) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (buttons) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (axes) {
																		return $elm$json$Json$Decode$succeed(
																			{axes: axes, buttons: buttons, id: id, index: index, mapping: mapping});
																	},
																	A2(
																		$elm$json$Json$Decode$field,
																		'axes',
																		$elm$json$Json$Decode$array($elm$json$Json$Decode$float)));
															},
															A2(
																$elm$json$Json$Decode$field,
																'buttons',
																$elm$json$Json$Decode$array(
																	A2(
																		$elm$json$Json$Decode$andThen,
																		function (_v0) {
																			return A2(
																				$elm$json$Json$Decode$andThen,
																				function (_v1) {
																					return $elm$json$Json$Decode$succeed(
																						_Utils_Tuple2(_v0, _v1));
																				},
																				A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$float));
																		},
																		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$bool)))));
													},
													A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
											},
											A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int));
									},
									A2($elm$json$Json$Decode$field, 'mapping', $elm$json$Json$Decode$string)))));
				},
				A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$float)))));
var $author$project$GamepadPort$onLoad = _Platform_incomingPort('onLoad', $elm$json$Json$Decode$string);
var $ohanhi$keyboard$Keyboard$Down = function (a) {
	return {$: 'Down', a: a};
};
var $ohanhi$keyboard$Keyboard$Up = function (a) {
	return {$: 'Up', a: a};
};
var $ohanhi$keyboard$Keyboard$RawKey = function (a) {
	return {$: 'RawKey', a: a};
};
var $ohanhi$keyboard$Keyboard$eventKeyDecoder = A2(
	$elm$json$Json$Decode$field,
	'key',
	A2($elm$json$Json$Decode$map, $ohanhi$keyboard$Keyboard$RawKey, $elm$json$Json$Decode$string));
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $ohanhi$keyboard$Keyboard$downs = function (toMsg) {
	return $elm$browser$Browser$Events$onKeyDown(
		A2($elm$json$Json$Decode$map, toMsg, $ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keyup');
var $ohanhi$keyboard$Keyboard$ups = function (toMsg) {
	return $elm$browser$Browser$Events$onKeyUp(
		A2($elm$json$Json$Decode$map, toMsg, $ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var $ohanhi$keyboard$Keyboard$subscriptions = $elm$core$Platform$Sub$batch(
	_List_fromArray(
		[
			$ohanhi$keyboard$Keyboard$downs($ohanhi$keyboard$Keyboard$Down),
			$ohanhi$keyboard$Keyboard$ups($ohanhi$keyboard$Keyboard$Up)
		]));
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Websocket$newFD = _Platform_incomingPort('newFD', $elm$json$Json$Decode$value);
var $author$project$Websocket$Socket = F2(
	function (url, fd) {
		return {fd: fd, url: url};
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Websocket$decodeSocket = $elm$json$Json$Decode$decodeValue(
	A3(
		$elm$json$Json$Decode$map2,
		$author$project$Websocket$Socket,
		A2($elm$json$Json$Decode$field, 'url', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'fd', $elm$json$Json$Decode$int)));
var $author$project$Websocket$processNewFD = F3(
	function (msgOpened, msgNotOpened, value) {
		var _v0 = $author$project$Websocket$decodeSocket(value);
		if (_v0.$ === 'Ok') {
			var socket = _v0.a;
			return msgOpened(socket);
		} else {
			var msg = _v0.a;
			return msgNotOpened(msg);
		}
	});
var $author$project$Websocket$recv = _Platform_incomingPort('recv', $elm$json$Json$Decode$value);
var $author$project$Websocket$subscriptions = F3(
	function (opened, notOpened, received) {
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					$author$project$Websocket$newFD(
					A2($author$project$Websocket$processNewFD, opened, notOpened)),
					$author$project$Websocket$recv(received)
				]));
	});
var $author$project$Main$subscriptions = function (model) {
	var _v0 = function ($) {
		return $.gamepadState;
	}(model);
	if (_v0.$ === 'RemappingGamepad') {
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A3($author$project$Websocket$subscriptions, $author$project$Main$SocketOpened, $author$project$Main$SocketNotOpened, $author$project$Main$SocketReceived),
					A2($elm$core$Platform$Sub$map, $author$project$Main$KeyboardEvent, $ohanhi$keyboard$Keyboard$subscriptions),
					A2($elm$time$Time$every, 4, $author$project$Main$Tick),
					$author$project$GamepadPort$onBlob(
					A2($elm$core$Basics$composeL, $author$project$Main$GamepadRemappingTool, $xarvh$elm_gamepad$Gamepad$Advanced$onBlob)),
					$author$project$GamepadPort$onLoad($author$project$Main$GamepadMappingLoad)
				]));
	} else {
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A3($author$project$Websocket$subscriptions, $author$project$Main$SocketOpened, $author$project$Main$SocketNotOpened, $author$project$Main$SocketReceived),
					A2($elm$core$Platform$Sub$map, $author$project$Main$KeyboardEvent, $ohanhi$keyboard$Keyboard$subscriptions),
					A2($elm$time$Time$every, 4, $author$project$Main$Tick),
					$author$project$GamepadPort$onBlob($author$project$Main$GamepadFrame),
					$author$project$GamepadPort$onLoad($author$project$Main$GamepadMappingLoad)
				]));
	}
};
var $author$project$Main$ActiveGamepad = function (a) {
	return {$: 'ActiveGamepad', a: a};
};
var $author$project$Main$CategoryList = {$: 'CategoryList'};
var $author$project$Main$Config = {$: 'Config'};
var $author$project$Main$EditSplitsView = {$: 'EditSplitsView'};
var $author$project$Main$GameList = {$: 'GameList'};
var $author$project$Main$MainMenu = {$: 'MainMenu'};
var $author$project$Main$MenuVisible = function (a) {
	return {$: 'MenuVisible', a: a};
};
var $author$project$Main$RemappingGamepad = function (a) {
	return {$: 'RemappingGamepad', a: a};
};
var $author$project$Main$ToggleMainMenu = {$: 'ToggleMainMenu'};
var $author$project$Timer$MultiCategory = function (a) {
	return {$: 'MultiCategory', a: a};
};
var $author$project$Main$addGame_ = function (s) {
	var _v0 = function ($) {
		return $.runTracker;
	}(s);
	if (_v0.$ === 'MultiCategory') {
		var rt = _v0.a;
		var newRun = _List_fromArray(
			[$author$project$Timer$noRun]);
		return _Utils_update(
			s,
			{
				runTracker: $author$project$Timer$MultiCategory(
					_Utils_update(
						rt,
						{
							upcoming: _Utils_ap(
								function ($) {
									return $.upcoming;
								}(rt),
								newRun)
						}))
			});
	} else {
		return s;
	}
};
var $author$project$Timer$Finished = function (a) {
	return {$: 'Finished', a: a};
};
var $author$project$Timer$Paused = function (a) {
	return {$: 'Paused', a: a};
};
var $author$project$Timer$Running = function (a) {
	return {$: 'Running', a: a};
};
var $author$project$Timer$mapT = F2(
	function (f, t) {
		switch (t.$) {
			case 'Stopped':
				var s = t.a;
				return $author$project$Timer$Stopped(
					f(s));
			case 'Paused':
				var s = t.a;
				return $author$project$Timer$Paused(
					f(s));
			case 'Finished':
				var s = t.a;
				return $author$project$Timer$Finished(
					f(s));
			default:
				var s = t.a;
				return $author$project$Timer$Running(
					f(s));
		}
	});
var $author$project$Main$addGame = $author$project$Timer$mapT($author$project$Main$addGame_);
var $author$project$Main$addSegment_ = function (run) {
	var s = function ($) {
		return $.splits;
	}(run);
	var newSegment = $author$project$Timer$emptySegment;
	var newSplit = _List_fromArray(
		[
			{
			endTime: $elm$core$Maybe$Nothing,
			segment: _Utils_update(
				newSegment,
				{name: '(New Segment)'}),
			segmentTime: $elm$core$Maybe$Nothing
		}
		]);
	return _Utils_update(
		run,
		{
			splits: _Utils_update(
				s,
				{
					upcoming: _Utils_ap(
						function ($) {
							return $.upcoming;
						}(s),
						newSplit)
				})
		});
};
var $author$project$Timer$mapR = F2(
	function (f, splits) {
		var _v0 = function ($) {
			return $.runTracker;
		}(splits);
		if (_v0.$ === 'SingleCategory') {
			var r = _v0.a;
			return _Utils_update(
				splits,
				{
					runTracker: $author$project$Timer$SingleCategory(
						f(r))
				});
		} else {
			var rs = _v0.a;
			var c = function ($) {
				return $.current;
			}(rs);
			if (c.$ === 'Nothing') {
				return splits;
			} else {
				var r = c.a;
				return _Utils_update(
					splits,
					{
						runTracker: $author$project$Timer$MultiCategory(
							_Utils_update(
								rs,
								{
									current: $elm$core$Maybe$Just(
										f(r))
								}))
					});
			}
		}
	});
var $author$project$Main$addSegment = $author$project$Timer$mapT(
	$author$project$Timer$mapR($author$project$Main$addSegment_));
var $xarvh$elm_gamepad$Gamepad$B = {$: 'B'};
var $author$project$Main$allMappableControls = _List_fromArray(
	[
		_Utils_Tuple2('Split', $xarvh$elm_gamepad$Gamepad$A),
		_Utils_Tuple2('Unsplit', $xarvh$elm_gamepad$Gamepad$B),
		_Utils_Tuple2('Skip Segment', $xarvh$elm_gamepad$Gamepad$X),
		_Utils_Tuple2('Reset Run', $xarvh$elm_gamepad$Gamepad$Y)
	]);
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Main$categoryListRequestJSON = function (game) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('Menu')),
				_Utils_Tuple2(
				'contents',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'tag',
							$elm$json$Json$Encode$string('MenuCategories')),
							_Utils_Tuple2(
							'contents',
							$elm$json$Json$Encode$int(game))
						])))
			]));
};
var $author$project$Main$closeSplitsRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('Menu')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('MenuCloseSplits'))
					])))
		]));
var $author$project$Main$gameListRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('Menu')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('MenuGames'))
					])))
		]));
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$multiCategoryLoadRequest = function (category) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('Menu')),
				_Utils_Tuple2(
				'contents',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'tag',
							$elm$json$Json$Encode$string('MenuLoadMultiCategory')),
							_Utils_Tuple2(
							'contents',
							$elm$json$Json$Encode$int(category))
						])))
			]));
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$Timer$allSplits = function (set) {
	var _v0 = function ($) {
		return $.current;
	}(set);
	if (_v0.$ === 'Nothing') {
		return A2(
			$elm$core$List$append,
			function ($) {
				return $.previous;
			}(set),
			function ($) {
				return $.upcoming;
			}(set));
	} else {
		var s = _v0.a;
		return $elm$core$List$concat(
			_List_fromArray(
				[
					function ($) {
					return $.previous;
				}(set),
					_List_fromArray(
					[s]),
					function ($) {
					return $.upcoming;
				}(set)
				]));
	}
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Timer$segmentsJSON_ = function (t) {
	if (t.$ === 'Nothing') {
		return $elm$json$Json$Encode$null;
	} else {
		var i = t.a;
		return $elm$json$Json$Encode$int(
			$elm$time$Time$posixToMillis(i));
	}
};
var $author$project$Timer$segmentsJSON = F2(
	function (offset, segment) {
		var _v0 = function ($) {
			return $.segment;
		}(segment).entityID;
		if (_v0.$ === 'Nothing') {
			return $elm$json$Json$Encode$null;
		} else {
			var i = _v0.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'segment',
						$elm$json$Json$Encode$int(i)),
						_Utils_Tuple2(
						'time',
						$author$project$Timer$segmentsJSON_(
							function ($) {
								return $.segmentTime;
							}(segment)))
					]));
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Timer$runToJSON = function (run) {
	var segments = A2(
		$elm$core$List$filter,
		function (x) {
			return (!_Utils_eq(
				function ($) {
					return $.segment;
				}(x).entityID,
				$elm$core$Maybe$Nothing)) && (!_Utils_eq(
				function ($) {
					return $.segmentTime;
				}(x),
				$elm$core$Maybe$Nothing));
		},
		$author$project$Timer$allSplits(
			function ($) {
				return $.splits;
			}(run)));
	var category = function () {
		var _v0 = A2(
			$elm$core$Basics$composeL,
			function ($) {
				return $.entityID;
			},
			function ($) {
				return $.category;
			})(run);
		if (_v0.$ === 'Nothing') {
			return $elm$json$Json$Encode$null;
		} else {
			var c = _v0.a;
			return $elm$json$Json$Encode$int(c);
		}
	}();
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('SingleRun')),
				_Utils_Tuple2('runCategory', category),
				_Utils_Tuple2(
				'segments',
				A2(
					$elm$json$Json$Encode$list,
					$author$project$Timer$segmentsJSON(
						$elm$time$Time$posixToMillis(
							A2(
								$elm$core$Maybe$withDefault,
								$author$project$Timer$epoch,
								function ($) {
									return $.runStarted;
								}(run)))),
					segments)),
				_Utils_Tuple2(
				'startTime',
				$elm$json$Json$Encode$int(
					$elm$time$Time$posixToMillis(
						A2(
							$elm$core$Maybe$withDefault,
							$author$project$Timer$epoch,
							function ($) {
								return $.runStarted;
							}(run))))),
				_Utils_Tuple2(
				'endTime',
				$elm$json$Json$Encode$int(
					$elm$time$Time$posixToMillis(
						A2(
							$elm$core$Maybe$withDefault,
							$author$project$Timer$epoch,
							function ($) {
								return $.runEnded;
							}(run))))),
				_Utils_Tuple2(
				'realTime',
				$elm$json$Json$Encode$int(
					A2(
						$elm$core$Maybe$withDefault,
						0,
						A3(
							$elm$core$Maybe$map2,
							F2(
								function (t1, t2) {
									return $elm$time$Time$posixToMillis(t1) - $elm$time$Time$posixToMillis(t2);
								}),
							function ($) {
								return $.runEnded;
							}(run),
							function ($) {
								return $.runStarted;
							}(run)))))
			]));
};
var $author$project$Timer$splitsFor = function (timer) {
	switch (timer.$) {
		case 'Stopped':
			var s = timer.a;
			return s;
		case 'Paused':
			var s = timer.a;
			return s;
		case 'Finished':
			var s = timer.a;
			return s;
		default:
			var s = timer.a;
			return s;
	}
};
var $author$project$Timer$tupleJSON = F3(
	function (e1, e2, _v0) {
		var v1 = _v0.a;
		var v2 = _v0.b;
		return A2(
			$elm$json$Json$Encode$list,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					e1(v1),
					e2(v2)
				]));
	});
var $author$project$Timer$toJSON = function (timer) {
	var splits = $author$project$Timer$splitsFor(timer);
	var _v0 = function ($) {
		return $.runTracker;
	}(splits);
	if (_v0.$ === 'SingleCategory') {
		var run = _v0.a;
		return $author$project$Timer$runToJSON(run);
	} else {
		var runs = _v0.a;
		var parentID = function ($) {
			return $.entityID;
		}(runs);
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tag',
					$elm$json$Json$Encode$string('MultiSet')),
					_Utils_Tuple2(
					'parentCategory',
					$elm$json$Json$Encode$int(
						A2($elm$core$Maybe$withDefault, -1, parentID))),
					_Utils_Tuple2(
					'startTime',
					$elm$json$Json$Encode$int(
						$elm$time$Time$posixToMillis(
							A2(
								$elm$core$Maybe$withDefault,
								$author$project$Timer$epoch,
								function ($) {
									return $.runStarted;
								}(splits))))),
					_Utils_Tuple2(
					'endTime',
					$elm$json$Json$Encode$int(
						$elm$time$Time$posixToMillis(
							A2(
								$elm$core$Maybe$withDefault,
								$author$project$Timer$epoch,
								function ($) {
									return $.runEnded;
								}(splits))))),
					_Utils_Tuple2(
					'realTime',
					$elm$json$Json$Encode$int(
						A2(
							$elm$core$Maybe$withDefault,
							0,
							A3(
								$elm$core$Maybe$map2,
								F2(
									function (t1, t2) {
										return $elm$time$Time$posixToMillis(t1) - $elm$time$Time$posixToMillis(t2);
									}),
								function ($) {
									return $.runEnded;
								}(splits),
								function ($) {
									return $.runStarted;
								}(splits))))),
					_Utils_Tuple2(
					'runs',
					A2(
						$elm$json$Json$Encode$list,
						A2($author$project$Timer$tupleJSON, $elm$json$Json$Encode$bool, $author$project$Timer$runToJSON),
						_Utils_ap(
							A2(
								$elm$core$List$map,
								function (r) {
									return _Utils_Tuple2(true, r);
								},
								function ($) {
									return $.previous;
								}(runs)),
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2(
									$elm$core$Maybe$map,
									function (r) {
										return _List_fromArray(
											[
												_Utils_Tuple2(false, r)
											]);
									},
									function ($) {
										return $.current;
									}(runs))))))
				]));
	}
};
var $author$project$Main$resetSplitsRequestJSON = function (t) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('TimerControl')),
				_Utils_Tuple2(
				'contents',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'tag',
							$elm$json$Json$Encode$string('RemoteReset')),
							_Utils_Tuple2(
							'contents',
							$author$project$Timer$toJSON(t))
						])))
			]));
};
var $author$project$Websocket$sendWS = _Platform_outgoingPort(
	'sendWS',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'data',
					$elm$core$Basics$identity($.data)),
					_Utils_Tuple2(
					'socket',
					function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'fd',
									$elm$json$Json$Encode$int($.fd)),
									_Utils_Tuple2(
									'url',
									$elm$json$Json$Encode$string($.url))
								]));
					}($.socket))
				]));
	});
var $author$project$Websocket$send = F2(
	function (s, v) {
		return $author$project$Websocket$sendWS(
			{data: v, socket: s});
	});
var $author$project$Main$send = F2(
	function (ms, v) {
		if (ms.$ === 'Nothing') {
			return $elm$core$Platform$Cmd$none;
		} else {
			var s = ms.a;
			return A2($author$project$Websocket$send, s, v);
		}
	});
var $author$project$Main$skipSplitsRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('TimerControl')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('RemoteSkip'))
					])))
		]));
var $author$project$Main$splitsLoadRequestJSON = function (category) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('Menu')),
				_Utils_Tuple2(
				'contents',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'tag',
							$elm$json$Json$Encode$string('MenuLoadSplits')),
							_Utils_Tuple2(
							'contents',
							$elm$json$Json$Encode$int(category))
						])))
			]));
};
var $author$project$Main$startSplitsRequestJSON = function (time) {
	if (time.$ === 'Just') {
		var t = time.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tag',
					$elm$json$Json$Encode$string('TimerControl')),
					_Utils_Tuple2(
					'contents',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('RemoteStartSplit')),
								_Utils_Tuple2(
								'contents',
								$elm$json$Json$Encode$int(t))
							])))
				]));
	} else {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tag',
					$elm$json$Json$Encode$string('TimerControl')),
					_Utils_Tuple2(
					'contents',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'tag',
								$elm$json$Json$Encode$string('RemoteStartSplit')),
								_Utils_Tuple2('contents', $elm$json$Json$Encode$null)
							])))
				]));
	}
};
var $author$project$Main$stopSplitsRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('TimerControl')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('RemoteStop'))
					])))
		]));
var $author$project$Main$unsplitSplitsRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('TimerControl')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('RemoteUnsplit'))
					])))
		]));
var $author$project$Main$broadcastIntent = F2(
	function (ms, msg) {
		switch (msg.$) {
			case 'StartSplit':
				var time = msg.a;
				return A2(
					$author$project$Main$send,
					ms,
					$author$project$Main$startSplitsRequestJSON(
						A2($elm$core$Maybe$map, $elm$time$Time$posixToMillis, time)));
			case 'Unsplit':
				return A2($author$project$Main$send, ms, $author$project$Main$unsplitSplitsRequestJSON);
			case 'Skip':
				return A2($author$project$Main$send, ms, $author$project$Main$skipSplitsRequestJSON);
			case 'Stop':
				return A2($author$project$Main$send, ms, $author$project$Main$stopSplitsRequestJSON);
			case 'Reset':
				var timer = msg.a;
				return A2(
					$author$project$Main$send,
					ms,
					$author$project$Main$resetSplitsRequestJSON(timer));
			case 'CloseSplits':
				return A2($author$project$Main$send, ms, $author$project$Main$closeSplitsRequestJSON);
			case 'ListGames':
				return A2($author$project$Main$send, ms, $author$project$Main$gameListRequestJSON);
			case 'ListCategories':
				var g = msg.a;
				if (g.$ === 'Nothing') {
					return $elm$core$Platform$Cmd$none;
				} else {
					var g_ = g.a;
					return A2(
						$author$project$Main$send,
						ms,
						$author$project$Main$categoryListRequestJSON(g_));
				}
			case 'LoadSplits':
				var c = msg.a;
				if (c.$ === 'Nothing') {
					return $elm$core$Platform$Cmd$none;
				} else {
					var c_ = c.a;
					return A2(
						$author$project$Main$send,
						ms,
						$author$project$Main$splitsLoadRequestJSON(c_));
				}
			case 'LoadMultiCategory':
				var c = msg.a;
				return A2(
					$author$project$Main$send,
					ms,
					$author$project$Main$multiCategoryLoadRequest(c));
			default:
				return $elm$core$Platform$Cmd$none;
		}
	});
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Main$controlDescMap = _List_fromArray(
	[
		_Utils_Tuple2($author$project$Main$UISplit, 'Split'),
		_Utils_Tuple2($author$project$Main$UIUnsplit, 'Unsplit'),
		_Utils_Tuple2($author$project$Main$UISkip, 'Skip Segment'),
		_Utils_Tuple2($author$project$Main$UIReset, 'Reset Run'),
		_Utils_Tuple2($author$project$Main$UIMenu, 'Toggle Menu')
	]);
var $author$project$Timer$noMultiRun = $author$project$Timer$MultiCategory(
	{current: $elm$core$Maybe$Nothing, entityID: $elm$core$Maybe$Nothing, previous: _List_Nil, upcoming: _List_Nil});
var $author$project$Timer$emptyMulti = $author$project$Timer$Stopped(
	_Utils_update(
		$author$project$Timer$empty_,
		{runTracker: $author$project$Timer$noMultiRun}));
var $author$project$Main$Reset = function (a) {
	return {$: 'Reset', a: a};
};
var $author$project$Main$Skip = {$: 'Skip'};
var $author$project$Main$StartSplit = function (a) {
	return {$: 'StartSplit', a: a};
};
var $author$project$Main$Unsplit = {$: 'Unsplit'};
var $author$project$Util$const = F2(
	function (a, _v0) {
		return a;
	});
var $author$project$Main$fetchControl = function (u) {
	switch (u.$) {
		case 'UISplit':
			return A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $author$project$Main$StartSplit, $elm$core$Maybe$Just),
					function ($) {
						return $.currentTime;
					}),
				$author$project$Timer$splitsFor);
		case 'UIUnsplit':
			return $author$project$Util$const($author$project$Main$Unsplit);
		case 'UISkip':
			return $author$project$Util$const($author$project$Main$Skip);
		case 'UIReset':
			return $author$project$Main$Reset;
		default:
			return $author$project$Util$const($author$project$Main$ToggleMainMenu);
	}
};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $xarvh$elm_gamepad$Gamepad$Private$Axis = {$: 'Axis'};
var $xarvh$elm_gamepad$Gamepad$Private$Button = {$: 'Button'};
var $xarvh$elm_gamepad$Gamepad$Private$Origin = F3(
	function (a, b, c) {
		return {$: 'Origin', a: a, b: b, c: c};
	});
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $xarvh$elm_gamepad$Gamepad$Advanced$userMappingsDecoder = function () {
	var stringToOriginType = function (s) {
		switch (s) {
			case 'axis':
				return $elm$json$Json$Decode$succeed($xarvh$elm_gamepad$Gamepad$Private$Axis);
			case 'button':
				return $elm$json$Json$Decode$succeed($xarvh$elm_gamepad$Gamepad$Private$Button);
			default:
				return $elm$json$Json$Decode$fail('unrecognised Origin Type');
		}
	};
	var originDecoder = A4(
		$elm$json$Json$Decode$map3,
		$xarvh$elm_gamepad$Gamepad$Private$Origin,
		A2($elm$json$Json$Decode$field, 'isReverse', $elm$json$Json$Decode$bool),
		A2(
			$elm$json$Json$Decode$field,
			'type',
			A2($elm$json$Json$Decode$andThen, stringToOriginType, $elm$json$Json$Decode$string)),
		A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int));
	var listToUserMappings = function (listByIndexAndId) {
		return $xarvh$elm_gamepad$Gamepad$Advanced$UserMappings(
			{
				byId: $elm$core$Dict$fromList(
					A2(
						$elm$core$List$map,
						$elm$core$Tuple$mapFirst($elm$core$Tuple$second),
						listByIndexAndId)),
				byIndexAndId: $elm$core$Dict$fromList(listByIndexAndId)
			});
	};
	var keyDecoder = A3(
		$elm$json$Json$Decode$map2,
		$elm$core$Tuple$pair,
		A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
	var tuplesDecoder = A3(
		$elm$json$Json$Decode$map2,
		$elm$core$Tuple$pair,
		keyDecoder,
		A2(
			$elm$json$Json$Decode$field,
			'mapping',
			$elm$json$Json$Decode$dict(originDecoder)));
	return A2(
		$elm$json$Json$Decode$map,
		listToUserMappings,
		$elm$json$Json$Decode$list(tuplesDecoder));
}();
var $xarvh$elm_gamepad$Gamepad$Advanced$userMappingsFromString = function (asString) {
	return A2($elm$json$Json$Decode$decodeString, $xarvh$elm_gamepad$Gamepad$Advanced$userMappingsDecoder, asString);
};
var $author$project$GamepadPort$fromString = F2(
	function (s, orig) {
		var _v0 = $xarvh$elm_gamepad$Gamepad$Advanced$userMappingsFromString(s);
		if (_v0.$ === 'Ok') {
			var mappings = _v0.a;
			return mappings;
		} else {
			return orig;
		}
	});
var $author$project$Main$fullCategoryListRequestJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('Menu')),
			_Utils_Tuple2(
			'contents',
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('MenuFullCategories'))
					])))
		]));
var $xarvh$elm_gamepad$Gamepad$Private$Gamepad = F3(
	function (a, b, c) {
		return {$: 'Gamepad', a: a, b: b, c: c};
	});
var $xarvh$elm_gamepad$Gamepad$Back = {$: 'Back'};
var $xarvh$elm_gamepad$Gamepad$DpadDown = {$: 'DpadDown'};
var $xarvh$elm_gamepad$Gamepad$DpadLeft = {$: 'DpadLeft'};
var $xarvh$elm_gamepad$Gamepad$DpadRight = {$: 'DpadRight'};
var $xarvh$elm_gamepad$Gamepad$DpadUp = {$: 'DpadUp'};
var $xarvh$elm_gamepad$Gamepad$Home = {$: 'Home'};
var $xarvh$elm_gamepad$Gamepad$LeftBumper = {$: 'LeftBumper'};
var $xarvh$elm_gamepad$Gamepad$LeftStickDown = {$: 'LeftStickDown'};
var $xarvh$elm_gamepad$Gamepad$LeftStickLeft = {$: 'LeftStickLeft'};
var $xarvh$elm_gamepad$Gamepad$LeftStickPress = {$: 'LeftStickPress'};
var $xarvh$elm_gamepad$Gamepad$LeftStickRight = {$: 'LeftStickRight'};
var $xarvh$elm_gamepad$Gamepad$LeftStickUp = {$: 'LeftStickUp'};
var $xarvh$elm_gamepad$Gamepad$LeftTrigger = {$: 'LeftTrigger'};
var $xarvh$elm_gamepad$Gamepad$RightBumper = {$: 'RightBumper'};
var $xarvh$elm_gamepad$Gamepad$RightStickDown = {$: 'RightStickDown'};
var $xarvh$elm_gamepad$Gamepad$RightStickLeft = {$: 'RightStickLeft'};
var $xarvh$elm_gamepad$Gamepad$RightStickPress = {$: 'RightStickPress'};
var $xarvh$elm_gamepad$Gamepad$RightStickRight = {$: 'RightStickRight'};
var $xarvh$elm_gamepad$Gamepad$RightStickUp = {$: 'RightStickUp'};
var $xarvh$elm_gamepad$Gamepad$RightTrigger = {$: 'RightTrigger'};
var $xarvh$elm_gamepad$Gamepad$Start = {$: 'Start'};
var $xarvh$elm_gamepad$Gamepad$digitalToString = function (destination) {
	switch (destination.$) {
		case 'A':
			return 'A';
		case 'B':
			return 'B';
		case 'X':
			return 'X';
		case 'Y':
			return 'Y';
		case 'Start':
			return 'Start';
		case 'Back':
			return 'Back';
		case 'Home':
			return 'Home';
		case 'LeftStickLeft':
			return 'LeftLeft';
		case 'LeftStickRight':
			return 'LeftRight';
		case 'LeftStickUp':
			return 'LeftUp';
		case 'LeftStickDown':
			return 'LeftDown';
		case 'LeftStickPress':
			return 'LeftPress';
		case 'LeftBumper':
			return 'LeftBumper';
		case 'LeftTrigger':
			return 'LeftTrigger';
		case 'RightStickLeft':
			return 'RightLeft';
		case 'RightStickRight':
			return 'RightRight';
		case 'RightStickUp':
			return 'RightUp';
		case 'RightStickDown':
			return 'RightDown';
		case 'RightStickPress':
			return 'RightPress';
		case 'RightBumper':
			return 'RightBumper';
		case 'RightTrigger':
			return 'RightTrigger';
		case 'DpadUp':
			return 'DpadUp';
		case 'DpadDown':
			return 'DpadDown';
		case 'DpadLeft':
			return 'DpadLeft';
		default:
			return 'DpadRight';
	}
};
var $xarvh$elm_gamepad$Gamepad$Advanced$pairsToMapping = F2(
	function (digitalToString, pairs) {
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var origin = _v0.a;
					var digital = _v0.b;
					return _Utils_Tuple2(
						digitalToString(digital),
						origin);
				},
				pairs));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$allStandardMapping = A2(
	$xarvh$elm_gamepad$Gamepad$Advanced$pairsToMapping,
	$xarvh$elm_gamepad$Gamepad$digitalToString,
	A2(
		$elm$core$List$map,
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			return _Utils_Tuple2(b, a);
		},
		_List_fromArray(
			[
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$A,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 0)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$B,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 1)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$X,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 2)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$Y,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 3)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$Start,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 9)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$Back,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 8)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$Home,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 16)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftStickLeft,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, true, $xarvh$elm_gamepad$Gamepad$Private$Axis, 0)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftStickRight,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Axis, 0)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftStickUp,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, true, $xarvh$elm_gamepad$Gamepad$Private$Axis, 1)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftStickDown,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Axis, 1)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftStickPress,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 10)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftBumper,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 4)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$LeftTrigger,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 6)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightStickLeft,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, true, $xarvh$elm_gamepad$Gamepad$Private$Axis, 2)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightStickRight,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Axis, 2)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightStickUp,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, true, $xarvh$elm_gamepad$Gamepad$Private$Axis, 3)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightStickDown,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Axis, 3)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightStickPress,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 11)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightBumper,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 5)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$RightTrigger,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 7)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$DpadUp,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 12)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$DpadDown,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 13)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$DpadLeft,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 14)),
				_Utils_Tuple2(
				$xarvh$elm_gamepad$Gamepad$DpadRight,
				A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, 15))
			])));
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$getGamepadMapping = F3(
	function (standardMapping, _v0, frame) {
		var database = _v0.a;
		var _v1 = A2(
			$elm$core$Dict$get,
			_Utils_Tuple2(frame.index, frame.id),
			database.byIndexAndId);
		if (_v1.$ === 'Just') {
			var mapping = _v1.a;
			return $elm$core$Maybe$Just(mapping);
		} else {
			return (frame.mapping === 'standard') ? $elm$core$Maybe$Just(standardMapping) : A2($elm$core$Dict$get, frame.id, database.byId);
		}
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$getGamepads = F3(
	function (controls, userMappings, _v0) {
		var currentBlobFrame = _v0.a;
		var previousBlobFrame = _v0.b;
		var env = _v0.c;
		var isConfigured = F2(
			function (digitalAsString, origin) {
				return A2(
					$elm$core$List$any,
					function (_v1) {
						var name = _v1.a;
						var digital = _v1.b;
						return _Utils_eq(
							$xarvh$elm_gamepad$Gamepad$digitalToString(digital),
							digitalAsString);
					},
					controls);
			});
		var standardMapping = A2($elm$core$Dict$filter, isConfigured, $xarvh$elm_gamepad$Gamepad$Advanced$allStandardMapping);
		var getGamepad = function (currentGamepadFrame) {
			return A3(
				$elm$core$Maybe$map2,
				F2(
					function (previousGamepadFrame, mapping) {
						return A3($xarvh$elm_gamepad$Gamepad$Private$Gamepad, mapping, currentGamepadFrame, previousGamepadFrame);
					}),
				A2(
					$elm_community$list_extra$List$Extra$find,
					function (prev) {
						return _Utils_eq(prev.index, currentGamepadFrame.index);
					},
					previousBlobFrame.gamepads),
				A3($xarvh$elm_gamepad$Gamepad$Advanced$getGamepadMapping, standardMapping, userMappings, currentGamepadFrame));
		};
		return A2($elm$core$List$filterMap, getGamepad, currentBlobFrame.gamepads);
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$Model = function (a) {
	return {$: 'Model', a: a};
};
var $xarvh$elm_gamepad$Gamepad$Private$emptyBlobFrame = {gamepads: _List_Nil, timestamp: 0};
var $xarvh$elm_gamepad$Gamepad$Private$emptyEnvironment = {languages: _List_Nil, userMappings: '{}'};
var $xarvh$elm_gamepad$Gamepad$Private$emptyBlob = _Utils_Tuple3($xarvh$elm_gamepad$Gamepad$Private$emptyBlobFrame, $xarvh$elm_gamepad$Gamepad$Private$emptyBlobFrame, $xarvh$elm_gamepad$Gamepad$Private$emptyEnvironment);
var $xarvh$elm_gamepad$Gamepad$Advanced$init = function (controls) {
	return $xarvh$elm_gamepad$Gamepad$Advanced$Model(
		{blob: $xarvh$elm_gamepad$Gamepad$Private$emptyBlob, controls: controls, maybeRemapping: $elm$core$Maybe$Nothing});
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Main$newClientJSON = $elm$json$Json$Encode$object(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'tag',
			$elm$json$Json$Encode$string('NewClient')),
			_Utils_Tuple2('contents', $elm$json$Json$Encode$null)
		]));
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$LoadSplits = function (a) {
	return {$: 'LoadSplits', a: a};
};
var $author$project$Timer$buildSegmentSums = F2(
	function (nextSplit, _v0) {
		var runningTime = _v0.a;
		var splits = _v0.b;
		var seg = function ($) {
			return $.segment;
		}(nextSplit);
		var pb = A2(
			$elm$core$Maybe$map,
			$elm$core$Basics$add(runningTime),
			function ($) {
				return $.pb;
			}(seg));
		var nextRunning = A2($elm$core$Maybe$withDefault, runningTime, pb);
		return _Utils_Tuple2(
			nextRunning,
			_Utils_ap(
				splits,
				_List_fromArray(
					[
						_Utils_update(
						nextSplit,
						{
							segment: _Utils_update(
								seg,
								{pb: pb})
						})
					])));
	});
var $author$project$Timer$loadSegments = function (run) {
	var splitset = function ($) {
		return $.splits;
	}(run);
	var _v0 = A3(
		$elm$core$List$foldl,
		$author$project$Timer$buildSegmentSums,
		_Utils_Tuple2(0, _List_Nil),
		function ($) {
			return $.upcoming;
		}(splitset));
	var upcoming = _v0.b;
	return _Utils_update(
		run,
		{
			splits: _Utils_update(
				splitset,
				{upcoming: upcoming})
		});
};
var $author$project$Timer$loadRun = function (runSpec) {
	if (runSpec.$ === 'SingleCategorySpec') {
		var segments = runSpec.a;
		return $author$project$Timer$SingleCategory(
			$author$project$Timer$loadSegments(segments));
	} else {
		var eid = runSpec.a;
		var segments = runSpec.b;
		return $author$project$Timer$MultiCategory(
			{
				current: $elm$core$Maybe$Nothing,
				entityID: $elm$core$Maybe$Just(eid),
				previous: _List_Nil,
				upcoming: A2($elm$core$List$map, $author$project$Timer$loadSegments, segments)
			});
	}
};
var $author$project$Timer$load = function (newSplits) {
	return $author$project$Timer$Stopped(
		_Utils_update(
			$author$project$Timer$empty_,
			{
				runTracker: A2(
					$elm$core$Basics$composeL,
					$author$project$Timer$loadRun,
					function ($) {
						return $.run;
					})(newSplits),
				subtitle: function ($) {
					return $.subtitle;
				}(newSplits),
				title: function ($) {
					return $.title;
				}(newSplits)
			}));
};
var $author$project$Timer$Category = F3(
	function (entityID, name, offset) {
		return {entityID: entityID, name: name, offset: offset};
	});
var $author$project$Main$FetchedCategoryList = function (a) {
	return {$: 'FetchedCategoryList', a: a};
};
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $author$project$Main$checkTag_ = F3(
	function (ds, target, da) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (s) {
				return _Utils_eq(s, target) ? da : $elm$json$Json$Decode$fail('No match on tag:' + s);
			},
			ds);
	});
var $author$project$Main$checkTag = F2(
	function (target, da) {
		return A3(
			$author$project$Main$checkTag_,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'tag']),
				$elm$json$Json$Decode$string),
			target,
			da);
	});
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Main$categoryListDecoder = A2(
	$author$project$Main$checkTag,
	'CategoryList',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$FetchedCategoryList,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'contents']),
			$elm$json$Json$Decode$list(
				A4(
					$elm$json$Json$Decode$map3,
					$author$project$Timer$Category,
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['categoryID']),
						$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['categoryData', 'categoryName']),
						$elm$json$Json$Decode$string),
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['categoryData', 'categoryOffset']),
						$elm$json$Json$Decode$int))))));
var $author$project$Main$ClientStateRequest = function (a) {
	return {$: 'ClientStateRequest', a: a};
};
var $author$project$Main$clientStateRequestDecoder = A2(
	$author$project$Main$checkTag,
	'ClientStateRequest',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$ClientStateRequest,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'contents']),
			$elm$json$Json$Decode$int)));
var $author$project$Main$UnloadSplits = {$: 'UnloadSplits'};
var $author$project$Main$closeSplitsDecoder = A2(
	$author$project$Main$checkTag,
	'CloseSplits',
	A2(
		$elm$json$Json$Decode$map,
		function (_v0) {
			return $author$project$Main$UnloadSplits;
		},
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'tag']),
			$elm$json$Json$Decode$string)));
var $author$project$Main$ConfigStoreSet = F2(
	function (a, b) {
		return {$: 'ConfigStoreSet', a: a, b: b};
	});
var $author$project$Main$configStoreDecoder = A2(
	$author$project$Main$checkTag,
	'ConfigStore',
	A3(
		$elm$json$Json$Decode$map2,
		$author$project$Main$ConfigStoreSet,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'contents']),
			A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'contents']),
			A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string))));
var $author$project$Main$FullCategoryList = function (a) {
	return {$: 'FullCategoryList', a: a};
};
var $author$project$Timer$MultiCategorySpec = F2(
	function (a, b) {
		return {$: 'MultiCategorySpec', a: a, b: b};
	});
var $author$project$Timer$Run = F5(
	function (runStarted, runEnded, game, category, splits) {
		return {category: category, game: game, runEnded: runEnded, runStarted: runStarted, splits: splits};
	});
var $author$project$Main$categoryDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Timer$Category,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetCategoryID']),
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetCategoryData', 'categoryName']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetCategoryData', 'categoryOffset']),
		$elm$json$Json$Decode$int));
var $author$project$Timer$Game = F4(
	function (entityID, name, icon, offset) {
		return {entityID: entityID, icon: icon, name: name, offset: offset};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Main$gameDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Timer$Game,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetGameID']),
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetGameData', 'gameName']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetGameData', 'gameIcon']),
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetGameData', 'gameDefaultOffset']),
		$elm$json$Json$Decode$int));
var $author$project$Timer$Segment = F7(
	function (entityID, name, icon, pb, gold, average, worst) {
		return {average: average, entityID: entityID, gold: gold, icon: icon, name: name, pb: pb, worst: worst};
	});
var $author$project$Timer$Split = F3(
	function (segment, endTime, segmentTime) {
		return {endTime: endTime, segment: segment, segmentTime: segmentTime};
	});
var $author$project$Timer$SplitSet = F3(
	function (previous, current, upcoming) {
		return {current: current, previous: previous, upcoming: upcoming};
	});
var $elm$json$Json$Decode$map7 = _Json_map7;
var $author$project$Main$splitSetDecoder = A2(
	$elm$json$Json$Decode$map,
	A2($author$project$Timer$SplitSet, _List_Nil, $elm$core$Maybe$Nothing),
	$elm$json$Json$Decode$list(
		A2(
			$elm$json$Json$Decode$map,
			function (s) {
				return A3($author$project$Timer$Split, s, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
			},
			A8(
				$elm$json$Json$Decode$map7,
				$author$project$Timer$Segment,
				A2(
					$elm$json$Json$Decode$field,
					'segmentID',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
				A2($elm$json$Json$Decode$field, 'segmentName', $elm$json$Json$Decode$string),
				A2(
					$elm$json$Json$Decode$field,
					'segmentIcon',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'segmentPB',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
				A2(
					$elm$json$Json$Decode$field,
					'segmentGold',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
				A2(
					$elm$json$Json$Decode$field,
					'segmentAverage',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
				A2(
					$elm$json$Json$Decode$field,
					'segmentWorst',
					$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int))))));
var $author$project$Main$singleCategorySpecDecoder_ = A4(
	$elm$json$Json$Decode$map3,
	A2($author$project$Timer$Run, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
	$author$project$Main$gameDecoder,
	$author$project$Main$categoryDecoder,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['splitSetSegments']),
		$author$project$Main$splitSetDecoder));
var $author$project$Main$multiCategorySpecDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Timer$MultiCategorySpec,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['multiCategoryID']),
		$elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['multiCategoryGames']),
		$elm$json$Json$Decode$list($author$project$Main$singleCategorySpecDecoder_)));
var $author$project$Timer$SingleCategorySpec = function (a) {
	return {$: 'SingleCategorySpec', a: a};
};
var $author$project$Main$singleCategorySpecDecoder = A2($elm$json$Json$Decode$map, $author$project$Timer$SingleCategorySpec, $author$project$Main$singleCategorySpecDecoder_);
var $author$project$Main$runSpecDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[$author$project$Main$singleCategorySpecDecoder, $author$project$Main$multiCategorySpecDecoder]));
var $author$project$Main$fullCategoryListDecoder = A2(
	$author$project$Main$checkTag,
	'FullCategoryList',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$FullCategoryList,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['data', 'contents']),
			$elm$json$Json$Decode$list($author$project$Main$runSpecDecoder))));
var $author$project$Main$FetchedGameList = function (a) {
	return {$: 'FetchedGameList', a: a};
};
var $author$project$Main$FullGameList = F2(
	function (gameList, multiCategoryList) {
		return {gameList: gameList, multiCategoryList: multiCategoryList};
	});
var $author$project$Main$MultiCategoryInfo = F2(
	function (name, entityID) {
		return {entityID: entityID, name: name};
	});
var $author$project$Main$gameListDecoder = A2(
	$author$project$Main$checkTag,
	'GameList',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$FetchedGameList,
		A3(
			$elm$json$Json$Decode$map2,
			$author$project$Main$FullGameList,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'gamesList']),
				$elm$json$Json$Decode$list(
					A5(
						$elm$json$Json$Decode$map4,
						$author$project$Timer$Game,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['gameID']),
							$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['gameData', 'gameName']),
							$elm$json$Json$Decode$string),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['gameData', 'gameIcon']),
							$elm$json$Json$Decode$maybe($elm$json$Json$Decode$string)),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['gameData', 'gameDefaultOffset']),
							$elm$json$Json$Decode$int)))),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'multiCategoriesList']),
				$elm$json$Json$Decode$list(
					A3(
						$elm$json$Json$Decode$map2,
						$author$project$Main$MultiCategoryInfo,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['multiCategoryName']),
							$elm$json$Json$Decode$string),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['multiCategoryID']),
							$elm$json$Json$Decode$int)))))));
var $author$project$Main$FetchedSplits = function (a) {
	return {$: 'FetchedSplits', a: a};
};
var $author$project$Timer$SplitsSpec = F3(
	function (title, subtitle, run) {
		return {run: run, subtitle: subtitle, title: title};
	});
var $author$project$Main$multiCategoryDecoder = A2(
	$author$project$Main$checkTag,
	'MultiCategoryRefresh',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$FetchedSplits,
		A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (t, cs) {
					return A3($author$project$Timer$SplitsSpec, t, '', cs);
				}),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'multiCategoryTitle']),
				$elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents']),
				$author$project$Main$runSpecDecoder))));
var $author$project$Main$RemoteReset = {$: 'RemoteReset'};
var $author$project$Main$SplitsControl = function (a) {
	return {$: 'SplitsControl', a: a};
};
var $author$project$Main$checkTag2 = F2(
	function (target, da) {
		return A3(
			$author$project$Main$checkTag_,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'tag']),
				$elm$json$Json$Decode$string),
			target,
			da);
	});
var $author$project$Main$decodeRemoteReset = A2(
	$author$project$Main$checkTag,
	'RemoteControl',
	A2(
		$author$project$Main$checkTag2,
		'RemoteReset',
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$SplitsControl,
			$elm$json$Json$Decode$succeed($author$project$Main$RemoteReset))));
var $author$project$Main$RemoteSkip = {$: 'RemoteSkip'};
var $author$project$Main$decodeRemoteSkip = A2(
	$author$project$Main$checkTag,
	'RemoteControl',
	A2(
		$author$project$Main$checkTag2,
		'RemoteSkip',
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$SplitsControl,
			$elm$json$Json$Decode$succeed($author$project$Main$RemoteSkip))));
var $author$project$Main$RemoteStartSplit = function (a) {
	return {$: 'RemoteStartSplit', a: a};
};
var $author$project$Main$decodeRemoteStartSplit = A2(
	$author$project$Main$checkTag,
	'RemoteControl',
	A2(
		$author$project$Main$checkTag2,
		'RemoteStartSplit',
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$SplitsControl,
			A2(
				$elm$json$Json$Decode$map,
				$author$project$Main$RemoteStartSplit,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['data', 'contents', 'contents']),
					$elm$json$Json$Decode$int)))));
var $author$project$Main$RemoteStop = {$: 'RemoteStop'};
var $author$project$Main$decodeRemoteStop = A2(
	$author$project$Main$checkTag,
	'RemoteControl',
	A2(
		$author$project$Main$checkTag2,
		'RemoteStop',
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$SplitsControl,
			$elm$json$Json$Decode$succeed($author$project$Main$RemoteStop))));
var $author$project$Main$RemoteUnsplit = {$: 'RemoteUnsplit'};
var $author$project$Main$decodeRemoteUnsplit = A2(
	$author$project$Main$checkTag,
	'RemoteControl',
	A2(
		$author$project$Main$checkTag2,
		'RemoteUnsplit',
		A2(
			$elm$json$Json$Decode$map,
			$author$project$Main$SplitsControl,
			$elm$json$Json$Decode$succeed($author$project$Main$RemoteUnsplit))));
var $author$project$Main$splitsControlDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[$author$project$Main$decodeRemoteStartSplit, $author$project$Main$decodeRemoteUnsplit, $author$project$Main$decodeRemoteSkip, $author$project$Main$decodeRemoteStop, $author$project$Main$decodeRemoteReset]));
var $author$project$Main$splitsSpecDecoder = A2(
	$author$project$Main$checkTag,
	'SplitsRefresh',
	A2(
		$elm$json$Json$Decode$map,
		$author$project$Main$FetchedSplits,
		A4(
			$elm$json$Json$Decode$map3,
			$author$project$Timer$SplitsSpec,
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'splitSetGameData', 'gameName']),
				$elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents', 'splitSetCategoryData', 'categoryName']),
				$elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['data', 'contents']),
				$author$project$Main$runSpecDecoder))));
var $author$project$Main$processIncomingEvent = $elm$json$Json$Decode$decodeValue(
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[$author$project$Main$clientStateRequestDecoder, $author$project$Main$splitsControlDecoder, $author$project$Main$gameListDecoder, $author$project$Main$categoryListDecoder, $author$project$Main$fullCategoryListDecoder, $author$project$Main$splitsSpecDecoder, $author$project$Main$multiCategoryDecoder, $author$project$Main$closeSplitsDecoder, $author$project$Main$splitsControlDecoder, $author$project$Main$configStoreDecoder])));
var $author$project$Main$Stop = {$: 'Stop'};
var $author$project$Main$finishSplitsRequestJSON = function (t) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string('TimerControl')),
				_Utils_Tuple2(
				'contents',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'tag',
							$elm$json$Json$Encode$string('RemoteFinish')),
							_Utils_Tuple2(
							'contents',
							$author$project$Timer$toJSON(t))
						])))
			]));
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Timer$resetSplit = function (s) {
	return _Utils_update(
		s,
		{endTime: $elm$core$Maybe$Nothing, segmentTime: $elm$core$Maybe$Nothing});
};
var $author$project$Timer$resetSplitSet = function (set) {
	return {
		current: $elm$core$Maybe$Nothing,
		previous: _List_Nil,
		upcoming: A2(
			$elm$core$List$map,
			$author$project$Timer$resetSplit,
			$author$project$Timer$allSplits(set))
	};
};
var $author$project$Timer$resetRun = function (run) {
	return _Utils_update(
		run,
		{
			splits: $author$project$Timer$resetSplitSet(
				function ($) {
					return $.splits;
				}(run))
		});
};
var $author$project$Timer$allRuns = function (set) {
	var _v0 = function ($) {
		return $.current;
	}(set);
	if (_v0.$ === 'Nothing') {
		return A2(
			$elm$core$List$append,
			function ($) {
				return $.previous;
			}(set),
			function ($) {
				return $.upcoming;
			}(set));
	} else {
		var r = _v0.a;
		return $elm$core$List$concat(
			_List_fromArray(
				[
					function ($) {
					return $.previous;
				}(set),
					_List_fromArray(
					[r]),
					function ($) {
					return $.upcoming;
				}(set)
				]));
	}
};
var $author$project$Timer$resetRuns = function (runs) {
	return {
		current: $elm$core$Maybe$Nothing,
		entityID: function ($) {
			return $.entityID;
		}(runs),
		previous: _List_Nil,
		upcoming: A2(
			$elm$core$List$map,
			$author$project$Timer$resetRun,
			$author$project$Timer$allRuns(runs))
	};
};
var $author$project$Timer$resetRunTracker = function (runTracker) {
	if (runTracker.$ === 'SingleCategory') {
		var run = runTracker.a;
		return $author$project$Timer$SingleCategory(
			$author$project$Timer$resetRun(run));
	} else {
		var runs = runTracker.a;
		return $author$project$Timer$MultiCategory(
			$author$project$Timer$resetRuns(runs));
	}
};
var $author$project$Timer$reset_ = function (splits) {
	return _Utils_update(
		splits,
		{
			runEnded: $elm$core$Maybe$Nothing,
			runStarted: $elm$core$Maybe$Nothing,
			runTracker: $author$project$Timer$resetRunTracker(
				function ($) {
					return $.runTracker;
				}(splits))
		});
};
var $author$project$Timer$stop = function (t) {
	return $author$project$Timer$Stopped(
		$author$project$Timer$splitsFor(t));
};
var $author$project$Timer$reset = A2(
	$elm$core$Basics$composeR,
	$author$project$Timer$stop,
	$author$project$Timer$mapT($author$project$Timer$reset_));
var $author$project$Timer$skip_ = function (run) {
	var splits = function ($) {
		return $.splits;
	}(run);
	var up = function ($) {
		return $.upcoming;
	}(splits);
	var prev = function ($) {
		return $.previous;
	}(splits);
	if (!up.b) {
		return run;
	} else {
		var next = up.a;
		var rest = up.b;
		var _v1 = function ($) {
			return $.current;
		}(splits);
		if (_v1.$ === 'Nothing') {
			return run;
		} else {
			var r = _v1.a;
			return _Utils_update(
				run,
				{
					splits: _Utils_update(
						splits,
						{
							current: $elm$core$Maybe$Just(next),
							previous: A2(
								$elm$core$List$append,
								prev,
								_List_fromArray(
									[
										_Utils_update(
										r,
										{endTime: $elm$core$Maybe$Nothing, segmentTime: $elm$core$Maybe$Nothing})
									])),
							upcoming: rest
						})
				});
		}
	}
};
var $author$project$Timer$skip = $author$project$Timer$mapT(
	$author$project$Timer$mapR($author$project$Timer$skip_));
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Timer$runFor = function (timer) {
	var _v0 = $author$project$Timer$splitsFor(timer).runTracker;
	if (_v0.$ === 'SingleCategory') {
		var r = _v0.a;
		return $elm$core$Maybe$Just(r);
	} else {
		var rs = _v0.a;
		var _v1 = function ($) {
			return $.current;
		}(rs);
		if (_v1.$ === 'Nothing') {
			return $elm$core$List$head(
				$elm$core$List$reverse(
					function ($) {
						return $.previous;
					}(rs)));
		} else {
			var r = _v1;
			return r;
		}
	}
};
var $author$project$Timer$split_ = F2(
	function (t, run) {
		var splits = function ($) {
			return $.splits;
		}(run);
		var up = function ($) {
			return $.upcoming;
		}(splits);
		var prev = function ($) {
			return $.previous;
		}(splits);
		var segmentDiff = A3(
			$elm$core$List$foldl,
			F2(
				function (candidate, lastKnown) {
					return _Utils_eq(
						function ($) {
							return $.endTime;
						}(candidate),
						$elm$core$Maybe$Nothing) ? lastKnown : function ($) {
						return $.endTime;
					}(candidate);
				}),
			function ($) {
				return $.runStarted;
			}(run),
			prev);
		var singleSegment = A2(
			$elm$core$Maybe$map,
			function (t_) {
				return $elm$time$Time$millisToPosix(
					$elm$time$Time$posixToMillis(t) - $elm$time$Time$posixToMillis(t_));
			},
			segmentDiff);
		if (!up.b) {
			var _v1 = function ($) {
				return $.current;
			}(splits);
			if (_v1.$ === 'Nothing') {
				return run;
			} else {
				var r = _v1.a;
				return _Utils_update(
					run,
					{
						runEnded: $elm$core$Maybe$Just(t),
						splits: _Utils_update(
							splits,
							{
								current: $elm$core$Maybe$Nothing,
								previous: A2(
									$elm$core$List$append,
									prev,
									_List_fromArray(
										[
											_Utils_update(
											r,
											{
												endTime: $elm$core$Maybe$Just(t),
												segmentTime: singleSegment
											})
										]))
							})
					});
			}
		} else {
			var next = up.a;
			var rest = up.b;
			var _v2 = function ($) {
				return $.current;
			}(splits);
			if (_v2.$ === 'Nothing') {
				return _Utils_update(
					run,
					{
						runStarted: $elm$core$Maybe$Just(t),
						splits: _Utils_update(
							splits,
							{
								current: $elm$core$Maybe$Just(next),
								upcoming: rest
							})
					});
			} else {
				var r = _v2.a;
				return _Utils_update(
					run,
					{
						splits: _Utils_update(
							splits,
							{
								current: $elm$core$Maybe$Just(next),
								previous: A2(
									$elm$core$List$append,
									prev,
									_List_fromArray(
										[
											_Utils_update(
											r,
											{
												endTime: $elm$core$Maybe$Just(t),
												segmentTime: singleSegment
											})
										])),
								upcoming: rest
							})
					});
			}
		}
	});
var $author$project$Timer$splitRun = A2(
	$elm$core$Basics$composeL,
	A2($elm$core$Basics$composeL, $author$project$Timer$mapT, $author$project$Timer$mapR),
	$author$project$Timer$split_);
var $author$project$Timer$split = F2(
	function (time, t) {
		var currentTime = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Timer$splitsFor(t).currentTime,
			time);
		if (t.$ === 'Running') {
			var splits = t.a;
			var _v1 = function ($) {
				return $.runTracker;
			}(splits);
			if (_v1.$ === 'SingleCategory') {
				var newt = A2($author$project$Timer$splitRun, currentTime, t);
				var _v2 = $author$project$Timer$runFor(newt);
				if (_v2.$ === 'Nothing') {
					return newt;
				} else {
					var r = _v2.a;
					var _v3 = function ($) {
						return $.splits;
					}(r).current;
					if (_v3.$ === 'Nothing') {
						var news = $author$project$Timer$splitsFor(newt);
						return $author$project$Timer$Finished(
							_Utils_update(
								news,
								{
									runEnded: $elm$core$Maybe$Just(
										function ($) {
											return $.currentTime;
										}(news))
								}));
					} else {
						return newt;
					}
				}
			} else {
				var rs = _v1.a;
				var _v4 = function ($) {
					return $.current;
				}(rs);
				if (_v4.$ === 'Just') {
					var newt = A2($author$project$Timer$splitRun, currentTime, t);
					var _v5 = $author$project$Timer$runFor(newt);
					if (_v5.$ === 'Nothing') {
						return newt;
					} else {
						var r = _v5.a;
						var _v6 = function ($) {
							return $.splits;
						}(r).current;
						if (_v6.$ === 'Nothing') {
							var news = $author$project$Timer$splitsFor(newt);
							var newr = _Utils_update(
								r,
								{
									runEnded: $elm$core$Maybe$Just(
										function ($) {
											return $.currentTime;
										}(news))
								});
							var newrt = _Utils_update(
								rs,
								{
									current: $elm$core$List$head(
										function ($) {
											return $.upcoming;
										}(rs)),
									previous: _Utils_ap(
										function ($) {
											return $.previous;
										}(rs),
										_List_fromArray(
											[newr])),
									upcoming: A2(
										$elm$core$List$drop,
										1,
										function ($) {
											return $.upcoming;
										}(rs))
								});
							var _v7 = function ($) {
								return $.current;
							}(newrt);
							if (_v7.$ === 'Nothing') {
								return $author$project$Timer$Finished(
									_Utils_update(
										news,
										{
											runEnded: $elm$core$Maybe$Just(
												function ($) {
													return $.currentTime;
												}(news)),
											runTracker: $author$project$Timer$MultiCategory(newrt)
										}));
							} else {
								return $author$project$Timer$Running(
									_Utils_update(
										news,
										{
											runTracker: $author$project$Timer$MultiCategory(newrt)
										}));
							}
						} else {
							return newt;
						}
					}
				} else {
					var _v8 = $elm$core$List$head(
						function ($) {
							return $.upcoming;
						}(rs));
					if (_v8.$ === 'Nothing') {
						return $author$project$Timer$Finished(splits);
					} else {
						var r = _v8;
						return $author$project$Timer$Running(
							_Utils_update(
								splits,
								{
									runTracker: $author$project$Timer$MultiCategory(
										_Utils_update(
											rs,
											{
												current: r,
												upcoming: A2(
													$elm$core$List$drop,
													1,
													function ($) {
														return $.upcoming;
													}(rs))
											}))
								}));
					}
				}
			}
		} else {
			return t;
		}
	});
var $author$project$Timer$start = F2(
	function (time, t) {
		var currentTime = $elm$core$Maybe$Just(
			A2(
				$elm$core$Maybe$withDefault,
				$author$project$Timer$splitsFor(t).currentTime,
				time));
		switch (t.$) {
			case 'Stopped':
				var s = t.a;
				var _v1 = function ($) {
					return $.runTracker;
				}(s);
				if (_v1.$ === 'SingleCategory') {
					return A2(
						$author$project$Timer$split,
						currentTime,
						$author$project$Timer$Running(
							_Utils_update(
								s,
								{
									runStarted: $elm$core$Maybe$Just(
										function ($) {
											return $.currentTime;
										}(s))
								})));
				} else {
					var rs = _v1.a;
					var newr = $author$project$Timer$MultiCategory(
						_Utils_update(
							rs,
							{
								current: $elm$core$List$head(
									function ($) {
										return $.upcoming;
									}(rs)),
								upcoming: A2(
									$elm$core$List$drop,
									1,
									function ($) {
										return $.upcoming;
									}(rs))
							}));
					return A2(
						$author$project$Timer$split,
						currentTime,
						$author$project$Timer$Running(
							_Utils_update(
								s,
								{
									runStarted: $elm$core$Maybe$Just(
										function ($) {
											return $.currentTime;
										}(s)),
									runTracker: newr
								})));
				}
			case 'Paused':
				var s = t.a;
				return $author$project$Timer$Running(s);
			default:
				return t;
		}
	});
var $author$project$Timer$unsplit_ = function (run) {
	var splits = function ($) {
		return $.splits;
	}(run);
	var up = function ($) {
		return $.upcoming;
	}(splits);
	var _v0 = $elm$core$List$reverse(
		function ($) {
			return $.previous;
		}(splits));
	if (!_v0.b) {
		return run;
	} else {
		var x = _v0.a;
		var xs = _v0.b;
		var _v1 = function ($) {
			return $.current;
		}(splits);
		if (_v1.$ === 'Nothing') {
			return _Utils_update(
				run,
				{
					splits: _Utils_update(
						splits,
						{
							current: $elm$core$Maybe$Just(
								_Utils_update(
									x,
									{endTime: $elm$core$Maybe$Nothing, segmentTime: $elm$core$Maybe$Nothing})),
							previous: $elm$core$List$reverse(xs)
						})
				});
		} else {
			var s = _v1.a;
			return _Utils_update(
				run,
				{
					splits: _Utils_update(
						splits,
						{
							current: $elm$core$Maybe$Just(
								_Utils_update(
									x,
									{endTime: $elm$core$Maybe$Nothing, segmentTime: $elm$core$Maybe$Nothing})),
							previous: $elm$core$List$reverse(xs),
							upcoming: A2(
								$elm$core$List$cons,
								_Utils_update(
									s,
									{endTime: $elm$core$Maybe$Nothing, segmentTime: $elm$core$Maybe$Nothing}),
								up)
						})
				});
		}
	}
};
var $author$project$Timer$unsplit = function (t) {
	var _v0 = A2(
		$author$project$Timer$mapT,
		$author$project$Timer$mapR($author$project$Timer$unsplit_),
		t);
	if (_v0.$ === 'Finished') {
		var r = _v0.a;
		return $author$project$Timer$Running(r);
	} else {
		var r = _v0;
		return r;
	}
};
var $author$project$Main$timerControl = F3(
	function (broadcast, model, msg) {
		var _v0 = function ($) {
			return $.splitsMode;
		}(model);
		if (_v0.$ === 'EditSplitsView') {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var msg_ = broadcast ? A2(
				$author$project$Main$broadcastIntent,
				function ($) {
					return $.socket;
				}(model),
				msg) : $elm$core$Platform$Cmd$none;
			switch (msg.$) {
				case 'StartSplit':
					var t = msg.a;
					var offsetTime = A2(
						$elm$core$Maybe$map,
						function (time) {
							return $elm$time$Time$millisToPosix(
								$elm$time$Time$posixToMillis(time) - A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$elm$time$Time$posixToMillis,
										$author$project$Timer$splitsFor(
											function ($) {
												return $.timer;
											}(model)).runStarted)));
						},
						t);
					var msg2 = broadcast ? A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						$author$project$Main$StartSplit(offsetTime)) : $elm$core$Platform$Cmd$none;
					var _v2 = function ($) {
						return $.timer;
					}(model);
					switch (_v2.$) {
						case 'Running':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										timer: A2(
											$author$project$Timer$split,
											t,
											function ($) {
												return $.timer;
											}(model))
									}),
								msg2);
						case 'Finished':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										timer: $author$project$Timer$reset(
											function ($) {
												return $.timer;
											}(model))
									}),
								A2(
									$author$project$Main$send,
									function ($) {
										return $.socket;
									}(model),
									$author$project$Main$finishSplitsRequestJSON(
										function ($) {
											return $.timer;
										}(model))));
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										timer: A2(
											$author$project$Timer$start,
											t,
											function ($) {
												return $.timer;
											}(model))
									}),
								msg2);
					}
				case 'Reset':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								timer: $author$project$Timer$reset(
									function ($) {
										return $.timer;
									}(model))
							}),
						msg_);
				case 'Unsplit':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								timer: $author$project$Timer$unsplit(
									function ($) {
										return $.timer;
									}(model))
							}),
						msg_);
				case 'Skip':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								timer: $author$project$Timer$skip(
									function ($) {
										return $.timer;
									}(model))
							}),
						msg_);
				default:
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Main$processSplitsControl = F2(
	function (model, msg) {
		switch (msg.$) {
			case 'RemoteStartSplit':
				var t = msg.a;
				return A3(
					$author$project$Main$timerControl,
					false,
					model,
					$author$project$Main$StartSplit(
						A2(
							$elm$core$Maybe$map,
							function (time) {
								return $elm$time$Time$millisToPosix(
									$elm$time$Time$posixToMillis(time) + t);
							},
							$author$project$Timer$splitsFor(
								function ($) {
									return $.timer;
								}(model)).runStarted)));
			case 'RemoteUnsplit':
				return A3($author$project$Main$timerControl, false, model, $author$project$Main$Unsplit);
			case 'RemoteSkip':
				return A3($author$project$Main$timerControl, false, model, $author$project$Main$Skip);
			case 'RemoteStop':
				return A3($author$project$Main$timerControl, false, model, $author$project$Main$Stop);
			default:
				return A3(
					$author$project$Main$timerControl,
					false,
					model,
					$author$project$Main$Reset(
						function ($) {
							return $.timer;
						}(model)));
		}
	});
var $author$project$Main$processData = F2(
	function (model, data) {
		var _v0 = $author$project$Main$processIncomingEvent(data);
		_v0$7:
		while (true) {
			if (_v0.$ === 'Ok') {
				switch (_v0.a.$) {
					case 'SplitsControl':
						var ctl = _v0.a.a;
						return A2($author$project$Main$processSplitsControl, model, ctl);
					case 'UnloadSplits':
						var _v1 = _v0.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{timer: $author$project$Timer$empty}),
							$elm$core$Platform$Cmd$none);
					case 'FetchedGameList':
						var games = _v0.a.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									gameList: function ($) {
										return $.gameList;
									}(games),
									multiCategoryList: function ($) {
										return $.multiCategoryList;
									}(games)
								}),
							$elm$core$Platform$Cmd$none);
					case 'FetchedCategoryList':
						var cats = _v0.a.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{categoryList: cats}),
							$elm$core$Platform$Cmd$none);
					case 'FullCategoryList':
						var runs = _v0.a.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{fullCategoryList: runs}),
							$elm$core$Platform$Cmd$none);
					case 'FetchedSplits':
						var s = _v0.a.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									timer: $author$project$Timer$load(s)
								}),
							$elm$core$Platform$Cmd$none);
					case 'ConfigStoreSet':
						var _v2 = _v0.a;
						var k = _v2.a;
						var v = _v2.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									configStore: A3(
										$elm$core$Dict$insert,
										k,
										v,
										function ($) {
											return $.configStore;
										}(model))
								}),
							A2(
								$author$project$Main$broadcastIntent,
								function ($) {
									return $.socket;
								}(model),
								$author$project$Main$LoadSplits(
									$elm$core$String$toInt(v))));
					default:
						break _v0$7;
				}
			} else {
				break _v0$7;
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$GamepadPort$saveMappings = _Platform_outgoingPort('saveMappings', $elm$json$Json$Encode$string);
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$encodeUserMappings = function (_v0) {
	var database = _v0.a;
	var encodeOriginType = function (t) {
		return $elm$json$Json$Encode$string(
			function () {
				if (t.$ === 'Axis') {
					return 'axis';
				} else {
					return 'button';
				}
			}());
	};
	var encodeOrigin = function (_v3) {
		var isReverse = _v3.a;
		var type_ = _v3.b;
		var index = _v3.c;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'isReverse',
					$elm$json$Json$Encode$bool(isReverse)),
					_Utils_Tuple2(
					'type',
					encodeOriginType(type_)),
					_Utils_Tuple2(
					'index',
					$elm$json$Json$Encode$int(index))
				]));
	};
	var encodeMapping = function (mapping) {
		return A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, encodeOrigin, mapping);
	};
	var encodeTuples = function (_v1) {
		var _v2 = _v1.a;
		var index = _v2.a;
		var id = _v2.b;
		var mapping = _v1.b;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'index',
					$elm$json$Json$Encode$int(index)),
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(id)),
					_Utils_Tuple2(
					'mapping',
					encodeMapping(mapping))
				]));
	};
	return A2(
		$elm$json$Json$Encode$list,
		encodeTuples,
		$elm$core$Dict$toList(database.byIndexAndId));
};
var $xarvh$elm_gamepad$Gamepad$Advanced$userMappingsToString = function (userMappings) {
	return A2(
		$elm$json$Json$Encode$encode,
		0,
		$xarvh$elm_gamepad$Gamepad$Advanced$encodeUserMappings(userMappings));
};
var $author$project$Main$remapGamepad_ = F2(
	function (maybeUpdateUserMappings, model) {
		if (maybeUpdateUserMappings.$ === 'Nothing') {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var updateMappings = maybeUpdateUserMappings.a;
			var newUserMappings = updateMappings(
				function ($) {
					return $.userMappings;
				}(model));
			var newModel = _Utils_update(
				model,
				{userMappings: newUserMappings});
			var cmd = _Utils_eq(
				newUserMappings,
				function ($) {
					return $.userMappings;
				}(model)) ? $elm$core$Platform$Cmd$none : $author$project$GamepadPort$saveMappings(
				$xarvh$elm_gamepad$Gamepad$Advanced$userMappingsToString(newUserMappings));
			return _Utils_Tuple2(newModel, cmd);
		}
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$AllButtonsUp = {$: 'AllButtonsUp'};
var $xarvh$elm_gamepad$Gamepad$Advanced$initRemap = F2(
	function (id, index) {
		return {id: id, index: index, pairs: _List_Nil, skipped: _List_Nil, waitingFor: $xarvh$elm_gamepad$Gamepad$Advanced$AllButtonsUp};
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$nextUnmappedAction = F2(
	function (controls, remapping) {
		var mapped = _Utils_ap(
			A2($elm$core$List$map, $elm$core$Tuple$second, remapping.pairs),
			remapping.skipped);
		var needsMapping = function (_v0) {
			var name = _v0.a;
			var destination = _v0.b;
			return A2(
				$elm$core$List$all,
				$elm$core$Basics$neq(destination),
				mapped);
		};
		return A2($elm_community$list_extra$List$Extra$find, needsMapping, controls);
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$pairsToUpdateUserMappings = F4(
	function (id, index, pairs, _v0) {
		var database = _v0.a;
		var mapping = A2($xarvh$elm_gamepad$Gamepad$Advanced$pairsToMapping, $xarvh$elm_gamepad$Gamepad$digitalToString, pairs);
		return $xarvh$elm_gamepad$Gamepad$Advanced$UserMappings(
			{
				byId: A3($elm$core$Dict$insert, id, mapping, database.byId),
				byIndexAndId: A3(
					$elm$core$Dict$insert,
					_Utils_Tuple2(index, id),
					mapping,
					database.byIndexAndId)
			});
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$maybeEndRemapping = F2(
	function (controls, remapping) {
		if (!_Utils_eq(
			A2($xarvh$elm_gamepad$Gamepad$Advanced$nextUnmappedAction, controls, remapping),
			$elm$core$Maybe$Nothing)) {
			return _Utils_Tuple2(
				$elm$core$Maybe$Just(remapping),
				$elm$core$Maybe$Nothing);
		} else {
			var updateFunction = A3($xarvh$elm_gamepad$Gamepad$Advanced$pairsToUpdateUserMappings, remapping.id, remapping.index, remapping.pairs);
			var gamepadIsDisabled = _Utils_eq(remapping.pairs, _List_Nil);
			var maybeRemapping = gamepadIsDisabled ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(remapping);
			return _Utils_Tuple2(
				maybeRemapping,
				$elm$core$Maybe$Just(updateFunction));
		}
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$noCmd = function (model) {
	return _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
};
var $xarvh$elm_gamepad$Gamepad$Advanced$SomeButtonDown = {$: 'SomeButtonDown'};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $xarvh$elm_gamepad$Gamepad$Advanced$axisToEstimate = F2(
	function (originIndex, v) {
		return _Utils_Tuple2(
			A3($xarvh$elm_gamepad$Gamepad$Private$Origin, v < 0, $xarvh$elm_gamepad$Gamepad$Private$Axis, originIndex),
			$elm$core$Basics$abs(v));
	});
var $xarvh$elm_gamepad$Gamepad$Private$boolToNumber = function (bool) {
	return bool ? 1 : 0;
};
var $xarvh$elm_gamepad$Gamepad$Advanced$buttonToEstimate = F2(
	function (originIndex, _v0) {
		var pressed = _v0.a;
		var v = _v0.b;
		return _Utils_Tuple2(
			A3($xarvh$elm_gamepad$Gamepad$Private$Origin, false, $xarvh$elm_gamepad$Gamepad$Private$Button, originIndex),
			$xarvh$elm_gamepad$Gamepad$Private$boolToNumber(pressed));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$estimateThreshold = function (_v0) {
	var origin = _v0.a;
	var confidence = _v0.b;
	return (confidence < 0.5) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(origin);
};
var $elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$indexedMap = F2(
	function (func, _v0) {
		var len = _v0.a;
		var tree = _v0.c;
		var tail = _v0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				$elm$core$Elm$JsArray$indexedMap,
				func,
				$elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * $elm$core$Array$branchFactor;
					var mappedLeaf = $elm$core$Array$Leaf(
						A3($elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2($elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			$elm$core$Array$builderToArray,
			true,
			A3($elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var $elm$core$List$sortBy = _List_sortBy;
var $xarvh$elm_gamepad$Gamepad$Advanced$estimateOriginInFrame = function (frame) {
	var buttonsEstimates = A2($elm$core$Array$indexedMap, $xarvh$elm_gamepad$Gamepad$Advanced$buttonToEstimate, frame.buttons);
	var axesEstimates = A2($elm$core$Array$indexedMap, $xarvh$elm_gamepad$Gamepad$Advanced$axisToEstimate, frame.axes);
	return A2(
		$elm$core$Maybe$andThen,
		$xarvh$elm_gamepad$Gamepad$Advanced$estimateThreshold,
		$elm$core$List$head(
			$elm$core$List$reverse(
				A2(
					$elm$core$List$sortBy,
					$elm$core$Tuple$second,
					$elm$core$Array$toList(
						A2($elm$core$Array$append, axesEstimates, buttonsEstimates))))));
};
var $xarvh$elm_gamepad$Gamepad$Advanced$estimateOrigin = F2(
	function (_v0, index) {
		var currentBlobFrame = _v0.a;
		var previousBlobFrame = _v0.b;
		var env = _v0.c;
		return A2(
			$elm$core$Maybe$andThen,
			$xarvh$elm_gamepad$Gamepad$Advanced$estimateOriginInFrame,
			A2(
				$elm_community$list_extra$List$Extra$find,
				function (pad) {
					return _Utils_eq(pad.index, index);
				},
				currentBlobFrame.gamepads));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$insertPair = F3(
	function (origin, destination, remapping) {
		return _Utils_update(
			remapping,
			{
				pairs: A2(
					$elm$core$List$cons,
					_Utils_Tuple2(origin, destination),
					remapping.pairs)
			});
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$updateRemapping = F2(
	function (remapping, model) {
		var _v0 = _Utils_Tuple2(
			remapping.waitingFor,
			A2($xarvh$elm_gamepad$Gamepad$Advanced$estimateOrigin, model.blob, remapping.index));
		_v0$2:
		while (true) {
			if (_v0.a.$ === 'AllButtonsUp') {
				if (_v0.b.$ === 'Nothing') {
					var _v1 = _v0.a;
					var _v2 = _v0.b;
					return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(
						$elm$core$Maybe$Just(
							_Utils_update(
								remapping,
								{waitingFor: $xarvh$elm_gamepad$Gamepad$Advanced$SomeButtonDown})));
				} else {
					break _v0$2;
				}
			} else {
				if (_v0.b.$ === 'Just') {
					var _v3 = _v0.a;
					var origin = _v0.b.a;
					var _v4 = A2($xarvh$elm_gamepad$Gamepad$Advanced$nextUnmappedAction, model.controls, remapping);
					if (_v4.$ === 'Nothing') {
						return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd($elm$core$Maybe$Nothing);
					} else {
						var _v5 = _v4.a;
						var name = _v5.a;
						var destination = _v5.b;
						return A2(
							$xarvh$elm_gamepad$Gamepad$Advanced$maybeEndRemapping,
							model.controls,
							A3(
								$xarvh$elm_gamepad$Gamepad$Advanced$insertPair,
								origin,
								destination,
								_Utils_update(
									remapping,
									{waitingFor: $xarvh$elm_gamepad$Gamepad$Advanced$AllButtonsUp})));
					}
				} else {
					break _v0$2;
				}
			}
		}
		return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(
			$elm$core$Maybe$Just(remapping));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$updateOnGamepad = function (model) {
	var _v0 = model.maybeRemapping;
	if (_v0.$ === 'Nothing') {
		return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(model);
	} else {
		var remapping = _v0.a;
		return A2(
			$elm$core$Tuple$mapFirst,
			function (r) {
				return _Utils_update(
					model,
					{maybeRemapping: r});
			},
			A2($xarvh$elm_gamepad$Gamepad$Advanced$updateRemapping, remapping, model));
	}
};
var $xarvh$elm_gamepad$Gamepad$Advanced$update = F2(
	function (msg, _v0) {
		var model = _v0.a;
		return A2(
			$elm$core$Tuple$mapFirst,
			$xarvh$elm_gamepad$Gamepad$Advanced$Model,
			function () {
				switch (msg.$) {
					case 'Noop':
						return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(model);
					case 'OnGamepad':
						var blob = msg.a;
						return $xarvh$elm_gamepad$Gamepad$Advanced$updateOnGamepad(
							_Utils_update(
								model,
								{blob: blob}));
					case 'OnStartRemapping':
						var id = msg.a;
						var index = msg.b;
						return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(
							_Utils_update(
								model,
								{
									maybeRemapping: $elm$core$Maybe$Just(
										A2($xarvh$elm_gamepad$Gamepad$Advanced$initRemap, id, index))
								}));
					case 'OnCancel':
						return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(
							_Utils_update(
								model,
								{maybeRemapping: $elm$core$Maybe$Nothing}));
					default:
						var digital = msg.a;
						var _v2 = model.maybeRemapping;
						if (_v2.$ === 'Nothing') {
							return $xarvh$elm_gamepad$Gamepad$Advanced$noCmd(model);
						} else {
							var remapping = _v2.a;
							var _v3 = A2(
								$xarvh$elm_gamepad$Gamepad$Advanced$maybeEndRemapping,
								model.controls,
								_Utils_update(
									remapping,
									{
										skipped: A2($elm$core$List$cons, digital, remapping.skipped)
									}));
							var maybeRemapping = _v3.a;
							var maybeUpdate = _v3.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{maybeRemapping: maybeRemapping}),
								maybeUpdate);
						}
				}
			}());
	});
var $author$project$Main$remapGamepad = F2(
	function (model, remapMsg) {
		var _v0 = function ($) {
			return $.gamepadState;
		}(model);
		if (_v0.$ === 'RemappingGamepad') {
			var remapModelOld = _v0.a;
			var _v1 = A2($xarvh$elm_gamepad$Gamepad$Advanced$update, remapMsg, remapModelOld);
			var remapModelNew = _v1.a;
			var maybeUpdateUserMappings = _v1.b;
			return A2(
				$author$project$Main$remapGamepad_,
				maybeUpdateUserMappings,
				_Utils_update(
					model,
					{
						gamepadState: $author$project$Main$RemappingGamepad(remapModelNew)
					}));
		} else {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Timer$setTime = F2(
	function (timer, t) {
		return A2(
			$author$project$Timer$mapT,
			function (s) {
				return _Utils_update(
					s,
					{currentTime: t});
			},
			timer);
	});
var $author$project$Main$categoryJSON = function (cat) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(
					function ($) {
						return $.name;
					}(cat))),
				_Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$int(
					function ($) {
						return $.offset;
					}(cat)))
			]));
};
var $author$project$Main$gameJSON = function (game) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(
					function ($) {
						return $.name;
					}(game))),
				_Utils_Tuple2(
				'icon',
				A2(
					$elm$core$Maybe$withDefault,
					$elm$json$Json$Encode$null,
					A2(
						$elm$core$Maybe$map,
						$elm$json$Json$Encode$string,
						function ($) {
							return $.icon;
						}(game)))),
				_Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$int(
					function ($) {
						return $.offset;
					}(game)))
			]));
};
var $author$project$Main$segmentsJSON = $elm$json$Json$Encode$list(
	function (seg) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(
						function ($) {
							return $.name;
						}(seg))),
					_Utils_Tuple2(
					'icon',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2(
							$elm$core$Maybe$map,
							$elm$json$Json$Encode$string,
							function ($) {
								return $.icon;
							}(seg))))
				]));
	});
var $author$project$Main$splitsSave = F2(
	function (ms, s) {
		var _v0 = function ($) {
			return $.runTracker;
		}(s);
		if (_v0.$ === 'SingleCategory') {
			var r = _v0.a;
			var segs = $author$project$Main$segmentsJSON(
				A2(
					$elm$core$List$map,
					function ($) {
						return $.segment;
					},
					function ($) {
						return $.splits;
					}(r).upcoming));
			var game = function ($) {
				return $.game;
			}(r);
			var game_ = _Utils_update(
				game,
				{
					name: function ($) {
						return $.title;
					}(s)
				});
			var gameJ = $author$project$Main$gameJSON(game_);
			var cat = function ($) {
				return $.category;
			}(r);
			var cat_ = _Utils_update(
				cat,
				{
					name: function ($) {
						return $.subtitle;
					}(s)
				});
			var catJ = $author$project$Main$categoryJSON(cat_);
			var json = $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('NewSplits')),
						_Utils_Tuple2(
						'contents',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'title',
									$elm$json$Json$Encode$string(
										function ($) {
											return $.title;
										}(s))),
									_Utils_Tuple2(
									'subtitle',
									$elm$json$Json$Encode$string(
										function ($) {
											return $.subtitle;
										}(s))),
									_Utils_Tuple2('game', gameJ),
									_Utils_Tuple2('category', catJ),
									_Utils_Tuple2('segments', segs)
								])))
					]));
			var run = _Utils_update(
				r,
				{category: cat_, game: game_});
			return A2($author$project$Main$send, ms, json);
		} else {
			var rs = _v0.a;
			var categoryIDs = A2(
				$elm$json$Json$Encode$list,
				A2(
					$elm$core$Basics$composeL,
					A2(
						$elm$core$Basics$composeL,
						A2(
							$elm$core$Basics$composeL,
							$elm$json$Json$Encode$int,
							$elm$core$Maybe$withDefault(0)),
						function ($) {
							return $.entityID;
						}),
					function ($) {
						return $.category;
					}),
				function ($) {
					return $.upcoming;
				}(rs));
			var json = $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'tag',
						$elm$json$Json$Encode$string('NewMultiCategorySplits')),
						_Utils_Tuple2(
						'contents',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'title',
									$elm$json$Json$Encode$string(
										function ($) {
											return $.title;
										}(s))),
									_Utils_Tuple2('categoryList', categoryIDs)
								])))
					]));
			return A2($author$project$Main$send, ms, json);
		}
	});
var $ohanhi$keyboard$Keyboard$Backspace = {$: 'Backspace'};
var $ohanhi$keyboard$Keyboard$Clear = {$: 'Clear'};
var $ohanhi$keyboard$Keyboard$Copy = {$: 'Copy'};
var $ohanhi$keyboard$Keyboard$CrSel = {$: 'CrSel'};
var $ohanhi$keyboard$Keyboard$Cut = {$: 'Cut'};
var $ohanhi$keyboard$Keyboard$Delete = {$: 'Delete'};
var $ohanhi$keyboard$Keyboard$EraseEof = {$: 'EraseEof'};
var $ohanhi$keyboard$Keyboard$ExSel = {$: 'ExSel'};
var $ohanhi$keyboard$Keyboard$Insert = {$: 'Insert'};
var $ohanhi$keyboard$Keyboard$Paste = {$: 'Paste'};
var $ohanhi$keyboard$Keyboard$Redo = {$: 'Redo'};
var $ohanhi$keyboard$Keyboard$Undo = {$: 'Undo'};
var $ohanhi$keyboard$Keyboard$editingKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Backspace':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Backspace);
		case 'Clear':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Clear);
		case 'Copy':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Copy);
		case 'CrSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CrSel);
		case 'Cut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cut);
		case 'Delete':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Delete);
		case 'EraseEof':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EraseEof);
		case 'ExSel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ExSel);
		case 'Insert':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Insert);
		case 'Paste':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Paste);
		case 'Redo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Redo);
		case 'Undo':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Undo);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$F1 = {$: 'F1'};
var $ohanhi$keyboard$Keyboard$F10 = {$: 'F10'};
var $ohanhi$keyboard$Keyboard$F11 = {$: 'F11'};
var $ohanhi$keyboard$Keyboard$F12 = {$: 'F12'};
var $ohanhi$keyboard$Keyboard$F13 = {$: 'F13'};
var $ohanhi$keyboard$Keyboard$F14 = {$: 'F14'};
var $ohanhi$keyboard$Keyboard$F15 = {$: 'F15'};
var $ohanhi$keyboard$Keyboard$F16 = {$: 'F16'};
var $ohanhi$keyboard$Keyboard$F17 = {$: 'F17'};
var $ohanhi$keyboard$Keyboard$F18 = {$: 'F18'};
var $ohanhi$keyboard$Keyboard$F19 = {$: 'F19'};
var $ohanhi$keyboard$Keyboard$F2 = {$: 'F2'};
var $ohanhi$keyboard$Keyboard$F20 = {$: 'F20'};
var $ohanhi$keyboard$Keyboard$F3 = {$: 'F3'};
var $ohanhi$keyboard$Keyboard$F4 = {$: 'F4'};
var $ohanhi$keyboard$Keyboard$F5 = {$: 'F5'};
var $ohanhi$keyboard$Keyboard$F6 = {$: 'F6'};
var $ohanhi$keyboard$Keyboard$F7 = {$: 'F7'};
var $ohanhi$keyboard$Keyboard$F8 = {$: 'F8'};
var $ohanhi$keyboard$Keyboard$F9 = {$: 'F9'};
var $ohanhi$keyboard$Keyboard$functionKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'F1':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F1);
		case 'F2':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F2);
		case 'F3':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F3);
		case 'F4':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F4);
		case 'F5':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F5);
		case 'F6':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F6);
		case 'F7':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F7);
		case 'F8':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F8);
		case 'F9':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F9);
		case 'F10':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F10);
		case 'F11':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F11);
		case 'F12':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F12);
		case 'F13':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F13);
		case 'F14':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F14);
		case 'F15':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F15);
		case 'F16':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F16);
		case 'F17':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F17);
		case 'F18':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F18);
		case 'F19':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F19);
		case 'F20':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$F20);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$ChannelDown = {$: 'ChannelDown'};
var $ohanhi$keyboard$Keyboard$ChannelUp = {$: 'ChannelUp'};
var $ohanhi$keyboard$Keyboard$MediaFastForward = {$: 'MediaFastForward'};
var $ohanhi$keyboard$Keyboard$MediaPause = {$: 'MediaPause'};
var $ohanhi$keyboard$Keyboard$MediaPlay = {$: 'MediaPlay'};
var $ohanhi$keyboard$Keyboard$MediaPlayPause = {$: 'MediaPlayPause'};
var $ohanhi$keyboard$Keyboard$MediaRecord = {$: 'MediaRecord'};
var $ohanhi$keyboard$Keyboard$MediaRewind = {$: 'MediaRewind'};
var $ohanhi$keyboard$Keyboard$MediaStop = {$: 'MediaStop'};
var $ohanhi$keyboard$Keyboard$MediaTrackNext = {$: 'MediaTrackNext'};
var $ohanhi$keyboard$Keyboard$MediaTrackPrevious = {$: 'MediaTrackPrevious'};
var $ohanhi$keyboard$Keyboard$mediaKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'ChannelDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelDown);
		case 'ChannelUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ChannelUp);
		case 'MediaFastForward':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaFastForward);
		case 'MediaPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPause);
		case 'MediaPlay':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlay);
		case 'MediaPlayPause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaPlayPause);
		case 'MediaRecord':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRecord);
		case 'MediaRewind':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaRewind);
		case 'MediaStop':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaStop);
		case 'MediaTrackNext':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackNext);
		case 'MediaTrackPrevious':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MediaTrackPrevious);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Alt = {$: 'Alt'};
var $ohanhi$keyboard$Keyboard$AltGraph = {$: 'AltGraph'};
var $ohanhi$keyboard$Keyboard$CapsLock = {$: 'CapsLock'};
var $ohanhi$keyboard$Keyboard$Control = {$: 'Control'};
var $ohanhi$keyboard$Keyboard$Fn = {$: 'Fn'};
var $ohanhi$keyboard$Keyboard$FnLock = {$: 'FnLock'};
var $ohanhi$keyboard$Keyboard$Hyper = {$: 'Hyper'};
var $ohanhi$keyboard$Keyboard$Meta = {$: 'Meta'};
var $ohanhi$keyboard$Keyboard$NumLock = {$: 'NumLock'};
var $ohanhi$keyboard$Keyboard$ScrollLock = {$: 'ScrollLock'};
var $ohanhi$keyboard$Keyboard$Shift = {$: 'Shift'};
var $ohanhi$keyboard$Keyboard$Super = {$: 'Super'};
var $ohanhi$keyboard$Keyboard$Symbol = {$: 'Symbol'};
var $ohanhi$keyboard$Keyboard$SymbolLock = {$: 'SymbolLock'};
var $ohanhi$keyboard$Keyboard$modifierKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Alt':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Alt);
		case 'AltGraph':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AltGraph);
		case 'CapsLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CapsLock);
		case 'Control':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Control);
		case 'Fn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Fn);
		case 'FnLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$FnLock);
		case 'Hyper':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Hyper);
		case 'Meta':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Meta);
		case 'NumLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$NumLock);
		case 'ScrollLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ScrollLock);
		case 'Shift':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Shift);
		case 'Super':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'OS':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Super);
		case 'Symbol':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Symbol);
		case 'SymbolLock':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$SymbolLock);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$ArrowDown = {$: 'ArrowDown'};
var $ohanhi$keyboard$Keyboard$ArrowUp = {$: 'ArrowUp'};
var $ohanhi$keyboard$Keyboard$End = {$: 'End'};
var $ohanhi$keyboard$Keyboard$Home = {$: 'Home'};
var $ohanhi$keyboard$Keyboard$PageDown = {$: 'PageDown'};
var $ohanhi$keyboard$Keyboard$PageUp = {$: 'PageUp'};
var $ohanhi$keyboard$Keyboard$navigationKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'ArrowDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'ArrowLeft':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'ArrowRight':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'ArrowUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'Down':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowDown);
		case 'Left':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'Right':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowRight);
		case 'Up':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ArrowUp);
		case 'End':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$End);
		case 'Home':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Home);
		case 'PageDown':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageDown);
		case 'PageUp':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$PageUp);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$oneOf = F2(
	function (fns, key) {
		oneOf:
		while (true) {
			if (!fns.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var fn = fns.a;
				var rest = fns.b;
				var _v1 = fn(key);
				if (_v1.$ === 'Just') {
					var a = _v1.a;
					return $elm$core$Maybe$Just(a);
				} else {
					var $temp$fns = rest,
						$temp$key = key;
					fns = $temp$fns;
					key = $temp$key;
					continue oneOf;
				}
			}
		}
	});
var $ohanhi$keyboard$Keyboard$AppSwitch = {$: 'AppSwitch'};
var $ohanhi$keyboard$Keyboard$Call = {$: 'Call'};
var $ohanhi$keyboard$Keyboard$Camera = {$: 'Camera'};
var $ohanhi$keyboard$Keyboard$CameraFocus = {$: 'CameraFocus'};
var $ohanhi$keyboard$Keyboard$EndCall = {$: 'EndCall'};
var $ohanhi$keyboard$Keyboard$GoBack = {$: 'GoBack'};
var $ohanhi$keyboard$Keyboard$GoHome = {$: 'GoHome'};
var $ohanhi$keyboard$Keyboard$HeadsetHook = {$: 'HeadsetHook'};
var $ohanhi$keyboard$Keyboard$LastNumberRedial = {$: 'LastNumberRedial'};
var $ohanhi$keyboard$Keyboard$MannerMode = {$: 'MannerMode'};
var $ohanhi$keyboard$Keyboard$Notification = {$: 'Notification'};
var $ohanhi$keyboard$Keyboard$VoiceDial = {$: 'VoiceDial'};
var $ohanhi$keyboard$Keyboard$phoneKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'AppSwitch':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$AppSwitch);
		case 'Call':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Call);
		case 'Camera':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Camera);
		case 'CameraFocus':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$CameraFocus);
		case 'EndCall':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$EndCall);
		case 'GoBack':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoBack);
		case 'GoHome':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$GoHome);
		case 'HeadsetHook':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$HeadsetHook);
		case 'LastNumberRedial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$LastNumberRedial);
		case 'Notification':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Notification);
		case 'MannerMode':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$MannerMode);
		case 'VoiceDial':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$VoiceDial);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Again = {$: 'Again'};
var $ohanhi$keyboard$Keyboard$Attn = {$: 'Attn'};
var $ohanhi$keyboard$Keyboard$Cancel = {$: 'Cancel'};
var $ohanhi$keyboard$Keyboard$ContextMenu = {$: 'ContextMenu'};
var $ohanhi$keyboard$Keyboard$Execute = {$: 'Execute'};
var $ohanhi$keyboard$Keyboard$Find = {$: 'Find'};
var $ohanhi$keyboard$Keyboard$Finish = {$: 'Finish'};
var $ohanhi$keyboard$Keyboard$Help = {$: 'Help'};
var $ohanhi$keyboard$Keyboard$Pause = {$: 'Pause'};
var $ohanhi$keyboard$Keyboard$Play = {$: 'Play'};
var $ohanhi$keyboard$Keyboard$Props = {$: 'Props'};
var $ohanhi$keyboard$Keyboard$Select = {$: 'Select'};
var $ohanhi$keyboard$Keyboard$ZoomIn = {$: 'ZoomIn'};
var $ohanhi$keyboard$Keyboard$ZoomOut = {$: 'ZoomOut'};
var $ohanhi$keyboard$Keyboard$uiKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Again':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Again);
		case 'Attn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Attn);
		case 'Cancel':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Cancel);
		case 'ContextMenu':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ContextMenu);
		case 'Escape':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Escape);
		case 'Execute':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Execute);
		case 'Find':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Find);
		case 'Finish':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Finish);
		case 'Help':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Help);
		case 'Pause':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Pause);
		case 'Play':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Play);
		case 'Props':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Props);
		case 'Select':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Select);
		case 'ZoomIn':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomIn);
		case 'ZoomOut':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$ZoomOut);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$Enter = {$: 'Enter'};
var $ohanhi$keyboard$Keyboard$Tab = {$: 'Tab'};
var $ohanhi$keyboard$Keyboard$whitespaceKey = function (_v0) {
	var value = _v0.a;
	switch (value) {
		case 'Enter':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Enter);
		case 'Tab':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Tab);
		case 'Spacebar':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		case ' ':
			return $elm$core$Maybe$Just($ohanhi$keyboard$Keyboard$Spacebar);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $ohanhi$keyboard$Keyboard$anyKeyWith = function (charParser) {
	return $ohanhi$keyboard$Keyboard$oneOf(
		_List_fromArray(
			[$ohanhi$keyboard$Keyboard$whitespaceKey, charParser, $ohanhi$keyboard$Keyboard$modifierKey, $ohanhi$keyboard$Keyboard$navigationKey, $ohanhi$keyboard$Keyboard$editingKey, $ohanhi$keyboard$Keyboard$functionKey, $ohanhi$keyboard$Keyboard$uiKey, $ohanhi$keyboard$Keyboard$phoneKey, $ohanhi$keyboard$Keyboard$mediaKey]));
};
var $elm$core$String$toUpper = _String_toUpper;
var $ohanhi$keyboard$Keyboard$characterKeyUpper = function (_v0) {
	var value = _v0.a;
	return ($elm$core$String$length(value) === 1) ? $elm$core$Maybe$Just(
		$ohanhi$keyboard$Keyboard$Character(
			$elm$core$String$toUpper(value))) : $elm$core$Maybe$Nothing;
};
var $ohanhi$keyboard$Keyboard$anyKeyUpper = $ohanhi$keyboard$Keyboard$anyKeyWith($ohanhi$keyboard$Keyboard$characterKeyUpper);
var $ohanhi$keyboard$Keyboard$insert = F3(
	function (keyParser, rawKey, list) {
		var _v0 = keyParser(rawKey);
		if (_v0.$ === 'Just') {
			var key = _v0.a;
			return A2(
				$elm$core$List$cons,
				key,
				A2(
					$elm$core$List$filter,
					$elm$core$Basics$neq(key),
					list));
		} else {
			return list;
		}
	});
var $ohanhi$keyboard$Keyboard$remove = F3(
	function (keyParser, rawKey, list) {
		var _v0 = keyParser(rawKey);
		if (_v0.$ === 'Just') {
			var key = _v0.a;
			return A2(
				$elm$core$List$filter,
				$elm$core$Basics$neq(key),
				list);
		} else {
			return list;
		}
	});
var $ohanhi$keyboard$Keyboard$updateWithParser = F3(
	function (keyParser, msg, state) {
		if (msg.$ === 'Down') {
			var key = msg.a;
			return A3($ohanhi$keyboard$Keyboard$insert, keyParser, key, state);
		} else {
			var key = msg.a;
			return A3($ohanhi$keyboard$Keyboard$remove, keyParser, key, state);
		}
	});
var $ohanhi$keyboard$Keyboard$update = $ohanhi$keyboard$Keyboard$updateWithParser($ohanhi$keyboard$Keyboard$anyKeyUpper);
var $author$project$Main$updateSegmentName_ = F3(
	function (i, n, run) {
		var s = function ($) {
			return $.splits;
		}(run);
		return _Utils_update(
			run,
			{
				splits: _Utils_update(
					s,
					{
						upcoming: A3(
							$elm$core$List$foldl,
							F2(
								function (_v0, splitlist) {
									var j = _v0.a;
									var split = _v0.b;
									if (_Utils_eq(i, j)) {
										var seg = function ($) {
											return $.segment;
										}(split);
										return _Utils_ap(
											splitlist,
											_List_fromArray(
												[
													_Utils_update(
													split,
													{
														segment: _Utils_update(
															seg,
															{name: n})
													})
												]));
									} else {
										return _Utils_ap(
											splitlist,
											_List_fromArray(
												[split]));
									}
								}),
							_List_Nil,
							A2(
								$elm$core$List$indexedMap,
								$elm$core$Tuple$pair,
								function ($) {
									return $.upcoming;
								}(s)))
					})
			});
	});
var $author$project$Main$updateSegmentName = F2(
	function (i, n) {
		return $author$project$Timer$mapT(
			$author$project$Timer$mapR(
				A2($author$project$Main$updateSegmentName_, i, n)));
	});
var $author$project$Main$updateSelectedCategory_ = F4(
	function (cs, i, c, s) {
		var _v0 = function ($) {
			return $.runTracker;
		}(s);
		if (_v0.$ === 'SingleCategory') {
			return s;
		} else {
			var rs = _v0.a;
			var r = function ($) {
				return $.upcoming;
			}(rs);
			var m = $author$project$Timer$MultiCategory(
				_Utils_update(
					rs,
					{
						upcoming: A3(
							$elm$core$List$foldl,
							F2(
								function (_v1, runList) {
									var j = _v1.a;
									var run = _v1.b;
									if (_Utils_eq(i, j)) {
										var _v2 = $elm$core$List$head(
											A2(
												$elm$core$List$filter,
												function (cat) {
													return _Utils_eq(
														function ($) {
															return $.entityID;
														}(cat),
														$elm$core$Maybe$Just(c));
												},
												cs));
										if (_v2.$ === 'Nothing') {
											return _Utils_ap(
												runList,
												_List_fromArray(
													[run]));
										} else {
											var c_ = _v2.a;
											return _Utils_ap(
												runList,
												_List_fromArray(
													[
														_Utils_update(
														run,
														{category: c_})
													]));
										}
									} else {
										return _Utils_ap(
											runList,
											_List_fromArray(
												[run]));
									}
								}),
							_List_Nil,
							A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, r))
					}));
			return _Utils_update(
				s,
				{runTracker: m});
		}
	});
var $author$project$Main$updateSelectedCategory = F3(
	function (rss, i, c) {
		var cs = A3(
			$elm$core$List$foldl,
			F2(
				function (spec, categories) {
					if (spec.$ === 'SingleCategorySpec') {
						var r = spec.a;
						return A2(
							$elm$core$List$cons,
							function ($) {
								return $.category;
							}(r),
							categories);
					} else {
						var rs = spec.b;
						return _Utils_ap(
							A2(
								$elm$core$List$map,
								function ($) {
									return $.category;
								},
								rs),
							categories);
					}
				}),
			_List_Nil,
			rss);
		return $author$project$Timer$mapT(
			A3($author$project$Main$updateSelectedCategory_, cs, i, c));
	});
var $author$project$Main$updateSubtitle_ = F2(
	function (name, splits) {
		return _Utils_update(
			splits,
			{subtitle: name});
	});
var $author$project$Main$updateSubtitle = function (n) {
	return $author$project$Timer$mapT(
		$author$project$Main$updateSubtitle_(n));
};
var $author$project$Main$updateTitle_ = F2(
	function (name, splits) {
		return _Utils_update(
			splits,
			{title: name});
	});
var $author$project$Main$updateTitle = function (n) {
	return $author$project$Timer$mapT(
		$author$project$Main$updateTitle_(n));
};
var $xarvh$elm_gamepad$Gamepad$axisToButton = function (n) {
	return n > 0.6;
};
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $xarvh$elm_gamepad$Gamepad$mappingToOrigin = F2(
	function (destination, mapping) {
		return A2(
			$elm$core$Dict$get,
			$xarvh$elm_gamepad$Gamepad$digitalToString(destination),
			mapping);
	});
var $xarvh$elm_gamepad$Gamepad$reverseAxis = F2(
	function (isReverse, n) {
		return isReverse ? (-n) : n;
	});
var $xarvh$elm_gamepad$Gamepad$getAsBool = F3(
	function (destination, mapping, frame) {
		var _v0 = A2($xarvh$elm_gamepad$Gamepad$mappingToOrigin, destination, mapping);
		if (_v0.$ === 'Nothing') {
			return false;
		} else {
			if (_v0.a.b.$ === 'Axis') {
				var _v1 = _v0.a;
				var isReverse = _v1.a;
				var _v2 = _v1.b;
				var index = _v1.c;
				return $xarvh$elm_gamepad$Gamepad$axisToButton(
					A2(
						$xarvh$elm_gamepad$Gamepad$reverseAxis,
						isReverse,
						A2(
							$elm$core$Maybe$withDefault,
							0,
							A2($elm$core$Array$get, index, frame.axes))));
			} else {
				var _v3 = _v0.a;
				var isReverse = _v3.a;
				var _v4 = _v3.b;
				var index = _v3.c;
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$first,
						A2($elm$core$Array$get, index, frame.buttons)));
			}
		}
	});
var $xarvh$elm_gamepad$Gamepad$wasClicked = F2(
	function (_v0, digital) {
		var mapping = _v0.a;
		var currentFrame = _v0.b;
		var previousFrame = _v0.c;
		return (!A3($xarvh$elm_gamepad$Gamepad$getAsBool, digital, mapping, previousFrame)) && A3($xarvh$elm_gamepad$Gamepad$getAsBool, digital, mapping, currentFrame);
	});
var $author$project$Main$processGamepadEvent = F2(
	function (model, blob) {
		var msgs = A2(
			$elm$core$List$map,
			function (m) {
				return A2(
					$author$project$Main$fetchControl,
					m,
					function ($) {
						return $.timer;
					}(model));
			},
			A2(
				$elm$core$List$map,
				function (_v12) {
					var msg = _v12.a;
					return msg;
				},
				A2(
					$elm$core$List$concatMap,
					function (s) {
						return A2(
							$elm$core$List$filter,
							function (_v13) {
								var desc = _v13.b;
								return _Utils_eq(desc, s);
							},
							$author$project$Main$controlDescMap);
					},
					A2(
						$elm$core$List$concatMap,
						function (g) {
							return A3(
								$elm$core$List$foldl,
								F2(
									function (_v14, cs) {
										var s = _v14.a;
										var c = _v14.b;
										return A2($xarvh$elm_gamepad$Gamepad$wasClicked, g, c) ? A2($elm$core$List$cons, s, cs) : cs;
									}),
								_List_Nil,
								$author$project$Main$allMappableControls);
						},
						A3(
							$xarvh$elm_gamepad$Gamepad$Advanced$getGamepads,
							$author$project$Main$allMappableControls,
							function ($) {
								return $.userMappings;
							}(model),
							blob)))));
		var _v9 = A3(
			$elm$core$List$foldl,
			F2(
				function (msg, _v10) {
					var model_ = _v10.a;
					var cmds = _v10.b;
					return function (_v11) {
						var v = _v11.a;
						var c = _v11.b;
						return _Utils_Tuple2(
							v,
							_Utils_ap(
								cmds,
								_List_fromArray(
									[c])));
					}(
						A2($author$project$Main$update, msg, model_));
				}),
			_Utils_Tuple2(
				_Utils_update(
					model,
					{
						gamepadState: $author$project$Main$ActiveGamepad(blob)
					}),
				_List_Nil),
			msgs);
		var m_ = _v9.a;
		var c_ = _v9.b;
		return _Utils_Tuple2(
			m_,
			$elm$core$Platform$Cmd$batch(c_));
	});
var $author$project$Main$processKeyboardEvent = F2(
	function (model, keyMsg) {
		var keys = A2(
			$ohanhi$keyboard$Keyboard$update,
			keyMsg,
			function ($) {
				return $.keyboardStatus;
			}(model));
		var f = F2(
			function (key, ms) {
				return _Utils_ap(
					ms,
					A2(
						$elm$core$List$map,
						function (_v6) {
							var val = _v6.b;
							return val;
						},
						A2(
							$elm$core$List$filter,
							function (_v7) {
								var kb = _v7.a;
								var kbm = _v7.b;
								return _Utils_eq(kb, key) && (_Utils_eq(kbm, $author$project$Main$ToggleMainMenu) || _Utils_eq(
									function ($) {
										return $.menu;
									}(model),
									$author$project$Main$MenuHidden));
							},
							A2(
								$elm$core$List$map,
								function (_v8) {
									var kbf = _v8.a;
									var kbm = _v8.b;
									return _Utils_Tuple2(
										kbm,
										$author$project$Main$fetchControl(kbf)(
											function ($) {
												return $.timer;
											}(model)));
								},
								function ($) {
									return $.keyboardMap;
								}(model)))));
			});
		var diff = A2(
			$elm$core$List$filter,
			function (key) {
				return !A2(
					$elm$core$List$member,
					key,
					function ($) {
						return $.keyboardStatus;
					}(model));
			},
			keys);
		var msgs = A3($elm$core$List$foldl, f, _List_Nil, diff);
		var _v3 = A3(
			$elm$core$List$foldl,
			F2(
				function (msg, _v4) {
					var model_ = _v4.a;
					var cmds = _v4.b;
					return function (_v5) {
						var v = _v5.a;
						var c = _v5.b;
						return _Utils_Tuple2(
							v,
							_Utils_ap(
								cmds,
								_List_fromArray(
									[c])));
					}(
						A2($author$project$Main$update, msg, model_));
				}),
			_Utils_Tuple2(
				_Utils_update(
					model,
					{keyboardStatus: keys}),
				_List_Nil),
			msgs);
		var m_ = _v3.a;
		var c_ = _v3.b;
		return _Utils_Tuple2(
			m_,
			$elm$core$Platform$Cmd$batch(c_));
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'OpenSocket':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Websocket$open(url));
			case 'SendSocket':
				var value = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$author$project$Main$send,
						function ($) {
							return $.socket;
						}(model),
						value));
			case 'SocketOpened':
				var newsocket = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							socket: $elm$core$Maybe$Just(newsocket)
						}),
					A2($author$project$Websocket$send, newsocket, $author$project$Main$newClientJSON));
			case 'SocketNotOpened':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'SocketReceived':
				var data = msg.a;
				return A2($author$project$Main$processData, model, data);
			case 'KeyboardEvent':
				var keys = msg.a;
				return A2($author$project$Main$processKeyboardEvent, model, keys);
			case 'ToggleMainMenu':
				var _v1 = function ($) {
					return $.menu;
				}(model);
				if (_v1.$ === 'MenuVisible') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{menu: $author$project$Main$MenuHidden}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								menu: $author$project$Main$MenuVisible($author$project$Main$MainMenu)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'Tick':
				var t = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: A2(
								$author$project$Timer$setTime,
								function ($) {
									return $.timer;
								}(model),
								t)
						}),
					$elm$core$Platform$Cmd$none);
			case 'StartSplit':
				return A3($author$project$Main$timerControl, true, model, msg);
			case 'Reset':
				return A3($author$project$Main$timerControl, true, model, msg);
			case 'Unsplit':
				return A3($author$project$Main$timerControl, true, model, msg);
			case 'Skip':
				return A3($author$project$Main$timerControl, true, model, msg);
			case 'CloseSplits':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{menu: $author$project$Main$MenuHidden, timer: $author$project$Timer$empty}),
					A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						msg));
			case 'EditSplits':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							backupTimer: $elm$core$Maybe$Just(
								$author$project$Timer$reset(
									function ($) {
										return $.timer;
									}(model))),
							menu: $author$project$Main$MenuHidden,
							splitsMode: $author$project$Main$EditSplitsView,
							timer: $author$project$Timer$reset(
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'EditSplitsCancel':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							menu: $author$project$Main$MenuHidden,
							splitsMode: $author$project$Main$NormalSplitsView,
							timer: A2(
								$elm$core$Maybe$withDefault,
								$author$project$Timer$empty,
								function ($) {
									return $.backupTimer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSegmentName':
				var i = msg.a;
				var n = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: A3(
								$author$project$Main$updateSegmentName,
								i,
								n,
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSelectedGame':
				var i = msg.a;
				var c = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: A4(
								$author$project$Main$updateSelectedCategory,
								function ($) {
									return $.fullCategoryList;
								}(model),
								i,
								A2(
									$elm$core$Maybe$withDefault,
									-1,
									$elm$core$String$toInt(c)),
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateTitle':
				var n = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: A2(
								$author$project$Main$updateTitle,
								n,
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSubtitle':
				var n = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: A2(
								$author$project$Main$updateSubtitle,
								n,
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateMultiCategory':
				var m = msg.a;
				var timerContainer = m ? $author$project$Timer$emptyMulti : $author$project$Timer$empty;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editMulticategory: m, timer: timerContainer}),
					A2(
						$author$project$Main$send,
						function ($) {
							return $.socket;
						}(model),
						$author$project$Main$fullCategoryListRequestJSON));
			case 'AddSegment':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: $author$project$Main$addSegment(
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'AddGame':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							timer: $author$project$Main$addGame(
								function ($) {
									return $.timer;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'EditSplitsSave':
				return _Utils_Tuple2(
					model,
					A2(
						$author$project$Main$splitsSave,
						function ($) {
							return $.socket;
						}(model),
						$author$project$Timer$splitsFor(
							function ($) {
								return $.timer;
							}(model))));
			case 'ListGames':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							gameList: _List_Nil,
							menu: $author$project$Main$MenuVisible($author$project$Main$GameList)
						}),
					A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						msg));
			case 'ListCategories':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							categoryList: _List_Nil,
							menu: $author$project$Main$MenuVisible($author$project$Main$CategoryList)
						}),
					A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						msg));
			case 'LoadSplits':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{menu: $author$project$Main$MenuHidden}),
					A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						msg));
			case 'LoadMultiCategory':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{menu: $author$project$Main$MenuHidden}),
					A2(
						$author$project$Main$broadcastIntent,
						function ($) {
							return $.socket;
						}(model),
						msg));
			case 'InputConfig':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							menu: $author$project$Main$MenuVisible($author$project$Main$Config)
						}),
					$elm$core$Platform$Cmd$none);
			case 'RemapGamepadToggle':
				var gp = function () {
					var _v2 = function ($) {
						return $.gamepadState;
					}(model);
					if (_v2.$ === 'RemappingGamepad') {
						return $author$project$Main$Uninitialized;
					} else {
						return $author$project$Main$RemappingGamepad(
							$xarvh$elm_gamepad$Gamepad$Advanced$init($author$project$Main$allMappableControls));
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{gamepadState: gp}),
					$elm$core$Platform$Cmd$none);
			case 'GamepadMappingLoad':
				var serialized = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							userMappings: A2(
								$author$project$GamepadPort$fromString,
								serialized,
								function ($) {
									return $.userMappings;
								}(model))
						}),
					$elm$core$Platform$Cmd$none);
			case 'GamepadRemappingTool':
				var remap = msg.a;
				return A2($author$project$Main$remapGamepad, model, remap);
			case 'GamepadFrame':
				var blob = msg.a;
				return A2($author$project$Main$processGamepadEvent, model, blob);
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Main$CloseSplits = {$: 'CloseSplits'};
var $author$project$Main$EditSplits = {$: 'EditSplits'};
var $author$project$Main$InputConfig = {$: 'InputConfig'};
var $author$project$Main$ListCategories = function (a) {
	return {$: 'ListCategories', a: a};
};
var $author$project$Main$ListGames = {$: 'ListGames'};
var $author$project$Main$LoadMultiCategory = function (a) {
	return {$: 'LoadMultiCategory', a: a};
};
var $author$project$Main$RemapGamepadToggle = {$: 'RemapGamepadToggle'};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $xarvh$elm_gamepad$Gamepad$Translations$en = {
	cancelRemapping: 'Cancel remapping',
	customMapping: 'Custom mapping',
	idle: 'idle',
	map: 'Map',
	needsMapping: 'Needs mapping',
	noGamepadsDetected: 'No gamepads detected',
	press: 'Press:',
	pressAnyButtonToGoBack: 'Press any button to go back.',
	pressTheEscKeyToToggle: _Utils_Tuple3('Press the', 'Esc', 'key to toggle the gamepad configuration menu'),
	receivingSignal: 'Receiving signal',
	remap: 'Remap',
	remappingGamepad: function (id) {
		return 'Remapping Gamepad ' + $elm$core$String$fromInt(id);
	},
	remappingGamepadComplete: function (id) {
		return 'Remapping Gamepad ' + ($elm$core$String$fromInt(id) + ' complete.');
	},
	skipThisAction: 'Skip this action',
	standardMapping: 'Standard mapping'
};
var $xarvh$elm_gamepad$Gamepad$Translations$allTranslations = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('en', $xarvh$elm_gamepad$Gamepad$Translations$en),
			_Utils_Tuple2(
			'fr',
			{
				cancelRemapping: 'Annuler la configuration',
				customMapping: 'Configuration personnalisée',
				idle: 'inactif',
				map: 'Attribuer',
				needsMapping: 'Configuration nécessaire',
				noGamepadsDetected: 'Aucune manette détectée',
				press: 'Pressez :',
				pressAnyButtonToGoBack: 'Pressez n\'importe quelle touche pour revenir en arrière.',
				pressTheEscKeyToToggle: _Utils_Tuple3('Appuyez sur la touche', 'Echap', 'pour basculer le menu de configuration du gamepad'),
				receivingSignal: 'Réception d\'un signal',
				remap: 'Configurer',
				remappingGamepad: function (id) {
					return 'Configuration de la manette ' + $elm$core$String$fromInt(id);
				},
				remappingGamepadComplete: function (id) {
					return 'Configuration de la manette ' + ($elm$core$String$fromInt(id) + ' terminée.');
				},
				skipThisAction: 'Passer cette action',
				standardMapping: 'Configuration standard'
			})
		]));
var $xarvh$elm_gamepad$Gamepad$Advanced$cssStyle = A2(
	$elm$core$String$join,
	'\n',
	_List_fromArray(
		['.elm-gamepad-mapping-unavailable { color: red; }', '.elm-gamepad-mapping-available { color: green; }', '.elm-gamepad-gamepad-index::before { content: \'Gamepad \'; }', '.elm-gamepad-gamepad-index::after { content: \': \'; }', '.elm-gamepad-remapping-skip { margin-top: 0.5em; }', '.elm-gamepad-remapping-cancel { margin-top: 0.5em; }']));
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $xarvh$elm_gamepad$Gamepad$Translations$getPrimarySubtag = function (tag) {
	return A2(
		$elm$core$Maybe$withDefault,
		tag,
		$elm$core$List$head(
			A2($elm$core$String$split, '-', tag)));
};
var $xarvh$elm_gamepad$Gamepad$Translations$pickTranslation = F2(
	function (languages, dict) {
		return A2(
			$xarvh$elm_gamepad$Gamepad$Translations$pickTranslation_,
			_Utils_ap(
				languages,
				A2($elm$core$List$map, $xarvh$elm_gamepad$Gamepad$Translations$getPrimarySubtag, languages)),
			dict);
	});
var $xarvh$elm_gamepad$Gamepad$Translations$pickTranslation_ = F2(
	function (languages, translations) {
		if (!languages.b) {
			return $xarvh$elm_gamepad$Gamepad$Translations$en;
		} else {
			var l = languages.a;
			var ls = languages.b;
			var _v1 = A2($elm$core$Dict$get, l, translations);
			if (_v1.$ === 'Just') {
				var t = _v1.a;
				return t;
			} else {
				return A2($xarvh$elm_gamepad$Gamepad$Translations$pickTranslation, ls, translations);
			}
		}
	});
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $xarvh$elm_gamepad$Gamepad$Advanced$OnStartRemapping = F2(
	function (a, b) {
		return {$: 'OnStartRemapping', a: a, b: b};
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $xarvh$elm_gamepad$Gamepad$getIndex = function (_v0) {
	var mapping = _v0.a;
	var currentFrame = _v0.b;
	var previousFrame = _v0.c;
	return currentFrame.index;
};
var $xarvh$elm_gamepad$Gamepad$Advanced$findIndex = F2(
	function (index, pads) {
		return A2(
			$elm_community$list_extra$List$Extra$find,
			function (pad) {
				return _Utils_eq(
					$xarvh$elm_gamepad$Gamepad$getIndex(pad),
					index);
			},
			pads);
	});
var $xarvh$elm_gamepad$Gamepad$isPressed = F2(
	function (_v0, digital) {
		var mapping = _v0.a;
		var currentFrame = _v0.b;
		var previousFrame = _v0.c;
		return A3($xarvh$elm_gamepad$Gamepad$getAsBool, digital, mapping, currentFrame);
	});
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$span = _VirtualDom_node('span');
var $xarvh$elm_gamepad$Gamepad$Advanced$viewGamepadFrame = F5(
	function (controls, userMappings, blob, translation, _v0) {
		var id = _v0.id;
		var index = _v0.index;
		var maybeGamepadWithoutConfig = A2(
			$xarvh$elm_gamepad$Gamepad$Advanced$findIndex,
			index,
			A3($xarvh$elm_gamepad$Gamepad$Advanced$getGamepads, controls, $xarvh$elm_gamepad$Gamepad$Advanced$emptyUserMappings, blob));
		var maybeGamepad = A2(
			$xarvh$elm_gamepad$Gamepad$Advanced$findIndex,
			index,
			A3($xarvh$elm_gamepad$Gamepad$Advanced$getGamepads, controls, userMappings, blob));
		var _v1 = function () {
			if (maybeGamepad.$ === 'Nothing') {
				return {
					buttonLabel: translation.map,
					signal: _Utils_eq(
						A2($xarvh$elm_gamepad$Gamepad$Advanced$estimateOrigin, blob, index),
						$elm$core$Maybe$Nothing) ? translation.idle : translation.receivingSignal,
					status: translation.needsMapping,
					symbolClass: 'elm-gamepad-mapping-unavailable',
					symbolFace: '✘'
				};
			} else {
				var gamepad = maybeGamepad.a;
				return {
					buttonLabel: translation.remap,
					signal: A2(
						$elm$core$Maybe$withDefault,
						translation.idle,
						A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$first,
							A2(
								$elm_community$list_extra$List$Extra$find,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Tuple$second,
									$xarvh$elm_gamepad$Gamepad$isPressed(gamepad)),
								controls))),
					status: _Utils_eq(maybeGamepad, maybeGamepadWithoutConfig) ? translation.standardMapping : translation.customMapping,
					symbolClass: 'elm-gamepad-mapping-available',
					symbolFace: '✔'
				};
			}
		}();
		var symbolFace = _v1.symbolFace;
		var symbolClass = _v1.symbolClass;
		var buttonLabel = _v1.buttonLabel;
		var status = _v1.status;
		var signal = _v1.signal;
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('elm-gamepad-gamepad-list-item')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-gamepad-gamepad-index')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(index))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(symbolClass)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(symbolFace)
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-gamepad-mapping-state-text')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(status)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-gamepad-current-action-text')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(signal)
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-gamepad-remap-button'),
							$elm$html$Html$Events$onClick(
							A2($xarvh$elm_gamepad$Gamepad$Advanced$OnStartRemapping, id, index))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(buttonLabel)
						]))
				]));
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$OnCancel = {$: 'OnCancel'};
var $xarvh$elm_gamepad$Gamepad$Advanced$OnSkip = function (a) {
	return {$: 'OnSkip', a: a};
};
var $xarvh$elm_gamepad$Gamepad$Advanced$viewRemapping = F3(
	function (controls, remapping, translation) {
		var statusClass = $elm$html$Html$Attributes$class('elm-gamepad-remapping-tellsUserWhatIsHappening');
		var instructionClass = $elm$html$Html$Attributes$class('elm-gamepad-remapping-tellsUserWhatToDo');
		var _v0 = A2($xarvh$elm_gamepad$Gamepad$Advanced$nextUnmappedAction, controls, remapping);
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-gamepad-remapping-complete')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[statusClass]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								translation.remappingGamepadComplete(remapping.index))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[instructionClass]),
						_List_fromArray(
							[
								$elm$html$Html$text(translation.pressAnyButtonToGoBack)
							]))
					]));
		} else {
			var _v1 = _v0.a;
			var actionName = _v1.a;
			var destination = _v1.b;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-gamepad-remapping')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[statusClass]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								translation.remappingGamepad(remapping.index))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[instructionClass]),
						_List_fromArray(
							[
								$elm$html$Html$text(translation.press)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('elm-gamepad-remapping-action-name')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(actionName)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('elm-gamepad-remapping-skip')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick(
										$xarvh$elm_gamepad$Gamepad$Advanced$OnSkip(destination))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(translation.skipThisAction)
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('elm-gamepad-remapping-cancel')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($xarvh$elm_gamepad$Gamepad$Advanced$OnCancel)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(translation.cancelRemapping)
									]))
							]))
					]));
		}
	});
var $xarvh$elm_gamepad$Gamepad$Advanced$view = F2(
	function (db, _v0) {
		var model = _v0.a;
		var _v1 = model.blob;
		var currentBlobFrame = _v1.a;
		var previousBlobFrame = _v1.b;
		var env = _v1.c;
		var translation = A2($xarvh$elm_gamepad$Gamepad$Translations$pickTranslation, env.languages, $xarvh$elm_gamepad$Gamepad$Translations$allTranslations);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('elm-gamepad')
				]),
			_List_fromArray(
				[
					function () {
					var _v2 = model.maybeRemapping;
					if (_v2.$ === 'Just') {
						var remapping = _v2.a;
						return A3($xarvh$elm_gamepad$Gamepad$Advanced$viewRemapping, model.controls, remapping, translation);
					} else {
						var _v3 = currentBlobFrame.gamepads;
						if (!_v3.b) {
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('elm-gamepad-no-gamepads')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(translation.noGamepadsDetected)
									]));
						} else {
							var gamepadFrames = _v3;
							return A2(
								$elm$html$Html$ul,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('elm-gamepad-gamepad-list')
									]),
								A2(
									$elm$core$List$map,
									A4($xarvh$elm_gamepad$Gamepad$Advanced$viewGamepadFrame, model.controls, db, model.blob, translation),
									gamepadFrames));
						}
					}
				}(),
					A3(
					$elm$html$Html$node,
					'style',
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text($xarvh$elm_gamepad$Gamepad$Advanced$cssStyle)
						]))
				]));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $author$project$Main$viewDigital = F2(
	function (gamepad, _v0) {
		var name = _v0.a;
		var digital = _v0.b;
		var s = function () {
			var _v1 = A2($xarvh$elm_gamepad$Gamepad$isPressed, gamepad, digital);
			if (_v1) {
				return 'True';
			} else {
				return 'False';
			}
		}();
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(s + (' <- ' + name))
				]));
	});
var $author$project$Main$viewGamepad = function (gamepad) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Gamepad ' + $elm$core$String$fromInt(
							$xarvh$elm_gamepad$Gamepad$getIndex(gamepad)))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				A2(
					$elm$core$List$map,
					$author$project$Main$viewDigital(gamepad),
					$author$project$Main$allMappableControls))
			]));
};
var $author$project$Main$viewGamepads = F2(
	function (model, blob) {
		var views = A2(
			$elm$core$List$map,
			$author$project$Main$viewGamepad,
			A3(
				$xarvh$elm_gamepad$Gamepad$Advanced$getGamepads,
				$author$project$Main$allMappableControls,
				function ($) {
					return $.userMappings;
				}(model),
				blob));
		return ($elm$core$List$length(views) > 0) ? A2($elm$html$Html$div, _List_Nil, views) : A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Can\'t find any gamepad! =(')
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('(The browser won\'t tell me they are there unless you press some button first, so maybe try that)')
						]))
				]));
	});
var $author$project$Main$menuView_ = F2(
	function (model, nav) {
		switch (nav.$) {
			case 'MainMenu':
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('menu-button'),
								$elm$html$Html$Events$onClick($author$project$Main$ListGames)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Load Splits')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('menu-button'),
								$elm$html$Html$Events$onClick($author$project$Main$EditSplits)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Edit Splits')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('menu-button'),
								$elm$html$Html$Events$onClick($author$project$Main$CloseSplits)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Close Splits')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('menu-button'),
								$elm$html$Html$Events$onClick($author$project$Main$InputConfig)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Edit Bindings')
							]))
					]);
			case 'GameList':
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (g) {
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('menu-button'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$ListCategories(
											function ($) {
												return $.entityID;
											}(g)))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										function ($) {
											return $.name;
										}(g))
									]));
						},
						function ($) {
							return $.gameList;
						}(model)),
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('---')
									]))
							]),
						A2(
							$elm$core$List$map,
							function (c) {
								return A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('menu-button'),
											$elm$html$Html$Events$onClick(
											$author$project$Main$LoadMultiCategory(
												function ($) {
													return $.entityID;
												}(c)))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function ($) {
												return $.name;
											}(c))
										]));
							},
							function ($) {
								return $.multiCategoryList;
							}(model))));
			case 'CategoryList':
				return A2(
					$elm$core$List$map,
					function (c) {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('menu-button'),
									$elm$html$Html$Events$onClick(
									$author$project$Main$LoadSplits(
										function ($) {
											return $.entityID;
										}(c)))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									function ($) {
										return $.name;
									}(c))
								]));
					},
					function ($) {
						return $.categoryList;
					}(model));
			default:
				var _v1 = function ($) {
					return $.gamepadState;
				}(model);
				switch (_v1.$) {
					case 'Uninitialized':
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Awaiting Gamepad input')
									]))
							]);
					case 'ActiveGamepad':
						var b = _v1.a;
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('menu-button'),
										$elm$html$Html$Events$onClick($author$project$Main$RemapGamepadToggle)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Remap Input')
									])),
								A2($elm$html$Html$div, _List_Nil, _List_Nil),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2($author$project$Main$viewGamepads, model, b)
									]))
							]);
					default:
						var m = _v1.a;
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('menu-button'),
										$elm$html$Html$Events$onClick($author$project$Main$RemapGamepadToggle)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Close Remap Tool')
									])),
								A2($elm$html$Html$div, _List_Nil, _List_Nil),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$map,
										$author$project$Main$GamepadRemappingTool,
										A2(
											$xarvh$elm_gamepad$Gamepad$Advanced$view,
											function ($) {
												return $.userMappings;
											}(model),
											m))
									]))
							]);
				}
		}
	});
var $author$project$Main$menuView = function (model) {
	var _v0 = function ($) {
		return $.menu;
	}(model);
	if (_v0.$ === 'MenuHidden') {
		return _List_Nil;
	} else {
		var nav = _v0.a;
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('modal-background')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('modal')
					]),
				A2($author$project$Main$menuView_, model, nav))
			]);
	}
};
var $author$project$Main$AddGame = {$: 'AddGame'};
var $author$project$Main$AddSegment = {$: 'AddSegment'};
var $author$project$Main$EditSplitsCancel = {$: 'EditSplitsCancel'};
var $author$project$Main$EditSplitsSave = {$: 'EditSplitsSave'};
var $author$project$Main$UpdateMultiCategory = function (a) {
	return {$: 'UpdateMultiCategory', a: a};
};
var $author$project$Main$UpdateSubtitle = function (a) {
	return {$: 'UpdateSubtitle', a: a};
};
var $author$project$Main$UpdateTitle = function (a) {
	return {$: 'UpdateTitle', a: a};
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Main$UpdateSelectedGame = F2(
	function (a, b) {
		return {$: 'UpdateSelectedGame', a: a, b: b};
	});
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$categoriesOptions = F2(
	function (active, rs) {
		if (rs.$ === 'MultiCategorySpec') {
			return A2(
				$elm$html$Html$option,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Error: Multi-category run')
					]));
		} else {
			var r = rs.a;
			var cid = function () {
				var _v1 = function ($) {
					return $.category;
				}(r).entityID;
				if (_v1.$ === 'Just') {
					var i = _v1.a;
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$value(
							$elm$core$String$fromInt(i)),
							$elm$html$Html$Attributes$selected(
							_Utils_eq(
								active,
								function ($) {
									return $.category;
								}(r).entityID))
						]);
				} else {
					return _List_Nil;
				}
			}();
			return A2(
				$elm$html$Html$option,
				cid,
				_List_fromArray(
					[
						$elm$html$Html$text(
						function ($) {
							return $.game;
						}(r).name + (' - ' + function ($) {
							return $.category;
						}(r).name))
					]));
		}
	});
var $elm$html$Html$select = _VirtualDom_node('select');
var $author$project$Main$categoriesEdit = F3(
	function (categories, _v0, hs) {
		var i = _v0.a;
		var r = _v0.b;
		var name = function ($) {
			return $.game;
		}(r).name + (' - ' + function ($) {
			return $.category;
		}(r).name);
		return _Utils_ap(
			hs,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('split-name')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$select,
									_List_fromArray(
										[
											$elm$html$Html$Events$onInput(
											$author$project$Main$UpdateSelectedGame(i))
										]),
									A2(
										$elm$core$List$map,
										$author$project$Main$categoriesOptions(
											function ($) {
												return $.category;
											}(r).entityID),
										categories))
								]))
						]))
				]));
	});
var $author$project$Main$UpdateSegmentName = F2(
	function (a, b) {
		return {$: 'UpdateSegmentName', a: a, b: b};
	});
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$splitsEdit_ = F2(
	function (_v0, hs) {
		var i = _v0.a;
		var s = _v0.b;
		var name = function ($) {
			return $.segment;
		}(s).name;
		return _Utils_ap(
			hs,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('split-name')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('text'),
											$elm$html$Html$Attributes$placeholder(
											'Segment #' + $elm$core$String$fromInt(1 + i)),
											$elm$html$Html$Attributes$value(name),
											$elm$html$Html$Events$onInput(
											$author$project$Main$UpdateSegmentName(i))
										]),
									_List_Nil)
								]))
						]))
				]));
	});
var $author$project$Main$splitsEdit = F2(
	function (categories, splits) {
		var _v0 = function ($) {
			return $.runTracker;
		}(splits);
		if (_v0.$ === 'SingleCategory') {
			var r = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				A3(
					$elm$core$List$foldl,
					$author$project$Main$splitsEdit_,
					_List_Nil,
					A2(
						$elm$core$List$indexedMap,
						$elm$core$Tuple$pair,
						A2(
							$elm$core$Basics$composeL,
							function ($) {
								return $.upcoming;
							},
							function ($) {
								return $.splits;
							})(r))));
		} else {
			var rs = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				A3(
					$elm$core$List$foldl,
					$author$project$Main$categoriesEdit(categories),
					_List_Nil,
					A2(
						$elm$core$List$indexedMap,
						$elm$core$Tuple$pair,
						function ($) {
							return $.upcoming;
						}(rs))));
		}
	});
var $author$project$Main$timerViewEdit = F3(
	function (categories, multicategory, timer) {
		var splits = $author$project$Timer$splitsFor(timer);
		var header = multicategory ? _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-multicategory')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('checkbox'),
								$elm$html$Html$Attributes$checked(multicategory),
								$elm$html$Html$Events$onClick(
								$author$project$Main$UpdateMultiCategory(false))
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-title')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('text'),
								$elm$html$Html$Attributes$placeholder('Title'),
								$elm$html$Html$Attributes$value(
								function ($) {
									return $.title;
								}(splits)),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateTitle)
							]),
						_List_Nil)
					]))
			]) : _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-multicategory')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('checkbox'),
								$elm$html$Html$Attributes$checked(multicategory),
								$elm$html$Html$Events$onClick(
								$author$project$Main$UpdateMultiCategory(true))
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-title')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('text'),
								$elm$html$Html$Attributes$placeholder('Title'),
								$elm$html$Html$Attributes$value(
								function ($) {
									return $.title;
								}(splits)),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateTitle)
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-subtitle')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('text'),
								$elm$html$Html$Attributes$placeholder('Category'),
								$elm$html$Html$Attributes$value(
								function ($) {
									return $.subtitle;
								}(splits)),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateSubtitle)
							]),
						_List_Nil)
					]))
			]);
		var footer = multicategory ? _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-edit-add')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$AddGame)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Add Category')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-edit-options')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$EditSplitsCancel)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('[ Cancel ]')
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$EditSplitsSave)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('[ Save ]')
							]))
					]))
			]) : _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-edit-add')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$AddSegment)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Add Segment')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('timer-edit-options')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$EditSplitsCancel)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('[ Cancel ]')
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$EditSplitsSave)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('[ Save ]')
							]))
					]))
			]);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('timer-container')
				]),
			_Utils_ap(
				header,
				_Utils_ap(
					_List_fromArray(
						[
							A2($author$project$Main$splitsEdit, categories, splits)
						]),
					footer)));
	});
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $author$project$Main$showD = F3(
	function (showSign, classes, d) {
		var hoursEmpty = !function ($) {
			return $.hours;
		}(d);
		var minutesEmpty = (!function ($) {
			return $.minutes;
		}(d)) && hoursEmpty;
		return A2(
			$elm$html$Html$span,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(classes)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-sign', true),
									_Utils_Tuple2('time-show-sign', showSign)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function ($) {
								return $.sign;
							}(d))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-hours', true),
									_Utils_Tuple2('time-empty', hoursEmpty)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(
								function ($) {
									return $.hours;
								}(d)))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-separator', true),
									_Utils_Tuple2('time-hours-separator', true),
									_Utils_Tuple2('time-empty', hoursEmpty)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(':')
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-minutes', true),
									_Utils_Tuple2('time-empty', minutesEmpty)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3(
								$elm$core$String$padLeft,
								2,
								_Utils_chr('0'),
								$elm$core$String$fromInt(
									function ($) {
										return $.minutes;
									}(d))))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-separator', true),
									_Utils_Tuple2('time-minutes-separator', true),
									_Utils_Tuple2('time-empty', minutesEmpty)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(':')
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-seconds', true)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3(
								$elm$core$String$padLeft,
								2,
								_Utils_chr('0'),
								$elm$core$String$fromInt(
									function ($) {
										return $.seconds;
									}(d))))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-separator', true),
									_Utils_Tuple2('time-seconds-separator', true)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('.')
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('time-millis', true)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3(
								$elm$core$String$padLeft,
								2,
								_Utils_chr('0'),
								$elm$core$String$fromInt(
									(function ($) {
										return $.millis;
									}(d) / 10) | 0)))
						]))
				]));
	});
var $author$project$Main$currentSum_ = F2(
	function (split, _v0) {
		var sum = _v0.a;
		var next = A2(
			$elm$core$Maybe$withDefault,
			sum,
			function ($) {
				return $.segment;
			}(split).pb);
		var single = A2(
			$elm$core$Maybe$map,
			function (x) {
				return x - next;
			},
			function ($) {
				return $.segment;
			}(split).pb);
		return _Utils_Tuple2(next, single);
	});
var $author$project$Main$currentSum = function (splits) {
	return A3(
		$elm$core$List$foldl,
		$author$project$Main$currentSum_,
		_Utils_Tuple2(0, $elm$core$Maybe$Nothing),
		splits);
};
var $author$project$Main$timeStatus = function (timer) {
	var _v0 = $author$project$Timer$runFor(timer);
	if (_v0.$ === 'Nothing') {
		return _List_fromArray(
			[
				_Utils_Tuple2('neutral', true)
			]);
	} else {
		var r = _v0.a;
		var s = function ($) {
			return $.splits;
		}(r);
		var currentTime_ = $elm$time$Time$posixToMillis(
			$author$project$Timer$splitsFor(timer).currentTime);
		var currentTime = A2(
			$elm$core$Maybe$withDefault,
			currentTime_,
			A2(
				$elm$core$Maybe$map,
				function (t) {
					return currentTime_ - $elm$time$Time$posixToMillis(t);
				},
				function ($) {
					return $.runStarted;
				}(r)));
		var _v1 = function ($) {
			return $.current;
		}(s);
		if (_v1.$ === 'Nothing') {
			var _v2 = $elm$core$List$reverse(
				function ($) {
					return $.previous;
				}(s));
			if (!_v2.b) {
				return _List_fromArray(
					[
						_Utils_Tuple2('neutral', true)
					]);
			} else {
				var x = _v2.a;
				var xs = _v2.b;
				var finalTime = A2(
					$elm$core$Maybe$withDefault,
					currentTime,
					A3(
						$elm$core$Maybe$map2,
						F2(
							function (t1, t2) {
								return $elm$time$Time$posixToMillis(t1) - $elm$time$Time$posixToMillis(t2);
							}),
						function ($) {
							return $.runEnded;
						}(r),
						function ($) {
							return $.runStarted;
						}(r)));
				var _v3 = $author$project$Main$currentSum(
					$elm$core$List$reverse(
						A2($elm$core$List$cons, x, xs)));
				var sum = _v3.a;
				var single = _v3.b;
				if (single.$ === 'Just') {
					var singleTime = single.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'ahead',
							_Utils_cmp(sum, finalTime) > -1),
							_Utils_Tuple2(
							'behind',
							_Utils_cmp(sum, finalTime) < 0),
							_Utils_Tuple2(
							'gaining',
							_Utils_cmp(
								singleTime,
								A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$elm$time$Time$posixToMillis,
										function ($) {
											return $.endTime;
										}(x))) - A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$elm$time$Time$posixToMillis,
										function ($) {
											return $.runStarted;
										}(r)))) < 1),
							_Utils_Tuple2(
							'losing',
							_Utils_cmp(
								singleTime,
								A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$elm$time$Time$posixToMillis,
										function ($) {
											return $.endTime;
										}(x))) - A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$elm$time$Time$posixToMillis,
										function ($) {
											return $.runStarted;
										}(r)))) > 0)
						]);
				} else {
					return _List_fromArray(
						[
							_Utils_Tuple2('ahead', true),
							_Utils_Tuple2('gaining', true)
						]);
				}
			}
		} else {
			var x = _v1.a;
			var _v5 = $author$project$Main$currentSum(
				_Utils_ap(
					function ($) {
						return $.previous;
					}(s),
					_List_fromArray(
						[x])));
			var sum = _v5.a;
			var single = _v5.b;
			if (single.$ === 'Just') {
				var singleTime = single.a;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'ahead',
						_Utils_cmp(sum, currentTime) > -1),
						_Utils_Tuple2(
						'behind',
						_Utils_cmp(sum, currentTime) < 0),
						_Utils_Tuple2(
						'gaining',
						_Utils_cmp(singleTime, currentTime) > -1),
						_Utils_Tuple2(
						'losing',
						_Utils_cmp(singleTime, currentTime) < 0)
					]);
			} else {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						'ahead',
						_Utils_cmp(sum, currentTime) > -1),
						_Utils_Tuple2(
						'behind',
						_Utils_cmp(sum, currentTime) < 0),
						_Utils_Tuple2(
						'gaining',
						_Utils_cmp(
							sum,
							A2(
								$elm$core$Maybe$withDefault,
								0,
								A2(
									$elm$core$Maybe$map,
									$elm$time$Time$posixToMillis,
									function ($) {
										return $.endTime;
									}(x))) - A2(
								$elm$core$Maybe$withDefault,
								0,
								A2(
									$elm$core$Maybe$map,
									$elm$time$Time$posixToMillis,
									function ($) {
										return $.runStarted;
									}(r)))) < 1),
						_Utils_Tuple2(
						'losing',
						_Utils_cmp(
							sum,
							A2(
								$elm$core$Maybe$withDefault,
								0,
								A2(
									$elm$core$Maybe$map,
									$elm$time$Time$posixToMillis,
									function ($) {
										return $.endTime;
									}(x))) - A2(
								$elm$core$Maybe$withDefault,
								0,
								A2(
									$elm$core$Maybe$map,
									$elm$time$Time$posixToMillis,
									function ($) {
										return $.runStarted;
									}(r)))) > 0)
					]);
			}
		}
	}
};
var $author$project$Main$Displayable = F5(
	function (hours, minutes, seconds, millis, sign) {
		return {hours: hours, millis: millis, minutes: minutes, seconds: seconds, sign: sign};
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Main$unitize = function (t_) {
	var t = $elm$core$Basics$abs(t_);
	var sign = (t_ < 0) ? '+' : '-';
	var seconds = A2($elm$core$Basics$modBy, 60, (t / 1000) | 0);
	var minutes = A2($elm$core$Basics$modBy, 60, (t / (1000 * 60)) | 0);
	var millis = A2($elm$core$Basics$modBy, 1000, t);
	var hours = A2($elm$core$Basics$modBy, 60, (t / ((1000 * 60) * 60)) | 0);
	return A5($author$project$Main$Displayable, hours, minutes, seconds, millis, sign);
};
var $author$project$Main$categoryTimer = function (timer) {
	var splits = $author$project$Timer$splitsFor(timer);
	var currentTime_ = function ($) {
		return $.currentTime;
	}(splits);
	var runStarted = $elm$time$Time$posixToMillis(
		A2(
			$elm$core$Maybe$withDefault,
			currentTime_,
			A2(
				$elm$core$Maybe$withDefault,
				$elm$core$Maybe$Nothing,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.runStarted;
					},
					$author$project$Timer$runFor(timer)))));
	var currentTime = $elm$time$Time$posixToMillis(currentTime_);
	var diffTime = function () {
		if (timer.$ === 'Finished') {
			var _v1 = $author$project$Timer$runFor(timer);
			if (_v1.$ === 'Nothing') {
				return (-1) * (currentTime - runStarted);
			} else {
				var r = _v1.a;
				return $elm$time$Time$posixToMillis(
					A2(
						$elm$core$Maybe$withDefault,
						currentTime_,
						function ($) {
							return $.runEnded;
						}(r))) - runStarted;
			}
		} else {
			return currentTime - runStarted;
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('main-timer'),
				$elm$html$Html$Attributes$classList(
				$author$project$Main$timeStatus(timer))
			]),
		_List_fromArray(
			[
				A3(
				$author$project$Main$showD,
				false,
				_List_Nil,
				$author$project$Main$unitize(diffTime))
			]));
};
var $author$project$Timer$statusTags = F4(
	function (indiv, running, singleTime, currentTime) {
		if (singleTime.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var st = singleTime.a;
			var _v1 = function ($) {
				return $.gold;
			}(indiv);
			if (_v1.$ === 'Nothing') {
				return _List_fromArray(
					[
						_Utils_Tuple2('gaining', true),
						_Utils_Tuple2('ahead', true)
					]);
			} else {
				var g = _v1.a;
				if (_Utils_cmp(g, st) > 0) {
					return _List_fromArray(
						[
							_Utils_Tuple2('gold', true)
						]);
				} else {
					var _v2 = function ($) {
						return $.pb;
					}(indiv);
					if (_v2.$ === 'Just') {
						var pb = _v2.a;
						var losing = _Utils_cmp(pb, st) < 0;
						var gaining = _Utils_cmp(st, pb) < 1;
						var _v3 = function ($) {
							return $.pb;
						}(running);
						if (_v3.$ === 'Nothing') {
							return _List_fromArray(
								[
									_Utils_Tuple2('gaining', gaining),
									_Utils_Tuple2('losing', losing),
									_Utils_Tuple2('ahead', true)
								]);
						} else {
							var r = _v3.a;
							if (currentTime.$ === 'Nothing') {
								return _List_fromArray(
									[
										_Utils_Tuple2('gaining', gaining),
										_Utils_Tuple2('losing', losing),
										_Utils_Tuple2('ahead', true)
									]);
							} else {
								var ct = currentTime.a;
								return _List_fromArray(
									[
										_Utils_Tuple2('gaining', gaining),
										_Utils_Tuple2('losing', losing),
										_Utils_Tuple2(
										'ahead',
										_Utils_cmp(ct, r) < 1),
										_Utils_Tuple2(
										'behind',
										_Utils_cmp(ct, r) > 0)
									]);
							}
						}
					} else {
						return _List_Nil;
					}
				}
			}
		}
	});
var $author$project$Timer$aggregateRun_ = F3(
	function (now, currentSplit, _v0) {
		var _v1 = _v0.a;
		var sums = _v1.a;
		var last = _v1.b;
		var lastTime = _v1.c;
		var segs = _v0.b;
		var seg = function ($) {
			return $.segment;
		}(currentSplit);
		var sums_ = _Utils_update(
			sums,
			{
				average: A2(
					$elm$core$Maybe$map,
					$elm$core$Basics$add(
						A2(
							$elm$core$Maybe$withDefault,
							0,
							function ($) {
								return $.average;
							}(seg))),
					function ($) {
						return $.average;
					}(sums)),
				gold: A2(
					$elm$core$Maybe$map,
					$elm$core$Basics$add(
						A2(
							$elm$core$Maybe$withDefault,
							0,
							function ($) {
								return $.gold;
							}(seg))),
					function ($) {
						return $.gold;
					}(sums)),
				pb: function ($) {
					return $.pb;
				}(seg),
				worst: A2(
					$elm$core$Maybe$map,
					$elm$core$Basics$add(
						A2(
							$elm$core$Maybe$withDefault,
							0,
							function ($) {
								return $.worst;
							}(seg))),
					function ($) {
						return $.worst;
					}(sums))
			});
		var nextSegment = _Utils_update(
			seg,
			{
				average: function () {
					var _v4 = function ($) {
						return $.average;
					}(seg);
					if (_v4.$ === 'Nothing') {
						return function ($) {
							return $.average;
						}(last);
					} else {
						var x = _v4;
						return x;
					}
				}(),
				gold: function () {
					var _v5 = function ($) {
						return $.gold;
					}(seg);
					if (_v5.$ === 'Nothing') {
						return function ($) {
							return $.gold;
						}(last);
					} else {
						var x = _v5;
						return x;
					}
				}(),
				pb: function () {
					var _v6 = function ($) {
						return $.pb;
					}(seg);
					if (_v6.$ === 'Nothing') {
						return function ($) {
							return $.pb;
						}(last);
					} else {
						var x = _v6;
						return x;
					}
				}(),
				worst: function () {
					var _v7 = function ($) {
						return $.worst;
					}(seg);
					if (_v7.$ === 'Nothing') {
						return function ($) {
							return $.worst;
						}(last);
					} else {
						var x = _v7;
						return x;
					}
				}()
			});
		var indiv = _Utils_update(
			seg,
			{
				average: function ($) {
					return $.average;
				}(seg),
				gold: function ($) {
					return $.gold;
				}(seg),
				pb: function () {
					var _v3 = A2(
						$elm$core$Maybe$map,
						function (t) {
							return t - A2(
								$elm$core$Maybe$withDefault,
								0,
								function ($) {
									return $.pb;
								}(last));
						},
						function ($) {
							return $.pb;
						}(seg));
					if (_v3.$ === 'Nothing') {
						return function ($) {
							return $.pb;
						}(last);
					} else {
						var x = _v3;
						return x;
					}
				}(),
				worst: function ($) {
					return $.worst;
				}(seg)
			});
		var currentTime = A2(
			$elm$core$Maybe$map,
			function (t) {
				return $elm$time$Time$posixToMillis(t) - now;
			},
			function ($) {
				return $.endTime;
			}(currentSplit));
		var nextTime = function () {
			if (currentTime.$ === 'Nothing') {
				return lastTime;
			} else {
				var x = currentTime;
				return x;
			}
		}();
		var singleTime = A3($elm$core$Maybe$map2, $elm$core$Basics$sub, currentTime, lastTime);
		var tags = A4($author$project$Timer$statusTags, indiv, seg, singleTime, currentTime);
		return _Utils_Tuple2(
			_Utils_Tuple3(sums_, nextSegment, nextTime),
			_Utils_ap(
				segs,
				_List_fromArray(
					[
						{currentSum: currentTime, running: sums_, segment: indiv, singleTime: singleTime, timingTags: tags}
					])));
	});
var $author$project$Timer$initSegment = {average: $elm$core$Maybe$Nothing, entityID: $elm$core$Maybe$Nothing, gold: $elm$core$Maybe$Nothing, icon: $elm$core$Maybe$Nothing, name: 'Run aggregation initial segment', pb: $elm$core$Maybe$Nothing, worst: $elm$core$Maybe$Nothing};
var $author$project$Timer$aggregateRun = F2(
	function (now, r) {
		var zeroSegment = _Utils_update(
			$author$project$Timer$initSegment,
			{
				average: $elm$core$Maybe$Just(0),
				gold: $elm$core$Maybe$Just(0),
				pb: $elm$core$Maybe$Just(0),
				worst: $elm$core$Maybe$Just(0)
			});
		var upcoming = function ($) {
			return $.splits;
		}(r).upcoming;
		var previous = function ($) {
			return $.splits;
		}(r).previous;
		var current = function () {
			var _v5 = function ($) {
				return $.current;
			}(
				function ($) {
					return $.splits;
				}(r));
			if (_v5.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var s = _v5.a;
				return _List_fromArray(
					[s]);
			}
		}();
		var _v0 = A3(
			$elm$core$List$foldl,
			$author$project$Timer$aggregateRun_(now),
			_Utils_Tuple2(
				_Utils_Tuple3(
					zeroSegment,
					$author$project$Timer$initSegment,
					$elm$core$Maybe$Just(0)),
				_List_Nil),
			previous);
		var _v1 = _v0.a;
		var sums1 = _v1.a;
		var pastSegment = _v1.b;
		var runningTotalsP = _v0.b;
		var _v2 = A3(
			$elm$core$List$foldl,
			$author$project$Timer$aggregateRun_(now),
			_Utils_Tuple2(
				_Utils_Tuple3(sums1, pastSegment, $elm$core$Maybe$Nothing),
				_List_Nil),
			current);
		var currentSegment = _v2.a;
		var runningTotalC_ = _v2.b;
		var _v3 = A3(
			$elm$core$List$foldl,
			$author$project$Timer$aggregateRun_(now),
			_Utils_Tuple2(currentSegment, _List_Nil),
			upcoming);
		var runningTotalsU = _v3.b;
		var runningTotalC = function () {
			if (!runningTotalC_.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = runningTotalC_.a;
				return $elm$core$Maybe$Just(x);
			}
		}();
		return {current: runningTotalC, previous: runningTotalsP, upcoming: runningTotalsU};
	});
var $author$project$Main$splitHeaders = _List_fromArray(
	[
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('segments-header', true)
					]))
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('pb', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('PB')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('gold', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Best')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('average', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Avg')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('worst', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Worst')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('split', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Split')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running', true),
								_Utils_Tuple2('pb', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('PB')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running', true),
								_Utils_Tuple2('gold', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Best')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running', true),
								_Utils_Tuple2('average', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Avg')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running', true),
								_Utils_Tuple2('worst', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Worst')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running', true),
								_Utils_Tuple2('split', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Split')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('diff', true),
								_Utils_Tuple2('pb', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- PB')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('diff', true),
								_Utils_Tuple2('gold', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Best')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('diff', true),
								_Utils_Tuple2('average', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Avg')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('diff', true),
								_Utils_Tuple2('worst', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Worst')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running-diff', true),
								_Utils_Tuple2('pb', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- PB')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running-diff', true),
								_Utils_Tuple2('gold', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Best')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running-diff', true),
								_Utils_Tuple2('average', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Avg')
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('header', true),
								_Utils_Tuple2('running-diff', true),
								_Utils_Tuple2('worst', true)
							]))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('+/- Worst')
					]))
			]))
	]);
var $author$project$Main$diffDatum = function (d) {
	var sdiff = F3(
		function (seg, f, g) {
			var _v0 = f(seg);
			if (_v0.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var a = _v0.a;
				return A2(
					$elm$core$Maybe$map,
					function (b) {
						return a - b;
					},
					g(d));
			}
		});
	var s = function ($) {
		return $.segment;
	}(d);
	var r = function ($) {
		return $.running;
	}(d);
	return _Utils_update(
		d,
		{
			running: _Utils_update(
				r,
				{
					average: A3(
						sdiff,
						r,
						function ($) {
							return $.average;
						},
						function ($) {
							return $.currentSum;
						}),
					gold: A3(
						sdiff,
						r,
						function ($) {
							return $.gold;
						},
						function ($) {
							return $.currentSum;
						}),
					pb: A3(
						sdiff,
						r,
						function ($) {
							return $.pb;
						},
						function ($) {
							return $.currentSum;
						}),
					worst: A3(
						sdiff,
						r,
						function ($) {
							return $.worst;
						},
						function ($) {
							return $.currentSum;
						})
				}),
			segment: _Utils_update(
				s,
				{
					average: A3(
						sdiff,
						s,
						function ($) {
							return $.average;
						},
						function ($) {
							return $.singleTime;
						}),
					gold: A3(
						sdiff,
						s,
						function ($) {
							return $.gold;
						},
						function ($) {
							return $.singleTime;
						}),
					pb: A3(
						sdiff,
						s,
						function ($) {
							return $.pb;
						},
						function ($) {
							return $.singleTime;
						}),
					worst: A3(
						sdiff,
						s,
						function ($) {
							return $.worst;
						},
						function ($) {
							return $.singleTime;
						})
				})
		});
};
var $author$project$Main$splitSegment = F3(
	function (showSign, tags, i) {
		var tags_ = A2(
			$elm$core$List$map,
			function (t) {
				return _Utils_Tuple2(t, true);
			},
			tags);
		if (i.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_Utils_ap(
							tags_,
							_List_fromArray(
								[
									_Utils_Tuple2('split-column', true),
									_Utils_Tuple2('segment-time', true)
								])))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('time-separator')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('-')
									]))
							]))
					]));
		} else {
			var t = i.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_Utils_ap(
							tags_,
							_List_fromArray(
								[
									_Utils_Tuple2('split-column', true),
									_Utils_Tuple2('segment-time', true)
								])))
					]),
				_List_fromArray(
					[
						A3(
						$author$project$Main$showD,
						showSign,
						_List_Nil,
						$author$project$Main$unitize(t))
					]));
		}
	});
var $author$project$Main$aggregateData = F2(
	function (_v0, _v2) {
		var i = _v0.a;
		var _v1 = _v0.b;
		var tag = _v1.a;
		var timing = _v1.b;
		var j = _v2.a;
		var hs = _v2.b;
		var even = !A2($elm$core$Basics$modBy, 2, i + j);
		var diff = $author$project$Main$diffDatum(timing);
		var div = A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_Utils_ap(
						function ($) {
							return $.timingTags;
						}(timing),
						_List_fromArray(
							[
								_Utils_Tuple2('segment', true),
								_Utils_Tuple2(tag, true),
								_Utils_Tuple2('even', even),
								_Utils_Tuple2('odd', !even)
							])))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('segment-name')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function ($) {
								return $.segment;
							}(timing).name)
						])),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['pb']),
					function ($) {
						return $.segment;
					}(timing).pb),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['gold']),
					function ($) {
						return $.segment;
					}(timing).gold),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['average']),
					function ($) {
						return $.segment;
					}(timing).average),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['worst']),
					function ($) {
						return $.segment;
					}(timing).worst),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['split']),
					function ($) {
						return $.singleTime;
					}(timing)),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['running', 'pb']),
					function ($) {
						return $.running;
					}(timing).pb),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['running', 'gold']),
					function ($) {
						return $.running;
					}(timing).gold),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['running', 'average']),
					function ($) {
						return $.running;
					}(timing).average),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['running', 'worst']),
					function ($) {
						return $.running;
					}(timing).worst),
					A3(
					$author$project$Main$splitSegment,
					false,
					_List_fromArray(
						['running', 'split']),
					function ($) {
						return $.currentSum;
					}(timing)),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['diff', 'pb']),
					function ($) {
						return $.segment;
					}(diff).pb),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['diff', 'gold']),
					function ($) {
						return $.segment;
					}(diff).gold),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['diff', 'average']),
					function ($) {
						return $.segment;
					}(diff).average),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['diff', 'worst']),
					function ($) {
						return $.segment;
					}(diff).worst),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['running-diff', 'pb']),
					function ($) {
						return $.running;
					}(diff).pb),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['running-diff', 'gold']),
					function ($) {
						return $.running;
					}(diff).gold),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['running-diff', 'average']),
					function ($) {
						return $.running;
					}(diff).average),
					A3(
					$author$project$Main$splitSegment,
					true,
					_List_fromArray(
						['running-diff', 'worst']),
					function ($) {
						return $.running;
					}(diff).worst)
				]));
		return _Utils_Tuple2(
			i + j,
			_Utils_ap(
				hs,
				_List_fromArray(
					[div])));
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Main$splitsList_ = F2(
	function (max, timing) {
		var f = function (s) {
			return $elm$core$List$map(
				function (x) {
					return _Utils_Tuple2(s, x);
				});
		};
		var last = A2(
			f,
			'upcoming',
			A2(
				$elm$core$List$take,
				1,
				$elm$core$List$reverse(
					function ($) {
						return $.upcoming;
					}(timing))));
		var pre1 = A2(
			f,
			'previous',
			A2(
				$elm$core$List$take,
				1,
				$elm$core$List$reverse(
					function ($) {
						return $.previous;
					}(timing))));
		var current_ = function () {
			var _v1 = function ($) {
				return $.current;
			}(timing);
			if (_v1.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var t = _v1.a;
				return _List_fromArray(
					[t]);
			}
		}();
		var current = A2(f, 'current', current_);
		var upcoming = A2(
			f,
			'upcoming',
			A2(
				$elm$core$List$take,
				max - $elm$core$List$length(
					_Utils_ap(
						pre1,
						_Utils_ap(last, current))),
				$elm$core$List$reverse(
					A2(
						$elm$core$List$drop,
						1,
						$elm$core$List$reverse(
							function ($) {
								return $.upcoming;
							}(timing))))));
		var pre = A2(
			f,
			'previous',
			$elm$core$List$reverse(
				A2(
					$elm$core$List$take,
					max - $elm$core$List$length(
						_Utils_ap(
							pre1,
							_Utils_ap(
								current,
								_Utils_ap(upcoming, last)))),
					A2(
						$elm$core$List$drop,
						1,
						$elm$core$List$reverse(
							function ($) {
								return $.previous;
							}(timing))))));
		var data = A2(
			$elm$core$List$map,
			function (x) {
				return _Utils_Tuple2(1, x);
			},
			_Utils_ap(
				pre,
				_Utils_ap(
					pre1,
					_Utils_ap(
						current,
						_Utils_ap(upcoming, last)))));
		var _v0 = A3(
			$elm$core$List$foldl,
			$author$project$Main$aggregateData,
			_Utils_Tuple2(0, _List_Nil),
			data);
		var divs = _v0.b;
		return divs;
	});
var $author$project$Main$splitsList = F2(
	function (max, splits) {
		var currentTime = $elm$time$Time$posixToMillis(
			A2(
				$elm$core$Maybe$withDefault,
				function ($) {
					return $.currentTime;
				}(splits),
				function ($) {
					return $.runStarted;
				}(splits)));
		var _v0 = function ($) {
			return $.runTracker;
		}(splits);
		if (_v0.$ === 'SingleCategory') {
			var r = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('splits-container')
					]),
				_Utils_ap(
					$author$project$Main$splitHeaders,
					A2(
						$author$project$Main$splitsList_,
						max,
						A2($author$project$Timer$aggregateRun, currentTime, r))));
		} else {
			var rs = _v0.a;
			var current = function () {
				var _v1 = function ($) {
					return $.current;
				}(rs);
				if (_v1.$ === 'Just') {
					var r = _v1.a;
					return _List_fromArray(
						[r]);
				} else {
					var _v2 = function ($) {
						return $.upcoming;
					}(rs);
					if (_v2.b) {
						var r = _v2.a;
						return _List_fromArray(
							[r]);
					} else {
						return A2(
							$elm$core$List$drop,
							$elm$core$List$length(
								function ($) {
									return $.previous;
								}(rs)) - 1,
							function ($) {
								return $.previous;
							}(rs));
					}
				}
			}();
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('splits-container')
					]),
				_Utils_ap(
					$author$project$Main$splitHeaders,
					A2(
						$elm$core$List$concatMap,
						$author$project$Main$splitsList_(max),
						A2(
							$elm$core$List$map,
							$author$project$Timer$aggregateRun(currentTime),
							current))));
		}
	});
var $author$project$Main$timerViewNormal = F2(
	function (max, timer) {
		var splits = $author$project$Timer$splitsFor(timer);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('timer-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('timer-title')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function ($) {
								return $.title;
							}(splits))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('timer-subtitle')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function ($) {
								return $.subtitle;
							}(splits))
						])),
					A2($author$project$Main$splitsList, max, splits),
					$author$project$Main$categoryTimer(timer)
				]));
	});
var $author$project$Main$timerView = function (model) {
	var _v0 = function ($) {
		return $.splitsMode;
	}(model);
	if (_v0.$ === 'NormalSplitsView') {
		return A2(
			$author$project$Main$timerViewNormal,
			function ($) {
				return $.maxSegmentsShown;
			}(model),
			function ($) {
				return $.timer;
			}(model));
	} else {
		return A3(
			$author$project$Main$timerViewEdit,
			function ($) {
				return $.fullCategoryList;
			}(model),
			function ($) {
				return $.editMulticategory;
			}(model),
			function ($) {
				return $.timer;
			}(model));
	}
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('main-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('app-top')
					]),
				_Utils_ap(
					$author$project$Main$menuView(model),
					_List_fromArray(
						[
							$author$project$Main$timerView(model)
						])))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$string)(0)}});}(this));