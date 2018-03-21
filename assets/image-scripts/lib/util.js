'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.on = exports.padTo = exports.allOk = exports.mapObj = exports.ellipsisAfter = exports.substring = exports.divideBy = exports.guardA = exports.lazyFindPred = exports.mapX = exports.length = exports.tryCatchO = exports.findPredMaybeGen = exports.findPredOkGen = exports.findPredOk = exports.xMatchGlobal = exports.xRegExpFlags = exports.minus = exports.recover = exports.then = exports.getType = undefined;

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _utilPred = require('./util-pred');

var _utilBilby = require('./util-bilby');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var _op = function _op(a, b) {
    return b(a);
};

var _op2 = (0, _ramda.curry)(function (a, b) {
    return (0, _ramda.compose)(b, a);
});

var _op3 = (0, _ramda.curry)(function (a, b) {
    return (0, _ramda.compose)(a, b);
});

// --- beware circular reference: don't use these in point-free functions.


var getType = exports.getType = _op2((0, _stick.callUnder)({}.toString), (0, _stick.dot2)('slice')(8, -1));

var then = exports.then = (0, _stick.dot1)('then');
var recover = exports.recover = (0, _stick.dot1)('catch');

var minus = exports.minus = (0, _ramda.curry)(function (a, b) {
    return b - a;
});

// ------ stick extensions

var removeSpaces = (0, _stick.dot2)('replace')(/\s+/g)('');

// --- beware, overwrites any flags that the re already had.
var xRegExpFlags = exports.xRegExpFlags = function xRegExpFlags(re, flags) {
    return new RegExp(_op(re.source, removeSpaces), flags);
};

var xMatchGlobal = exports.xMatchGlobal = (0, _ramda.curry)(function (re, mapper, target) {
    var out = [];
    var reGlobal = xRegExpFlags(re, 'g');
    var m = void 0;
    while (m = reGlobal.exec(target)) {
        _op(mapper(m), (0, _stick.appendToMut)(out));
    }return out;
});

var findPredOk = exports.findPredOk = (0, _ramda.curry)(function (pred, xs) {
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
        for (var _iterator = xs[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var x = _step.value;

            var p = pred(x);
            if ((0, _stick.ok)(p)) return p;
        }
    } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
    } finally {
        try {
            if (!_iteratorNormalCompletion && _iterator.return) {
                _iterator.return();
            }
        } finally {
            if (_didIteratorError) {
                throw _iteratorError;
            }
        }
    }
});

var findPredOkGen = exports.findPredOkGen = (0, _ramda.curry)(function (pred, gen) {
    var n = void 0;
    while (!(n = gen.next()).done) {
        var p = pred(n.value);
        if ((0, _stick.ok)(p)) return p;
    }
});

var findPredMaybeGen = exports.findPredMaybeGen = (0, _ramda.curry)(function (pred, gen) {
    var n = void 0;
    while (!(n = gen.next()).done) {
        var p = pred(n.value);
        if (_op(p, _utilBilby.isJust)) return _op(p, _utilBilby.toJust);
    }
});

var tryCatchO = exports.tryCatchO = (0, _ramda.curry)(function (whatToTry, howToCatch, val) {
    try {
        return whatToTry(val);
    } catch (e) {
        return howToCatch(e, val);
    }
});

var length = exports.length = (0, _ramda.prop)('length');

var mapX = exports.mapX = _op(_ramda.map, _ramda.addIndex);

// ------ lazyfish extensions
var lazyFindPred = exports.lazyFindPred = (0, _ramda.curry)(function (pred, lxs) {
    while (true) {
        var _lxs$next = lxs.next(),
            value = _lxs$next.value,
            done = _lxs$next.done;

        if (done) break;
        var predVal = pred(value);
        if (predVal) return predVal;
    }
});

var guardA = exports.guardA = _op2(_ramda.always, _stick.guard);

var divideBy = exports.divideBy = (0, _ramda.curry)(function (a, b) {
    return b / a;
});

var substring = exports.substring = (0, _stick.dot2)('substring');

var ellipsisAfter = exports.ellipsisAfter = (0, _ramda.curry)(function (n, s) {
    return _op(s, (0, _utilPred.ifLongerThan)(n)(_op2(substring(0, n), (0, _stick.concatFrom)('…')), _ramda.identity));
});

// --- only own (R.toPairs and R.map are like this too)
// --- order: k, v
var mapObj = exports.mapObj = (0, _ramda.curry)(function (f, o) {
    var ret = [];
    for (var i in o) {
        ret.push(f(i, o[i]));
    }
    return ret;
});

var allOk = exports.allOk = function allOk(x) {
    return _op(_op(x, (0, _ramda.findIndex)(_op2(_stick.ok, _ramda.not))), (0, _utilPred.ifNegativeOne)(_ramda.T, _ramda.F));
};

var precatTo = _stick.concatFrom;

// --- doesn't truncate if too long.
var padTo = exports.padTo = (0, _ramda.curry)(function (n, str) {
    return (0, _stick.laatStar)(function (_) {
        return str.length;
    }, function (l) {
        return _op(str, (0, _stick.cond)([_op(function (_) {
            return l >= n;
        }, guardA(str)), _op(_stick.otherwise, (0, _stick.guard)(function (x) {
            return _op(x, _op(_op((0, _stick.repeat)(n - l)(' '), (0, _ramda.join)('')), precatTo));
        }))]));
    });
});

var on = exports.on = (0, _stick.tapDot2)('on');