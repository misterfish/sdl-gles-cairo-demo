'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.ifAllOk = exports.ifSingletonLeft = exports.ifNegativeOne = exports.ifException = exports.isException = exports.ifLongerThan = exports.isArray = exports.isType = undefined;

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _util = require('./util');

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


var isType = exports.isType = (0, _ramda.curry)(function (type, val) {
    return _op((0, _util.getType)(val), (0, _ramda.equals)(type));
});
var isArray = exports.isArray = isType('Array');

var ifLongerThan = exports.ifLongerThan = function ifLongerThan(n) {
    return (0, _stick.ifPredicate)(_op2(_util.length, (0, _stick.gt)(n)));
};

var isException = exports.isException = isType('Error');
var ifException = exports.ifException = (0, _stick.ifPredicate)(isException);
var ifNegativeOne = exports.ifNegativeOne = (0, _stick.ifPredicate)(_op(-1, _stick.eq));

// --- ugly (...args), but avoiding circular refs.
var ifSingletonLeft = exports.ifSingletonLeft = function ifSingletonLeft() {
    return (0, _stick.ifPredicate)((0, _ramda.allPass)([isArray, _op2(_util.length, (0, _stick.eq)(1)), _op2((0, _ramda.prop)(0), _utilBilby.isLeft)])).apply(undefined, arguments);
};
var ifAllOk = exports.ifAllOk = function ifAllOk() {
    return (0, _stick.ifPredicate)(_util.allOk).apply(undefined, arguments);
};