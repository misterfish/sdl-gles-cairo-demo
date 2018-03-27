'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.toJS = exports.dag = undefined;

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

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

var dag = exports.dag = function dag(type) {
    return function (x) {
        return type.is(x);
    };
};

var toJS = exports.toJS = (0, _stick.dot)('toJS');