'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.cata = exports.sequenceM = exports.toJust = exports.isJust = exports.toEither = exports.flatMap = exports.fold = exports.isLeft = exports.Nothing = exports.Just = exports.Right = exports.Left = undefined;

var _toConsumableArray2 = require('babel-runtime/helpers/toConsumableArray');

var _toConsumableArray3 = _interopRequireDefault(_toConsumableArray2);

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _bilby = require('bilby');

var _bilby2 = _interopRequireDefault(_bilby);

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

exports.Left = _bilby.left;
exports.Right = _bilby.right;
exports.Just = _bilby.some;
exports.Nothing = _bilby.none;
var isLeft = exports.isLeft = (0, _ramda.prop)('isLeft');
var fold = exports.fold = (0, _stick.dot2)('fold');
var flatMap = exports.flatMap = (0, _stick.dot1)('flatMap');

var toEither = exports.toEither = (0, _ramda.curry)(function (l, o) {
    return _op(o, (0, _stick.ifOk)(_bilby.right, _op(_op(l, _bilby.left), _ramda.always)));
});

var isJust = exports.isJust = (0, _ramda.prop)('isSome');
var toJust = exports.toJust = fold(_ramda.identity, _stick.noop);

var colon = (0, _ramda.curry)(function (x, xs) {
    return [x].concat((0, _toConsumableArray3.default)(xs));
});
var liftA2 = _op('liftA2', (0, _stick.bind)(_bilby2.default));

var sequenceM = exports.sequenceM = function sequenceM(pure) {
    var _sequence = function _sequence(xs) {
        return xs.length === 0 ? _op([], pure) : liftA2(colon, _op(xs, _ramda.head), _op(_op(xs, _ramda.tail), _sequence));
    };
    return _sequence;
};

var cata = exports.cata = (0, _stick.dot1)('cata');