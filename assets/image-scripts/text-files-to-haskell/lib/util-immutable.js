'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toOrderedMap = exports.entrySeq = exports.listToOrderedSet = exports.filter = exports.getIn = exports.get = exports.valueSeq = exports.size = exports.updateIn = exports.update = exports.setIn = exports.set = exports.del = exports.find = exports.add = exports.push = exports.sortBy = exports.toJS = exports.fromJS = undefined;

var _toConsumableArray2 = require('babel-runtime/helpers/toConsumableArray');

var _toConsumableArray3 = _interopRequireDefault(_toConsumableArray2);

var _ramda = require('ramda');

var _stick = require('stick');

var _immutable = require('immutable');

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

exports.fromJS = _immutable.fromJS;
var toJS = exports.toJS = (0, _stick.dot)('toJS');
var sortBy = exports.sortBy = (0, _stick.dot1)('sortBy');
var push = exports.push = (0, _stick.dot1)('push');
var add = exports.add = (0, _stick.dot1)('add');
var find = exports.find = (0, _stick.dot1)('find');
var del = exports.del = (0, _stick.dot1)('delete');

var set = exports.set = (0, _stick.dot2)('set');
var setIn = exports.setIn = (0, _stick.dot2)('setIn');

var update = exports.update = (0, _stick.dot2)('update');
var updateIn = exports.updateIn = (0, _stick.dot2)('updateIn');

var size = exports.size = (0, _ramda.prop)('size');

var valueSeq = exports.valueSeq = (0, _stick.dot)('valueSeq');
var get = exports.get = (0, _stick.dot1)('get');
var getIn = exports.getIn = (0, _stick.dot1)('getIn');

var filter = exports.filter = (0, _stick.dot1)('filter');

var listToOrderedSet = exports.listToOrderedSet = function listToOrderedSet(xs) {
  return _immutable.OrderedSet.of.apply(_immutable.OrderedSet, (0, _toConsumableArray3.default)(xs));
};

var entrySeq = exports.entrySeq = (0, _stick.dot)('entrySeq');
var toOrderedMap = exports.toOrderedMap = (0, _stick.dot)('toOrderedMap');