'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.tmpfile = exports.slurp = exports.readdir = exports.hideCursor = exports.showCursor = exports.stopSpinner = exports.startSpinner = exports.goUp = exports.shellQuote = exports.blue = exports.brightBlue = exports.cyan = exports.brightRed = exports.red = exports.magenta = exports.yellow = exports.green = exports.log = exports.error = exports.warn = exports.appendToFile = exports.write = exports.writeFile = exports.sys = exports.sysPromise = exports.sprintf = exports.getopt = undefined;

var _toConsumableArray2 = require('babel-runtime/helpers/toConsumableArray');

var _toConsumableArray3 = _interopRequireDefault(_toConsumableArray2);

var _fs = require('fs');

var _fs2 = _interopRequireDefault(_fs);

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _tmp = require('tmp');

var _tmp2 = _interopRequireDefault(_tmp);

var _fishLib = require('fish-lib');

var _fishLib2 = _interopRequireDefault(_fishLib);

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

var getopt = exports.getopt = _fishLib2.default.getopt;
var sprintf = exports.sprintf = _fishLib2.default.sprintf;

// --- note: supplying stdin to a sync process is hard.
// --- should probably change fish-lib to make the default for stdin be pipe
// instead of process (0)
_fishLib2.default.sysSet({ sync: true, die: true, verbose: true });
_fishLib2.default.bulletSet('٭');

var on = (0, _stick.dot2)('on');

var sysPromise = exports.sysPromise = function sysPromise() {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
    }

    return new Promise(function (res, rej) {
        var ret = void 0;
        var sysArgs = _op(args, (0, _stick.concatFrom)([function (_ref) {
            var out = _ref.out,
                ok = _ref.ok,
                err = _ref.err;
            return _op(ok, (0, _stick.ifFalse)(function (_) {
                return _op(_op("cmd failed", _stick.exception), _stick.raise);
            }, function (_) {
                return ret = out;
            }));
        }]));
        _op(_op(_fishLib.sysSpawn.apply(undefined, (0, _toConsumableArray3.default)(sysArgs)), on('close')(function (code, signal) {
            return code === 0 ? _op(ret, res) : _op(_op(code, 'cmd error (code = %d)'), rej);
        })), on('error')(rej));
    });
};

var sys = exports.sys = function sys(verbose) {
    return function () {
        for (var _len2 = arguments.length, args = Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
            args[_key2] = arguments[_key2];
        }

        var ret = void 0;
        _fishLib.sysSpawn.apply(undefined, args.concat([{ verbose: verbose }, function (_ref2) {
            var out = _ref2.out;
            return ret = out;
        }]));
        return ret;
    };
};

var writeFile = exports.writeFile = (0, _ramda.curry)(function (path, contents) {
    return _fs2.default.writeFileSync(path, contents);
});
var write = exports.write = _op('write', (0, _stick.bind)(process.stdout));

var appendToFile = exports.appendToFile = (0, _ramda.curry)(function (filename, contents) {
    return _fs2.default.appendFileSync(filename, contents);
});

exports.warn = _fishLib.warn;
exports.error = _fishLib.error;
exports.log = _fishLib.log;
exports.green = _fishLib.green;
exports.yellow = _fishLib.yellow;
exports.magenta = _fishLib.magenta;
exports.red = _fishLib.red;
exports.brightRed = _fishLib.brightRed;
exports.cyan = _fishLib.cyan;
exports.brightBlue = _fishLib.brightBlue;
exports.blue = _fishLib.blue;
exports.shellQuote = _fishLib.shellQuote;
var goUp = exports.goUp = _op('[A', _ramda.always);

var spinner = {
    job: void 8,
    charIdx: 0,
    chars: "◓◑◒◐",
    label: '',
    lastNumChars: 0,
    cycleChar: function cycleChar() {
        this.charIdx = ++this.charIdx % this.chars.length;
    },
    str: function str() {
        var _this = this;

        return _op((0, _stick.laatStar)(function (_) {
            return _op(_op('', (0, _stick.repeat)(_this.lastNumChars)), (0, _ramda.join)(''));
        }, function (_) {
            return _this.chars[_this.charIdx];
        }, function (_) {
            return _this.label;
        }, function (pref, char, label) {
            return _op(_op(_op([char, label, char], (0, _stick.sprintfN)('%s %s %s')), (0, _ramda.tap)(function (l) {
                return _this.lastNumChars = l.length;
            })), (0, _stick.concatTo)(pref));
        }), (0, _ramda.tap)(function (_) {
            return _this.cycleChar();
        }));
    },
    start: function start(label) {
        var _this2 = this;

        this.label = label;
        this.job = setInterval(function (_) {
            return _op(_this2.str(), write);
        }, 100);
    },
    stop: function stop() {
        clearInterval(this.job);
    }
};

var startSpinner = exports.startSpinner = _op('start', (0, _stick.bind)(spinner));
var stopSpinner = exports.stopSpinner = _op('stop', (0, _stick.bind)(spinner));

var showCursor = exports.showCursor = function showCursor(_) {
    return _op('\x1B[?25h', write);
};
var hideCursor = exports.hideCursor = function hideCursor(_) {
    return _op('\x1B[?25l', write);
};

// --- throws.
var readdir = exports.readdir = _fs2.default.readdirSync;

var slurp = exports.slurp = function slurp(filename) {
    return (0, _stick.tryCatch__)(function () {
        return _fs2.default.readFileSync(filename).toString();
    }, function (e) {
        return (0, _fishLib.error)(sprintf("Could not read file %s: %s", (0, _fishLib.brightRed)(filename), e));
    });
};

// --- throws
var tmpfile = exports.tmpfile = function tmpfile(_) {
    return _tmp2.default.fileSync().name;
};