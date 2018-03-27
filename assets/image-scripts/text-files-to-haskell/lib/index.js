#!/usr/bin/env node
'use strict';

var _toArray2 = require('babel-runtime/helpers/toArray');

var _toArray3 = _interopRequireDefault(_toArray2);

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _utilBilby = require('./util-bilby');

var _utilIo = require('./util-io');

var _util = require('util');

var _util2 = _interopRequireDefault(_util);

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

// --- add this to the patterns.
// script with sys commands, sync & die, no flatmap.

var config = {
    rootDir: _op([__dirname, '..'], (0, _ramda.join)('/')),
    inputDir: function inputDir() {
        return _op([this.rootDir, 'a'], (0, _ramda.join)('/'));
    },

    cropHeight: 512,
    cropWidth: 512,
    // --- xxx
    tmpdir: '/tmp',
    debug: false,
    usage: 'identifier'
};

var whenNotOk = (0, _stick.whenPredicate)(_op2(_stick.ok, _ramda.not));
var ifNotOk = (0, _stick.ifPredicate)(_op2(_stick.ok, _ramda.not));

var usageF = function usageF(msg) {
    return function () {
        var _process$argv = (0, _toArray3.default)(process.argv),
            _ = _process$argv[0],
            scriptName = _process$argv[1],
            args = _process$argv.slice(2);

        var str = (0, _ramda.join)(' ', (0, _stick.compactOk)([scriptName, msg]));
        return (0, _utilIo.sprintf)("Usage: %s", str);
    };
};

var usage = usageF(config.usage);

var opt = (0, _utilIo.getopt)({
    h: 'b',
    identifier: 's'
});

if (opt.h) {
    (0, _utilIo.info)(usage());
    process.exit(0);
}

var sys = (0, _utilIo.sys)(config.debug);

var output = {
    slurped: []
};

var inspect = function inspect(x) {
    return _util2.default.inspect(x, { depth: null, colors: process.stdout.isTTY });
};

// --- all sys commands are sync: true and die: true, so there are few or no eithers necessary.
var go = function go(identifier, files) {
    return _op(_op(_op(_op(files, (0, _ramda.reduce)(reducer)(output)), (0, _ramda.prop)('slurped')), toHaskell(identifier)), _utilIo.log);
};

var length = (0, _ramda.prop)('length');
var lines = (0, _ramda.split)('\n');

var strRepeat = function strRepeat(n) {
    return function (str) {
        return _op(_op(str, (0, _stick.repeat)(n)), (0, _ramda.join)(''));
    };
};

// --- result is slow to compile
var toHaskellMultiLineStringLong = function toHaskellMultiLineStringLong(str) {
    return _op(_op(_op(_op(str, lines), (0, _ramda.join)('\\n')), (0, _stick.concatFrom)('"')), (0, _stick.concatTo)('"'));
};

// --- result is extremely slow to compile / out of memory
var toHaskellMultiLineStringMonoid = function toHaskellMultiLineStringMonoid(identifierLength) {
    return function (str) {
        return (0, _stick.laats)(function (_) {
            return _op(' ', strRepeat(identifierLength + 5));
        }, function (ind) {
            return _op(_op(_op(str, lines), (0, _ramda.map)(_op2((0, _stick.concatFrom)('\\n"'), (0, _stick.concatTo)('"')))), (0, _ramda.join)(' <>\n' + ind));
        });
    };
};

var toHaskell = function toHaskell(identifier) {
    return function (xs) {
        return (0, _stick.laats)(function (_) {
            return _op(identifier, length);
        }, function (l) {
            return _op(_op(' ', (0, _stick.repeat)(l + 3)), (0, _ramda.join)(''));
        },
        // l => xs | map (toHaskellMultiLineStringMonoid (l)),
        function (l) {
            return _op(xs, (0, _ramda.map)(toHaskellMultiLineStringLong));
        }, function (_1, ind, vs) {
            return _op(vs, (0, _ramda.join)('\n' + ind + ', '));
        }, function (_1, _2, _3, x) {
            return _op([identifier, x], (0, _stick.sprintfN)('%s = [ %s ]'));
        });
    };
};

var reducer = function reducer(output, file) {
    return (0, _stick.laats)(function (_) {
        return _op(file, _utilIo.slurp);
    }, function (slurped) {
        return _op(output, (0, _ramda.assoc)('slurped')(_op(output.slurped, (0, _stick.appendFrom)(slurped))));
    });
};

var files = _op(opt.argv.remain, (0, _ramda.tap)((0, _stick.whenEmpty)(function (_) {
    return (0, _utilIo.error)(usage());
})));

var identifier = _op(opt.identifier, ifNotOk(function (_) {
    return (0, _utilIo.error)(usage());
}, _ramda.identity));

go(identifier, files);