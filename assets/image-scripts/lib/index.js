#!/usr/bin/env node
'use strict';

var _slicedToArray2 = require('babel-runtime/helpers/slicedToArray');

var _slicedToArray3 = _interopRequireDefault(_slicedToArray2);

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _stick = require('stick');

var _utilBilby = require('./util-bilby');

var _utilIo = require('./util-io');

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

var config = {
    rootDir: _op([__dirname, '..'], (0, _ramda.join)('/')),
    inputDir: function inputDir() {
        return _op([this.rootDir, 'a'], (0, _ramda.join)('/'));
    },

    cropHeight: 512,
    cropWidth: 512,
    // --- xxx
    tmpdir: '/tmp',
    tag: 'movieMock',
    debug: false
};

var sys = (0, _utilIo.sys)(config.debug);

var output = {
    base64Data: []
};

var takePartial = _op(config.debug, (0, _stick.ifTrue)(_op(_op(3, _ramda.take), _ramda.always), _op(_ramda.identity, _ramda.always)));

// --- all sys commands are sync: true and die: true, so there are few or no eithers necessary.
var go = function go(_) {
    return (0, _stick.laats)(function (_) {
        return config.inputDir();
    }, function (dir) {
        return _op(_op(_op(_op(_op(_op(_op(dir, _utilIo.readdir), takePartial), (0, _ramda.map)(_op2((0, _stick.appendTo)([dir]), (0, _ramda.join)('/')))), (0, _ramda.reduce)(reducer)(output)), (0, _ramda.prop)('base64Data')), toHaskell(config.tag)), _utilIo.log);
    });
};

var length = (0, _ramda.prop)('length');

var toHaskell = function toHaskell(tag) {
    return function (xs) {
        return (0, _stick.laats)(function (_) {
            return _op(tag, length) + 3;
        }, function (l) {
            return _op(_op(' ', (0, _stick.repeat)(l)), (0, _ramda.join)(''));
        }, function (_) {
            return _op(xs, (0, _ramda.map)(_op2((0, _stick.concatFrom)('"'), (0, _stick.concatTo)('"'))));
        }, function (_1, ind, vs) {
            return _op(vs, (0, _ramda.join)('\n' + ind + ', '));
        }, function (_1, _2, _3, x) {
            return _op([tag, x], (0, _stick.sprintfN)('%s = [ %s ]'));
        });
    };
};

var reducer = function reducer(output, file) {
    return (0, _stick.laats)(function (_) {
        return _op(_op(file, transform), toBase64);
    }, function (base64) {
        return _op(output
        // --- xxx update
        , (0, _ramda.assoc)('base64Data')(_op(output.base64Data, (0, _stick.appendFrom)(base64))));
    });
};

var transform = function transform(filename) {
    return _op(_op(filename, getDimensions), function (_ref) {
        var _ref2 = (0, _slicedToArray3.default)(_ref, 2),
            w = _ref2[0],
            h = _ref2[1];

        return crop(filename, w, h);
    });
};

var identify = function identify(filename) {
    return sys('identify', [filename]);
};

var toBase64 = function toBase64(filename) {
    return sys('base64', ['-w', '0', filename]);
};

var crop = function crop(filename, w, h) {
    return (0, _stick.laats)(function (_) {
        return config.cropWidth;
    }, function (_) {
        return config.cropHeight;
    }, function (cw, ch) {
        return (w - cw) / 2;
    }, function (cw, ch) {
        return (h - ch) / 2;
    }, function (cw, ch, xx, yy) {
        return _op([cw, ch, xx, yy], (0, _stick.sprintfN)('%sx%s+%s+%s'));
    }, function (_1, _2, _3, _4, r) {
        return _op((0, _utilIo.tmpfile)(), (0, _ramda.tap)(function (tmp) {
            return sys('convert', [filename, '-crop', r, tmp]);
        }));
    });
};

var words = (0, _ramda.split)(/\s+/);

var getDimensions = function getDimensions(filename) {
    return _op(_op(_op(_op(_op(filename, identify), words), (0, _ramda.prop)(2)), (0, _ramda.split)('x')), (0, _ramda.map)(Number));
};

go();