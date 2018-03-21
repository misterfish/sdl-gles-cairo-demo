#!/usr/bin/env node

defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

// --- add this to the patterns.
// script with sys commands, sync & die, no flatmap.

import ramda, {
    always, not,
    either, both,
    any, all, allPass, anyPass,
    isEmpty, tap, has, hasIn, flip, fromPairs, toPairs, assoc, assocPath, head,
    tail, reduceRight, chain, identity as id, reduce, map, filter, reject, join,
    split, splitAt, prop, curry, zip, contains,
    forEach as each, forEachObjIndexed as eachObj, complement,
    isNil, addIndex, take, equals, mapAccum, compose, append, concat,
    T, F, repeat as rRepeat, times as rTimes, range,
} from 'ramda'

import {
    ok, ifOk, ifTrue, ifFalse, ifYes, ifNo, ifPredicate, ifEmpty,
    whenOk, whenTrue, whenFalse, whenYes, whenNo, whenPredicate, whenEmpty,
    dot, dot1, dot2, nieuw, nieuw1, nieuw2,
    cond, guard, otherwise,
    raise, exception,
    sprintf1, sprintfN, times, rangeBy,
    noop, doe, blush,
    concatTo, concatFrom, appendTo, appendFrom, appendToMut,
    prependFrom,
    invoke, applyN, pass1,
    laat, laatO, laats, laatsO,
    compactOk, compact,
    lt, gt, eq, ne, lte, gte,
    prependTo,
    repeat,
} from 'stick'

import {
    toEither, flatMap,
    fold,
    Left, Right,
} from './util-bilby'

import {
    info, error, readdir, log,
    sys as sysSpawn, tmpfile,
} from './util-io'

const config = {
    rootDir: [__dirname, '..'] | join ('/'),
    inputDir () { return [this.rootDir, 'a'] | join ('/') },
    cropHeight: 512,
    cropWidth: 512,
    // --- xxx
    tmpdir: '/tmp',
    tag: 'movieMock',
    debug: false,
}

const sys = sysSpawn (config.debug)

const output = {
    base64Data: [],
}

const takePartial = config.debug | ifTrue (
    3 | take | always,
    id | always,
)

// --- all sys commands are sync: true and die: true, so there are few or no eithers necessary.
const go = _ => laats (
    _ => config.inputDir (),
    dir => dir
        | readdir
        | takePartial
		| map (appendTo ([dir]) >> join ('/'))
        | reduce (reducer) (output)
		| prop ('base64Data')
        | toHaskell (config.tag)
        | log
)

const length = prop ('length')

const toHaskell = tag => xs => laats (
    _ => (tag | length) + 3,
    l => ' ' | repeat (l) | join (''),
    _ => xs | map (concatFrom ('"') >> concatTo ('"')),
    (_1, ind, vs) => vs | join ('\n' + ind + ', '),
    (_1, _2, _3, x) => [tag, x] | sprintfN ('%s = [ %s ]')
)

const reducer = (output, file) => laats (
    _ => file | transform | toBase64,
	base64 => output
		// --- xxx update
		| assoc ('base64Data') (output.base64Data | appendFrom (base64))
	)

const transform = filename => filename
    | getDimensions
    | (([w, h]) => crop (filename, w, h))

const identify = filename => sys (
    'identify',
    [filename],
)

const toBase64 = filename => sys (
    'base64',
    ['-w', '0', filename],
)

const crop = (filename, w, h) => laats (
    _ => config.cropWidth,
    _ => config.cropHeight,
    (cw, ch) => (w - cw) / 2,
    (cw, ch) => (h - ch) / 2,
    (cw, ch, xx, yy) => [cw, ch, xx, yy] | sprintfN (
        '%sx%s+%s+%s'
    ),
    (_1, _2, _3, _4, r) => tmpfile ()
        | tap (tmp => sys (
            'convert',
            [filename, '-crop', r, tmp],
        ))
)

const words = split (/\s+/)

const getDimensions = filename => filename
    | identify
    | words
    | prop (2)
    | split ('x')
    | map (Number)

go ()
