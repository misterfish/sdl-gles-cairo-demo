#!/usr/bin/env node

defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import ramda, {
    always, not,
    either, both,
    any, all, allPass, anyPass,
    isEmpty, tap, has, hasIn, flip, fromPairs, toPairs, assoc, assocPath, head,
    tail, reduceRight, chain, identity as id, reduce, map, filter, reject, join,
    split, splitAt, prop, curry, zip, contains,
    forEach as each, forEachObjIndexed as eachObj, complement,
    isNil, addIndex, take, equals, mapAccum, compose, append, concat,
    T, F, repeat as rRepeat, times as rTimes,
    range,
} from 'ramda'

import fishLib, {
    log, info, warn, error, green, yellow, magenta, brightRed, cyan, brightBlue,
    sprintf, forceColors, getopt, shellQuote,
} from 'fish-lib'

import {
    ok, ifOk, ifTrue, ifFalse, ifYes, ifNo, ifPredicate, ifEmpty,
    whenOk, whenTrue, whenFalse, whenYes, whenNo, whenPredicate, whenEmpty,
    dot, dot1, dot2, nieuw, nieuw1, nieuw2,
    cond, guard, otherwise,
    sprintf1, sprintfN, times, rangeBy,
    noop, doe, blush,
    concatTo, concatFrom, appendTo, appendFrom, appendToMut,
    invoke, applyN, pass1,
    laat, laatO, laats, laatsO,
    compactOk, compact,
    lt, gt, eq, ne, lte, gte,
    xReplace,
    prependTo,
    tryCatch__,
} from 'stick'

import fs from 'fs'
const slurp = filename => tryCatch__ (
    () => fs.readFileSync (filename).toString (),
    e => error (sprintf ("Could not read file %s: %s", brightRed (filename), e))
)

const go = (x, y) => fn => fn
    | slurp
    | xReplace (/ (<path \s+ id= "[^"]+") .+ /) ((_, g1) => g1 | concatFrom ([x, y] | sprintfN (' transform="translate(%d, %d)"')))
    | tap (fn | write)

const write = fn => c => fs.writeFileSync (fn, c)

range (1, 10)
    | map (x => [__dirname, x] | sprintfN ('%s/../path%02d.svg'))
| map (go (-130, 0))
