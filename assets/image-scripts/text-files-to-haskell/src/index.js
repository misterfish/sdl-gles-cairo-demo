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
    getopt,
    sprintf,
    slurp,
} from './util-io'

const config = {
    rootDir: [__dirname, '..'] | join ('/'),
    inputDir () { return [this.rootDir, 'a'] | join ('/') },
    cropHeight: 512,
    cropWidth: 512,
    // --- xxx
    tmpdir: '/tmp',
    debug: false,
    usage: 'identifier'
}

const whenNotOk = whenPredicate (ok >> not)
const ifNotOk = ifPredicate (ok >> not)

const usageF = msg => () => {
    const [_, scriptName, ...args] = process.argv
    const str = join (' ', compactOk ([scriptName, msg]))
    return sprintf ("Usage: %s", str)
}

const usage = usageF (config.usage)

const opt = getopt ({
    h:  'b',
    identifier: 's',
})

if (opt.h) {
    info (usage ())
    process.exit (0)
}

const sys = sysSpawn (config.debug)

const output = {
    slurped: [],
}

import util from 'util'
const inspect = x => util.inspect (x, { depth: null, colors: process.stdout.isTTY, })

// --- all sys commands are sync: true and die: true, so there are few or no eithers necessary.
const go = (identifier, files) => files
    | reduce (reducer) (output)
    | prop ('slurped')
    | toHaskell (identifier)
    | log

const length = prop ('length')
const lines = split ('\n')

const strRepeat = n => str => str | repeat (n) | join ('')

// --- result is slow to compile
const toHaskellMultiLineStringLong = str => str
    | lines
    | join ('\\n')
    | concatFrom ('"')
    | concatTo ('"')

// --- result is extremely slow to compile / out of memory
const toHaskellMultiLineStringMonoid = identifierLength => str => laats (
    _ => ' ' | strRepeat (identifierLength + 5),
    ind => str
        | lines
        | map (concatFrom ('\\n"') >> concatTo ('"'))
        | join (' <>\n' + ind),
)

const toHaskell = identifier => xs => laats (
    _ => identifier | length,
    l => ' ' | repeat (l + 3) | join (''),
    // l => xs | map (toHaskellMultiLineStringMonoid (l)),
    l => xs | map (toHaskellMultiLineStringLong),
    (_1, ind, vs) => vs | join ('\n' + ind + ', '),
    (_1, _2, _3, x) => [identifier, x] | sprintfN ('%s = [ %s ]')
)

const reducer = (output, file) => laats (
    _ => file | slurp,
    slurped => output | assoc ('slurped') (output.slurped | appendFrom (slurped))
)

const files = opt.argv.remain
    | tap (whenEmpty (_ => error (usage ())))

const identifier = opt.identifier | ifNotOk (
    _ => error (usage ()),
    id,
)

go (identifier, files)
