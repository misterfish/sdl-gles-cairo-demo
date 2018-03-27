defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import ramda, {
    findIndex,
    always, not,
    any, all, allPass, anyPass,
    isEmpty, tap, has, hasIn, flip, fromPairs, toPairs, assoc, assocPath, head,
    tail, reduceRight, chain, identity as id, reduce, map, filter, reject, join,
    split, splitAt, prop, curry, zip, contains,
    forEach as each, forEachObjIndexed as eachObj, complement,
    isNil, addIndex, take, equals, mapAccum, compose, append, concat,
    T, F, repeat as rRepeat, times as rTimes,
} from 'ramda'

import {
    ok, ifOk, ifTrue, ifFalse, ifYes, ifNo, ifPredicate, ifEmpty,
    whenOk, whenTrue, whenFalse, whenYes, whenNo, whenPredicate, whenEmpty,
    dot, dot1, dot2, tapDot2, nieuw, nieuw1, nieuw2,
    cond, guard, otherwise,
    sprintf1, sprintfN, times, range, rangeBy,
    noop, doe, blush,
    concatTo, concatFrom, appendTo, appendFrom, appendToMut,
    invoke, applyN, pass1,
    laat, laatDat, laatStar as laats, laatStarDat,
    compactOk, compact,
    gt, eq, ne,
    callUnder,
    repeat,
} from 'stick'

// --- beware circular reference: don't use these in point-free functions.
import {
    ifLongerThan, ifNegativeOne,
} from './util-pred'

import {
    isJust, toJust,
} from './util-bilby'

export const getType = callUnder ({}.toString) >> dot2 ('slice') (8, -1)

export const then = dot1 ('then')
export const recover = dot1 ('catch')

export const minus = curry ((a, b) => b - a)

// ------ stick extensions

const removeSpaces = dot2 ('replace') (/\s+/g) ('')

// --- beware, overwrites any flags that the re already had.
export const xRegExpFlags = (re, flags) => new RegExp (
    re.source | removeSpaces,
    flags,
)

export const xMatchGlobal = curry ((re, mapper, target) => {
    let out = []
    const reGlobal = xRegExpFlags (re, 'g')
    let m
    while (m = reGlobal.exec (target))
        mapper (m) | appendToMut (out)
    return out
})

export const findPredOk = curry ((pred, xs) => {
    for (const x of xs) {
        const p = pred (x)
        if (ok (p)) return p
    }
})

export const findPredOkGen = curry ((pred, gen) => {
    let n
    while (! (n = gen.next ()).done) {
        const p = pred (n.value)
        if (ok (p)) return p
    }
})

export const findPredMaybeGen = curry ((pred, gen) => {
    let n
    while (! (n = gen.next ()).done) {
        const p = pred (n.value)
        if (p | isJust) return p | toJust
    }
})

export const tryCatchO = curry ((whatToTry, howToCatch, val) => {
    try { return whatToTry (val) }
    catch (e) { return howToCatch (e, val) }
})

export const length = prop ('length')

export const mapX = map | addIndex

// ------ lazyfish extensions
export const lazyFindPred = curry ((pred, lxs) => {
    while (true) {
        const { value, done, } = lxs.next ()
        if (done) break
        const predVal = pred (value)
        if (predVal) return predVal
    }
})

export const guardA = always >> guard

export const divideBy = curry ((a, b) => b / a)

export const substring = dot2 ('substring')

export const ellipsisAfter = curry ((n, s) => s | ifLongerThan (n) (
    substring (0, n) >> concatFrom ('…'),
    id,
))

// --- only own (R.toPairs and R.map are like this too)
// --- order: k, v
export const mapObj = curry ((f, o) => {
    const ret = []
    for (const i in o) {
        ret.push (f (i, o[i]))
    }
    return ret
})

export const allOk = x => x
    | findIndex (ok >> not)
    | ifNegativeOne (T, F)

const precatTo = concatFrom

// --- doesn't truncate if too long.
export const padTo = curry ((n, str) => laats (
    _ => str.length,
    (l) => str | cond ([
        (_ => l >= n) | guardA (str),
        otherwise | guard (x => x | (repeat (n - l) (' ') | join ('') | precatTo)),
    ])
))

export const on = tapDot2 ('on')

