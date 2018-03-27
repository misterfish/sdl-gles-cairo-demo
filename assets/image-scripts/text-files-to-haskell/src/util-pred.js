defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import ramda, {
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
    dot, dot1, dot2, nieuw, nieuw1, nieuw2,
    cond, guard, otherwise,
    sprintf1, sprintfN, times, range, rangeBy,
    noop, doe, blush,
    concatTo, concatFrom, appendTo, appendFrom, appendToMut,
    invoke, applyN, pass1,
    laat, laatDat, laatStar as laats, laatStarDat,
    compactOk, compact,
    gt, eq, ne,
    callUnder,
} from 'stick'

// --- beware circular reference: don't use these in point-free functions.
import {
    getType,
    allOk,
    length,
} from './util'

import {
    isJust, toJust, isLeft,
} from './util-bilby'

export const isType = curry ((type, val) => getType (val) | equals (type))
export const isArray = isType ('Array')

export const ifLongerThan = n => ifPredicate (length >> gt (n))

export const isException = isType ('Error')
export const ifException = ifPredicate (isException)
export const ifNegativeOne = ifPredicate (-1 | eq)

// --- ugly (...args), but avoiding circular refs.
export const ifSingletonLeft = (...args) => ifPredicate (allPass ([isArray, length >> eq (1), prop (0) >> isLeft])) (...args)
export const ifAllOk = (...args) => ifPredicate (allOk) (...args)
