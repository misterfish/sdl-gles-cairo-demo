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
    bind,
} from 'stick'

import bilby, {
    left as Left,
    right as Right,
    some as Just,
    none as Nothing,
} from 'bilby'

export {
    Left, Right,
    Just, Nothing,
}

export const isLeft = prop ('isLeft')
export const fold = dot2 ('fold')
export const flatMap = dot1 ('flatMap')

export const toEither = curry ((l, o) => o | ifOk (
    Right,
    l | Left | always,
))

export const isJust = prop ('isSome')
export const toJust = fold (
    id, noop,
)

const colon = curry ((x, xs) => [x, ...xs])
const liftA2 = 'liftA2' | bind (bilby)

export const sequenceM = (pure) => {
    const _sequence = xs => xs.length === 0
        ? ([] | pure)
        : liftA2 (colon, xs | head, xs | tail | _sequence)
    return _sequence
}

export const cata = dot1 ('cata')
