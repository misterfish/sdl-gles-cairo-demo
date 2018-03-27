defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import {
  prop, fromPairs, map, identity, tap, curry, compose,
  values, addIndex,
  complement,
  clone, assoc,
  equals, not,
  either, T,
  always,
  isEmpty,
  isNil,
} from 'ramda'

import {
  ok, mergeFrom, whenOk, dot, dot1, xMatch,
  ifPredicate, whenPredicate,
  eq, lt, gt,
  ifFalse, ifTrue, noop,
  concatFrom,
  cond, otherwise, guard,
  sprintf1,
  laatStar as laats, blush,
  callUnder, dot2,
  exception, raise,
  invoke,
  defaultTo,
} from 'stick'

import { fromJS, OrderedSet, } from 'immutable'
export { fromJS, }

export const toJS = dot ('toJS')
export const sortBy = dot1 ('sortBy')
export const push = dot1 ('push')
export const add = dot1 ('add')
export const find = dot1 ('find')
export const del = dot1 ('delete')

export const set = dot2 ('set')
export const setIn = dot2 ('setIn')

export const update = dot2 ('update')
export const updateIn = dot2 ('updateIn')

export const size = prop ('size')

export const valueSeq = dot ('valueSeq')
export const get = dot1 ('get')
export const getIn = dot1 ('getIn')

export const filter = dot1 ('filter')

export const listToOrderedSet = xs => OrderedSet.of (...xs)

export const entrySeq = dot ('entrySeq')
export const toOrderedMap = dot ('toOrderedMap')
