defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import fs from 'fs'

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
    exception, raise,
    bind,
    repeat,
    tryCatch__,
} from 'stick'

import tmp from 'tmp'

import fishLib, {
    log, info, warn, error, green, yellow, magenta, red, brightRed, cyan, brightBlue, blue,
    forceColors, shellQuote, sysSpawn,
} from 'fish-lib'

export const getopt = fishLib.getopt;
export const sprintf = fishLib.sprintf;

// --- note: supplying stdin to a sync process is hard.
// --- should probably change fish-lib to make the default for stdin be pipe
// instead of process (0)
fishLib.sysSet ({ sync: true, die: true, verbose: true, })
fishLib.bulletSet ('٭')

const on = dot2 ('on')

export const sysPromise = (...args) => new Promise ((res, rej) => {
    let ret
    const sysArgs = args | concatFrom ([
        ({ out, ok, err, }) => ok | ifFalse (
            _ => "cmd failed" | exception | raise,
            _ => ret = out,
        )
    ])
    sysSpawn (...sysArgs)
    | on ('close') ((code, signal) => code === 0
        ? ret | res
        : code | 'cmd error (code = %d)' | rej
    )
    | on ('error') (rej)
})

export const sys = verbose => (...args) => {
    let ret
    sysSpawn (
        ...args,
        ({ verbose, }),
        ({ out, }) => ret = out,
    )
    return ret
}

export const writeFile = curry ((path, contents) => fs.writeFileSync (path, contents))
export const write = 'write' | bind (process.stdout)

export const appendToFile = curry ((filename, contents) => fs.appendFileSync (filename, contents))

export {
    warn, error, log,
    green, yellow, magenta, red, brightRed, cyan, brightBlue, blue,
    shellQuote,
}

export const goUp = '[A' | always

const spinner = {
    job: void 8,
    charIdx: 0,
    chars: "◓◑◒◐",
    label: '',
    lastNumChars: 0,
    cycleChar () {
        this.charIdx = ++this.charIdx % this.chars.length
    },
    str () {
        return laats (
            _ => '' | repeat (this.lastNumChars) | join (''),
            _ => this.chars [this.charIdx],
            _ => this.label,
            (pref, char, label) => [char, label, char]
                | sprintfN ('%s %s %s')
                | tap (l => this.lastNumChars = l.length)
                | concatTo (pref)
        )
        | tap (_ => this.cycleChar ())
    },
    start (label) {
        this.label = label
        this.job = setInterval (
            _ => this.str () | write,
            100,
        )
    },
    stop () {
        clearInterval (this.job)
    },
}

export const startSpinner = 'start' | bind (spinner)
export const stopSpinner = 'stop' | bind (spinner)

export const showCursor = _ => '\u001b[?25h' | write
export const hideCursor = _ => '\u001b[?25l' | write

// --- throws.
export const readdir = fs.readdirSync

export const slurp = filename => tryCatch__ (
    () => fs.readFileSync (filename).toString (),
    e => error (sprintf ("Could not read file %s: %s", brightRed (filename), e))
)

// --- throws
export const tmpfile = _ => tmp.fileSync ().name
