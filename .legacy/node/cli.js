#!/usr/bin/env node
"use strict";

let repl = require('repl')
let r = repl.start('|> ')
let c = r.context
let R = require('ramda')
let most = require('most')

c.repl = { repl, r, c }

c.most = most
c.R = R
c.faker = require('faker')
c.fetch = require('isomorphic-fetch')
c.moment = require('moment')
c.fs = require('fs')
c.path = require('path')
c.os = require('os')
c.util = require('util')
c.C = require('child_process')
c.B = require('bignumber.js')
c.docker = new require('dockerode')({socketPath: '/var/run/docker.sock'})
//c.jwt = require('jsonwebtoken')
c.readFilePromise = (filename) => {
  return new Promise((res, rej)=>{
    c.fs.readFile(filename, 'utf-8', (err, f)=>res(f))
  })
}
c.ll = function (prefix, func){
  return R.pipe(
    R.tap(function(s){
      console.log.call(
        console,
        ['【', prefix, '】', ' :: ', typeof s].join(''))
    }),
    R.tap(function(s){console.log.call(console, s)}),
    R.tap(func ? R.compose(console.log.bind(console), func) : R.identity)
  )
}
c.l = console.log.bind(console)

Promise.factory = f => (...params) => new Promise((res, rej) => {
    f(...params, (err, data) => {
      if (err !== null) return rej(err)
      res(data)
    })
  })

Promise.factoryFromMethod = (o,m) => Promise.factory(o[m].bind(o))

Promise.prototype.toObs = function(){ return most.fromPromise(this) }

c.s1 = [{a:1,b:2,c:3},{a:2,b:3,c:4},{a:3,b:4,c:5}]

