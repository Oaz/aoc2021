#!/bin/bash

DAY=D$(date +%d)

function create() {
  if [ -f $1 ]
  then
    echo "$1 exists"
    exit 1
  fi
  cat >$1
}

create ${DAY}.fs <<EOF
module aoc2021.Core.${DAY}

open System
open Tools


EOF

create ${DAY}T.fs <<EOF
module aoc2021.Core.${DAY}T

open NUnit.Framework
open ${DAY}
open Tools

let input = [
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
  "zzzzzzzzzzzz"
]

[<Test>]
let TestExample () =
  Assert.AreEqual(4, 2+2)

EOF
