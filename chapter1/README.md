# 1. A minimum viable virtual machine

## Setup
To try it out; simply download and install [Swift](https://www.swift.org/download/) run `swift main.swift` from this directory.

## VM Design
The virtual machine we'll build is stack based, as opposed to register based.<br/>
<br/>
Each design has it's strengths and weaknesses; stacks need to be shuffled and registers allocated; but I find stacks more elegant and easy to reason about.