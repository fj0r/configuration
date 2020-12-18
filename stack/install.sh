#!/bin/bash
stack install `node -e "console.log(require('js-yaml').load(fs.readFileSync('./package.yaml', 'utf-8')).dependencies.sort().join(' '))"`