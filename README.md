# SketchFlow

[Check out the live app](https://dscarpetti.github.io/sketchflow/)


## Overview

SketchFlow is an experimental DSL for graph drawing. It is intended to avoid boilerplate and allow quick and easy text specification of tree and graph structures. The current implementation uses graphviz for graph drawing.

## Help

View the 'Help' tab in the running app for usage instructions

## Development

Developed using [figwheel-main](https://github.com/bhauman/figwheel-main)

---

To get an interactive development environment run:

    lein fig:build

This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

	lein clean

To create a production build run:

	lein clean
	lein fig:min


## License

Copyright © 2021 David Scarpetti

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
