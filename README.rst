NEAL
====

**NEAL** (**N**\ ot **E**\ xactly **A** **L**\ inter) is a language-independent code analysis tool that aims to enable more people to write quality enforcement rules.

Example
-------

One of the simplest rules we have at Uber is to restrict the use of `Forced-Values in Swift`__. (`This post`__ explains some of the risks of abusing forced-values.)

__ https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Expressions.html#//apple_ref/doc/uid/TP40014097-CH32-ID404
__ https://www.andrewcbancroft.com/2016/06/13/force-unwrapping-swift-optionals-code-smell/

A forced-value consists of any expression that results in an optional value followed by the forced-unwrapping operator (``!``). Here's a contrived example that would result in a runtime crash.

.. code-block:: swift

  // test.swift
  (nil as Int?)!

NEAL doesn't have any rules built-in, so we have to write a new rule to detect this pattern:

.. code-block:: cpp

  // test.rules
  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("No forced unwrapping allowed")
    }
  }

Now if you run NEAL you should see something like the following:

.. code-block:: bash

  $ neal --rules test.rules test.swift

  [1 of 1]: Analysing test.swift
  On file test.swift: (NoForcedValues)

    1 | (nil as Int?)!
    ~ |              ^

  error: No forced unwrapping allowed

Alternatively you can create a minimal configuration file as follows:

.. code-block:: json

  {
    "rules": [ "test.rules" ]
  }

After that you can get the same result by simply running:

.. code-block:: bash

  $ neal .

For a more comprehensive guide to writing your own rules check out `Writing a new rule <docs//rules.rst>`_.

Installation
------------

To build and install NEAL from source, make sure you have `OPAM`__ installed and that you're using OCaml 4.04 or later.

.. code-block:: bash

  $ brew install opam
  $ opam init
  $ opam switch 4.04.2
  $ eval "$(opam config env)"

__ https://opam.ocaml.org/

After you have setup OPAM and OCaml, you can setup, build and install NEAL using ``make``.

.. code-block:: bash

  $ NATIVE=1 make setup build install

Basic usage
-----------

NEAL has builtin support for Python and Swift, but it's highly extensible, and can be used with any language.

.. code-block:: bash

  $ neal [options] [path to files or directories]

For a list of CLI options, runtime options and configuration attributes see the `Configuration <configuration.html>`_ guide.

.. _brew: https://brew.sh

.. toctree::
  :hidden:
  :titlesonly:

  Getting Started <self>
  rules
  testing_rules
  reference
  configuration
  basics
  components/index
  developing
  changelog
