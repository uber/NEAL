Contributing
============

Building from source
--------------------

Setup
+++++

First, make sure you have `Ocaml Package Manager (OPAM) <OPAM>`_ installed

.. code-block:: bash

  $ brew install opam

Before you use OPAM for the first time you need to initialise it by running

.. code-block:: bash

  $ opam init

OPAM you give you the option to add the configuration to your bashrc/zshrc so you have it in every session of your terminal. You can also just manually run the command

.. code-block:: bash

  $ eval $(opam config env)

Before building the project, you can run the setup script to make sure the right version of the compiler and all the dependencies are installed

.. code-block:: bash

  $ make setup

Building and running
++++++++++++++++++++

To build the project you can use make

.. code-block:: bash

  $ make build # build is also the default action

By default, Neal will be compiled to bytecode and the executable will be available at `neal/main.bte`. You can also compile Neal to a native binary using

.. code-block:: bash

  $ NATIVE=1 make build

Tests
+++++

In order to run the tests you can also use `make`

.. code-block:: bash

  $ make test

The default action will run all the tests, which include Neal's end-to-end tests and the Swift parser tests.

Alternatively you can run the coverage target, which runs the tests and at the end will generate a test coverage report

.. code-block:: bash

  $ make coverage
  $ open neal/_build/coverage/index.html

.. _OPAM: https://opam.ocaml.org/
