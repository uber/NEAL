Writing your own components
===========================

.. toctree::
  :hidden:
  :titlesonly:

  Introduction <self>
  reporters
  providers


NEAL is written in OCaml_, and all the plugins must have at least a thin OCaml_ compatibility layer.

Currently, there are two kinds of components:

* `Reporters <reporters.rst>`_
* `AST Providers <providers.rst>`_

Compiling and loading components
--------------------------------

Components are loaded at runtime, depending on the configurations provided to NEAL, i.e. you shouldn't need to recompile anything in order to switch or add a new AST Provider. In order to achieve that, plugins must be shared as `.cmxs` files, which are `OCaml dynamic libraries`__.

.. __: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Dynlink.html

All the components released with NEAL (and NEAL itself) are built with Ocamlbuild_, which really simplifies the build configuration. Although using Ocamlbuild to build your components is in no way required, I'll describe it here as it's standard way components are built throughout NEAL.

Building you component with Ocamlbuild_
-------------------------------------------

First, make sure you have OPAM installed:

.. code-block:: bash

  $ brew install opam

Next you'll need to install both NEAL and Ocamlfind_:

.. code-block:: bash

  opam install neal ocamlfind

Now, supposing you have your component code in a file `component.ml`, you can build you component using the following command:

.. code-block:: bash

  $ ocamlbuild -use-ocamlfind -package neal component.cmxs

Lastly make sure to add the appropriate suffix to your component as describe in the manuals for reporters and AST providers, e.g. if you are building a custom reporter:

.. code-block:: bash

  $ cp _build/component.cmxs component.reporter.cmxs

.. _OCaml: http://ocaml.org/
.. _Ocamlbuild: https://github.com/ocaml/ocamlbuild
.. _Ocamlfind: https://opam.ocaml.org/packages/ocamlfind/
