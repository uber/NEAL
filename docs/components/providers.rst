Writing a custom AST Provider
=============================

AST Providers in NEAL are responsible for adding support for a new language. They must conform to a small interface, and have complete ownership over the analysis to be done.

They can either do take care of parsing internally, e.g. using a parser written in OCaml (like the `Swift provider`_) or call out to an external parser (like the `Python provider`_).

Either way, they must conform to the following interface:

.. code-block:: ocaml

  module type PROVIDER = sig val name: string
    val extensions: string list
    val parse: filename -> source -> Absyn.absyn
    val exports : (function_name * exported_function) list
  end

With the following type aliases:

.. code-block:: ocaml

  type exported_function = Ctx.ctx -> Rule.literal list -> Rule.literal
  type filename = string
  type source = string
  type function_name = string

Using a custom provider
-----------------------

* Custom providers must be compiled to a ``.cmxs`` file, which is OCaml's dynamic library format (as described `here <../components#plugins>`_);
* They must have ``.provider`` suffix before the ``.cmxs`` extension, e.g. ``swift.provider.cmxs``;
* You must tell NEAL the path to your provider (using the ``-p``/``--provider`` `configuration flag <../configuration.html#flags>`_).

After that NEAL should already be able to parse files using your new provider and evaluate rules that target it.

## Examples

Here we'll use the two providers currently available with NEAL to illustrate the two use cases:

`Swift provider`_
+++++++++++++++++

Here's the current implementation (we'll discuss it below):

.. code-block:: ocaml

  Provider.register(module struct
    let name = "Swift"
    let extensions = [".swift"]
    let parse filename source = Parser.parse filename source
    let exports = [
      ("inheritsFrom", inherits_from)
    ]
  end)

What this provider is saying is:
* Its name is `Swift`, i.e. the provider name you'll use in a `rule <../rules.html>`_ to reference to this provider is `Swift`.
* It can parse files that have the `.swift` extension. (It should always include the `.`)
* Parsing is delegate to `Parser.parse`, which in this case is a Swift parser implemented in OCaml.
* It exports a function called `inheritsFrom`, and its implementation is the OCaml function `inherits_from`. (Which should have the `exported_function` type from above.)

`Python provider`_
+++++++++++++++++++

The actual Python provider module looks very similar to the Swift one:

.. code-block:: ocaml

  let () = Provider.register(module struct
    let name = "Python"
    let extensions = [".py"]
    let parse = parse
    let exports = []
  end)

The main different is the ``parse`` function. Here, instead of actually parsing the file and generating the AST all in OCaml, it calls out to a script which uses the parser built into the python interpreter to generate the AST and dumps it as JSON.

.. code-block:: ocaml

  let exec_name = Neal.Utils.relative_path "providers/helpers/dump_python_ast.py"

  let parse filename _ =
    let cmd = Printf.sprintf "%s %s" exec_name filename in
    let stdout = Unix.open_process_in cmd in
    let json = Yojson.Safe.from_channel stdout in
    let _ = Unix.close_process_in stdout in
    absyn_of_json json

The implementation of ``absyn_of_json`` simply takes the JSON parsed using yojson_ and converts it to NEAL's AST type. You can check the `source code <Python provider>`_, but here will ignore it as an implementation detail.

.. _`Swift provider`: https://github.com/uber/NEAL/blob/master/src/providers/swift/swift_provider.ml
.. _`Python provider`: https://github.com/uber/NEAL/blob/master/src/providers/python/python.ml
.. _yojson: https://github.com/mjambon/yojson
