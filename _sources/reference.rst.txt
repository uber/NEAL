Rule Language Reference
=======================

A quick reference to all the builtin functions, operators and actions.

* Functions_ are used in predicates (i.e. in the :code:`where` clause) of matchers.
* Operators_ can be used to combine multiple expressions within predicates.
* Actions_ can be taken in the body of match.
* `AST Providers`_ can also export Functions and Actions.
* `Variables & Conditions`_ can be used for expressing pre-conditions
* `Scoped Matchers`_ can be used to restrict a matcher's breadth and depth.

**Example:**

.. code-block:: swift

  Function where not(Public) && Override {
    fail("")
  }

In the sample matcher above, :code:`not` is a function, :code:`&&` is an operator and :code:`fail` is an action.

----

Functions
---------

:code:`match(Property, RegExp)`
+++++++++++++++++++++++++++++++

Tries to convert the value of :code:`Property` to a string, and matches it against the regular expression in :code:`RegExp`.

:code:`not(Expression)`
+++++++++++++++++++++++

Converts the value of :code:`Expression` to a boolean value and performs a logical negation.

:code:`count(List)`
+++++++++++++++++++

Converts the value of `List` to a list and returns the number of elements in the list. Returns :code:`0` if :code:`List` is not an actual list.

----

Operators
---------

:code:`Expression1 == Expression2`
++++++++++++++++++++++++++++++++++

Structural equality operator.

:code:`Expression1 != Expression2`
++++++++++++++++++++++++++++++++++

Negation of :code:`Expression1 == Expression2`

:code:`Expression1 && Expression2`
++++++++++++++++++++++++++++++++++

Converts both :code:`Expression1` and :code:`Expression2` to boolean values and performs a boolean "and".

:code:`Expression1 || Expression2`
++++++++++++++++++++++++++++++++++

Converts both :code:`Expression1` and :code:`Expression2` to boolean values and performs a boolean "or".

:code:`node.property`
++++++++++++++++++++++++++++++++++

Access ``property`` of the AST ``node``. If the ``node`` is actually a list of nodes, the property access works like flatMap: the property access is computed for every node in the list and then compacted into a new list.

----

Actions
-------

:code:`fail(Explanation)`
+++++++++++++++++++++++++

Reports that an error was found in the current node being matched. :code:`Explanation` should be a string explaining why is this is a violation and, ideally, how to correct it.

:code:`warn(Explanation)`
+++++++++++++++++++++++++

Similar to :code:`fail(Explanation)`, but reports a warning instead of an error.

----

AST Providers
-------------

Swift
+++++

The Swift provider only exports one convenience function:

:code:`inheritsFrom(ClassName)`

This function assumes that a class is being matched and will return :code:`true` if the string :code:`ClassName` appears in the inheritance clause of the class declaration. It returns :code:`false` if either the current node does not have a :code:`TypeInheritanceClause` (i.e. it's not a :code:`ClassDeclaration`) or if the string :code:`ClassName` does not appear in the inheritance clause.

Python
++++++

The Python provider currently does not export any function or action.

----

Variables & Conditions
----------------------

NEAL allows you to declare boolean variables inside a matcher. Those variables can be mutated from nested matchers and used in ``where`` clauses and conditions.

Declaring a new variable
++++++++++++++++++++++++

Variable declarations must be prefix with ``var`` and have a initial value assigned to it.

.. code-block:: swift
  :emphasize-lines: 2,3

  Provider::Matcher {
    var is_valid := true
    var did_match := false
  }

Mutating a variable
+++++++++++++++++++

Variables are block-scoped and can be mutated from nested matchers at any depth.

.. code-block:: swift
  :emphasize-lines: 6,10

  Provider::Matcher {
    var is_valid := true
    var did_match := false

    NestedMatcher {
      did_match := true
    }

    OtherMatcher {
      is_valid := false
    }
  }

Conditions
++++++++++

Conditions are evaluated when exiting the node and can be used in conjunction with variables to check that a series of pre-conditions within that node.

.. code-block:: swift
  :emphasize-lines: 5-7

  Provider::Matcher {
    var is_valid := true
    var did_match := false

    condition (is_valid && did_match) {
      fail("....")
    }

    NestedMatcher where is_valid {
      did_match := true
    }

    OtherMatcher {
      is_valid := false
    }
  }

----

Scoped Matchers
---------------

Any matcher can be scoped to a given property of it's parent matcher, thus restricting in breadth: instead of searching through all the properties of the current AST node, restrict it to a single property. The choice of the operator used for scoping (``>`` vs ``>>``) can be used to restrict in depth: ``>`` will match only immediate children nodes, ``>>`` will nodes at any depth.

For example, the following matcher will match ``class C { var x: String = "" }`` but not ``class C { func f() { var x: String = "" } }``

.. code-block:: swift

  Swift::Class {
    ClassBody > VariableDeclaration {
      fail("...")
    }
  }

Replacing the ``>`` operator with ``>>`` would lead to matching both cases.
