Rule Language Reference
=======================

A quick reference to all the builtin functions, operators and actions.

* Functions_ are used in predicates (i.e. in the :code:`where` clause) of matchers.
* Operators_ can be used to combine multiple expressions within predicates.
* Actions_ can be taken in the body of match.
* `AST Providers`_ can also export Functions and Actions.

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
