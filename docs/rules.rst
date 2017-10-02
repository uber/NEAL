Tutorial: Writing a new rule
============================

Structure
---------

NEAL's `Domain Specific Language (DSL)`__ is designed to match the `Abstract Syntax Tree (AST)`__ of the program being analysed as closely as possible.

__ https://en.wikipedia.org/wiki/Domain-specific_language
__ https://en.wikipedia.org/wiki/Abstract_syntax_tree

The rules have the following format:

.. code-block:: rust

  rule RuleName {
    ProviderName::AstNode where Condition {
      AnotherAstNode where Condition {
        action()
      }
    }

    /* ... */
  }

To make it more concrete, take a look at the couple examples that follows.

Simple Example
--------------

To start with a contrived example, let's look at what the process to writing a simple rule, such as the one in the `"Getting Started" <..>`_ section, is like.

Just to recall, we want to forbid the use of `Forced-Values in Swift`__, which have the following form:

__ https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Expressions.html#//apple_ref/doc/uid/TP40014097-CH32-ID404

.. code-block:: swift

  <expression>!

In the original example we picked used ``(nil as Int?)`` as our ``<expression>``, but in order to minimise our example, let's use the simplest possible expression: an identifier.

.. code-block:: swift

  x!

The rules closely follows the AST, so let's take a look into what the AST looks like. NEAL provides a convenient way of quickly inspecting the AST for a given snippet:

.. code-block:: bash

  $ echo 'x!' | neal --stdin swift --print-ast

And the output should look something like the following [1]_:

.. code-block:: javascript

  [
    ForcedValueExpression {
      Expression = Identifier {
        Value = "x"
      }
    }
  ]

The name of the top-level node is ``ForcedValueExpression``, and that's all we need to write our rule:

We start by giving our rule a name:

.. code-block:: rust

  rule NoForcedValues { }

Now we can add a top-level matcher, that will target the ``Swift`` provider, as this is a Swift check:

.. code-block:: rust

  rule NoForcedValues {
    Swift::
  }

We add the name of the AST node we just inspected:

.. code-block:: rust

  rule NoForcedValues {
    Swift::ForcedValueExpression
  }

Now all we need to do is add an action for when this pattern is matched.

The two builtin actions are ``fail`` and ``warn``, and both of them take a explanation message. Let's use ``fail`` for this example:

.. code-block:: rust

  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`")
    }
  }

Now, if we save our rule in a ``test.rules`` file, and save our test case in a ``test.swift`` file, we can run NEAL as follows:

.. code-block:: bash

  $ neal --rules test.rules test.swift

And the output should look something like:

.. code-block:: bash

  On file test.swift: (NoForcedValues)

    1 | x!
    ~ |  ^

  error: Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`

That's it: this was the exact process we went through in order to create this rule when adding it to the Getting Started section.

.. [1] Some metadata, such as source location per node, was omitted for brevity.

Advanced Example
----------------

Given the following snippet:

.. code-block:: swift

  func expensive() {
    /* ... */
  }

  class B { /* ... */ }

  class A : B {
    override init() {
      expensive()
    }
  }

Let's create a rule that checks for subclasses of ``B`` that make to calls to ``expensive`` from initialisers.

Just like in our simpler example, we start by inspecting the AST of a snippet that violates our rule-to-be:

.. code-block:: bash

  $ echo 'class A : B { override init() { expensive() } }' | neal --stdin swift --print-ast

.. code-block:: swift

  ClassDeclaration {
    ClassName = Identifier { Value = "A" }
    TypeInheritanceClause = [
      TypeIdentifier {
        TypeName = Identifier { Value = "B" }
      }
    ]
    ClassBody = [
      InitializerDeclaration {
        Override = true
        InitializerBody = [
          CallExpression {
            Callee = Identifier { Value = "expensive" }
            Arguments = []
          }]}]}

Based on the names in the AST, what we want is: A ``ClassDeclaration``, with a ``TypeInheritanceClause`` that contains ``B``, and which has an ``InitializerDeclaration`` that contains ``CallExpression`` whose ``Callee`` is ``expensive``.

Let's start writing our rule: again we start by giving it a name and choosing a provider:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::
  }

Now, from looking at a notes above, the top node in our rule needs to be a ``ClassDeclaration`` that inherits from ``B``:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::ClassDeclaration where /* inherits from B */ {
    }
  }

We could write this condition in terms of the nodes of ``ClassDeclaration``, but luckily the Swift provider gives us an ``inheritsFrom`` helper that does just that:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::ClassDeclaration where inheritsFrom("B") {
    }
  }

Next we need to look inside the ``InitializerDeclaration``, so we add it to our rule:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::ClassDeclaration where inheritsFrom("B") {
      InitializerDeclaration {
      }
    }
  }

Now, to the part that actually violates our rule, we check that there exists a call to ``expensive``:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::ClassDeclaration where inheritsFrom("B") {
      InitializerDeclaration {
        CallExpression where Callee == "expensive" {
        }
      }
    }
  }

And last but not least, we trigger an error with some helpful message:

.. code-block:: rust

  rule NoExpensiveSubclassesOfB {
    Swift::ClassDeclaration where inheritsFrom("B") {
      InitializerDeclaration {
        CallExpression where Callee == "expensive" {
          fail("Don't call `expensive` from initialisers of subclasses of `B` because <...>. Use <...> instead.")
        }
      }
    }
  }

Now we can test our rule:

.. code-block:: bash

  $ neal --rules test.rules test.swift

.. code-block:: text

  [1 of 1]: Analysing test.swift
  On file test.swift: (NoExpensiveSubclassesOfB)

     7 | class A : B {
     8 |   override init() {
     9 |     expensive()
     ~ |               ^
    10 |   }
    11 | }

  error: Don't call `expensive` from initialisers of subclasses of `B` because <...>. Use <...> instead.

Done, now your code base is safe from expensive subclass of ``B``!
