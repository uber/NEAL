How it works?
=============

The two key principles of NEAL's design are **composability** and **flexibility**.

**Composability**: Since it's just impossible for NEAL to have built-in support for every language out there, it has to be highly composable. In order to achieve this level of composition, language support is handled by components in NEAL, and this components are called *AST providers* (or *providers* for short).

**Flexibility**: On the other hand, at the same time that it'd be great to be effortless to add a new language, this should compromise NEAL's analysis capabilities. In order to achieve this level of flexibility, the complexity of the analysis one can perform using NEAL should only be bound by the *provider* it targets, and not the platform itself.

## Architecture

In order to keep the whole system highly composable, it's broken into four types on components:

* Driver (Core): The core piece of NEAL, responsible for controlling the overall execution and selecting the necessary components.
* AST Providers:
* Rules:
* Reporters:

Here's a small diagram illustrating how all of the components interact with each other:

.. image:: imgs/neal_diagram.png
  :alt: Diagram of the interactions between all of NEAL's components
