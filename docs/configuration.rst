Configuration
=============

Configuration File
------------------

NEAL allows you to put all the configuration for your project in one place by using a configuration file, `neal.json`. The options available for the configuration file are:

========================================= =========================================================================================================================================
Flag                                      Value
========================================= =========================================================================================================================================
`rules`                                   A list of paths to NEAL files
`glob`                                    A list of strings containing regular expressions that will match the files that should be analysed by NEAL
`exclude`                                 A list of strings containing regular expressions that will match the files that should be ignored by NEAL
`provider-map`                            A object where the keys are the names of the providers and the values is like `glob`, matching files that should be handled by that provider
`rule-map`                                A list of objects containing `rules`, `glob` and `exclude`. It allows for multiple configurations in a single file. Each file to be analysed will be tested against each `glob` of each entry. The rules of the first entry to have a matching `glob` will be used.
========================================= =========================================================================================================================================

Example Configuration files
---------------------------

Basic
+++++

Only specify the set of rules used in your project and some files to ignore:

.. code-block:: json

  {
    "rules": [
      "./neal/Python.rules",
      "./neal/Swift.rules",
      "./neal/directory/with/many/rules"
    ],
    "exclude": [
      "./vendor"
    ]
  }

Complete
++++++++

Using `rule-map` and `provider-map`:

.. code-block:: json

  {
    "provider-map": {
      "Python": [ "BUCK$" ]
    },
    "rule-map": [{
      "glob": [ "folder/with/libraries" ],
      "rules": [ "./neal/Libraries.rules" ],
      "ignores": [ "folder/with/libraries/vendor/*.py" ]
    }, {
      "glob": [ "./app", "./framework" ],
      "rules": [ "./neal/App.rules" ]
    }, {
      "rules": [ "./neal/Default.rules" ]
    }]
  }

Disabling rules inline
----------------------

You can use comments to disable NEAL rules. The comments must have the prefix ``NEAL:`` and can specify the rules to be disabled (or all the rules), the context (the whole file or a given line) and must provide an explanation.

Examples
++++++++

* Disabling a specific set of rules for a given line

.. code-block:: c

  // NEAL: skip RuleA, RuleB and RuleC on the next line because it will be fixed in #123
  TriggerRuleA(); TriggerRuleB(); TriggerRuleC();

  TriggerRuleA(); TriggerRuleC(); // NEAL: skip RuleA and RuleC on this line because it will be fixed in #412

  TriggerRuleB();
  // NEAL: skip RuleB on the previous line because it will be fixed on #705

* Disabling all rules for a given line: (all of the following are equivalent)

.. code-block:: c

  // NEAL: skip all rules on the next line because it will be fixed on #123
  lint_error(); // NEAL: skip all rules on this line because it will be fixed on #123
  // NEAL: skip all rules on the previous line line because it will be fixed on #123

* Disabling a set of rules for a given file

.. code-block:: c

  // NEAL: skip RuleA, RuleB and RuleC on this file because this file is auto generated.

* Disabling all rules for a given file

.. code-block:: c

  // NEAL: skip all rules on this file because this file is auto generated.

Runtime flags
-------------

To enable any of the following flags just set the environment variable with the name of the flag preceded by ``NEAL_`` on your process or when you run NEAL, e.g.

.. code-block:: bash

  $ NEAL_STATS=1 neal test


================ ==============================================================================================================
Flag             Description
================ ==============================================================================================================
DEBUG            Turn on debug logs (e.g. AST traversal logs, provider selection, etc.)
STATS            Output statistics for the run once it finishes (e.g. Number of files parsed, number of  parsing failures, etc.)
PARSING_FAILURES Print all the path of all the files that failed to parse
================ ==============================================================================================================
