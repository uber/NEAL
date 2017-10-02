Testing your rules
==================

When writing rules, it's good to be sure that they'll actually match what you expect, and sometimes also good to check that they won't report false positives in some edge cases. In order to validate that, NEAL provides some builtin facilities for testing rules.

Example
-------

Let's write a couple tests for one of the rules we wrote in the `tutorial <rules>`_.

ForcedValueExpression
+++++++++++++++++++++

Here's the rule that we wrote:

.. code-block:: swift

  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`")
    }
  }

The first thing that we need to do is add a :code:`tests` block to our rule.

.. code-block:: swift

  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`")
    }

    tests {
    }
  }

Now let's write a simple test, in order to make sure that it reports violations correctly.

.. code-block:: swift

  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`")
    }

    tests {
      should_report: expect_fail("x!")
    }
  }

We give the test a name, :code:`should_report`, and write a expectation that the test should report a failure for the input :code:`"x!"`.

We can also add a positive test:

.. code-block:: swift

  rule NoForcedValues {
    Swift::ForcedValueExpression {
      fail("Force unwrapping optionals is not allowed. Please refactor your code to use `?` instead of `!`")
    }

    tests {
      should_report: expect_fail("x!")
      should_pass: expect_ok("x != y")
    }
  }

Here, again, we give a name to the test, :code:`should_pass`, and this time we use :code:`expect_ok` to check that no violation will be reported for the input :code:`"x != y"`.

Running the tests
-----------------

You can save the file rule above in a file name, e.g. :code:`Swift.rules`, and then you can run the tests with:

.. code-block:: bash

  neal test Swift.rules

And the result should look something like the following.

.. code-block:: text

  ✓ Test `should_report' of rule `NoForcedValues' passed
  ✓ Test `should_pass' of rule `NoForcedValues' passed

  ✓ 2 tests passed, ⅹ 0 tests failed, ! 0 tests errors

If we change the input in the :code:`should_report` test from :code:`"x!"` to :code:`"x"` and re-run the test, it should now report an error as following.

.. code-block:: text

  ⅹ Test `should_report' of rule `NoForcedValues' failed:
          Expected 1 violations, but found 0
  ✓ Test `should_pass' of rule `NoForcedValues' passed

  ✓ 1 test passed, ⅹ 1 test failed, ! 0 tests errors
