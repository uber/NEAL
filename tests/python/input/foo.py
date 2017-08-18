# RUN: %not %neal %args | %check --comment='.*#'


def foo(visibility=[]):
    pass


def apple_test(test_host_app="", labels=[]):
    pass


# CHECK: error:\d+ Must use visibility = PRIVATE instead.
foo(visibility=['PUBLIC'])

# CHECK: error:\d+ Only UI tests should have test host apps
apple_test(test_host_app="Test.app", labels=["ui"])

# CHECK-NOT: .*
