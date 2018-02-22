// RUN: %not %neal %args | %check

// swiftlint:disable:next custom_rules
print("")

x! // CHECK: error: No force unwrap
