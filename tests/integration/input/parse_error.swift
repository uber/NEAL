// RUN: %neal %args
// RUN: %not %neal %args --strict-parse

// NEAL shouldn't exit with error for parse errors unless --parse-strict is set

print(1
