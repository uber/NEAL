// RUN: %neal %args
// RUN: %not %neal %args --strict

// NEAL shouldn't exit with error on warnings unless --strict is set

print(1)
