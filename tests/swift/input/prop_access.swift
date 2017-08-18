// RUN: %not %neal %args | %check

class C : Bar {
  init() {
    foo() // CHECK: error:\d+ Explanation
  }
}
