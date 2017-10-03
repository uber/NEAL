// RUN: %not %neal %args | %check

class C {
  let const = {
    // CHECK: Child
    // CHECK: Descendant
    self.crash()
  }

  init() {
    func f() {
      let const = {
        // CHECK-NOT: Child
        // CHECK: Descendant
        self.crash()
      }
    }
  }
}
