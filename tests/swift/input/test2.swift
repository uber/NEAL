// RUN: %not %neal %args | %check

import Foundation

class Foo {
  init() {
    let _ = DisposeBag() // CHECK-NOT: error
  }
}

class DisposeBag {}

class PlusOneCapacityRouter: ViewableRouter  {

    init() {
      let _ = DisposeBag() // CHECK: error
    }
}


let f : ViewableRouter = PlusOneCapacityRouter()
