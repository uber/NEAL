// RUN: %neal-swift

protocol x {
    init() async throws
}

func asyncFunc() async {}

enum TestError: Error {
    case invalid
}

func asyncFuncInt() async throws -> Int {
    throw TestError.invalid
    return 2
}

class AsyncClass {
    init() async {

    }
}