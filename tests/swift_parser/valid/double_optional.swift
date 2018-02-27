// RUN: %neal-swift

    
let string: String?? = .none
if case let string?? = string {}
print(string??.count)
