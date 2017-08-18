// RUN: %not %neal %args | %check

import Foundation

print(arc4random()) // CHECK: error:\d+ Explanation
