// RUN: %not %neal %args | %check

// NEAL: runonly X, Bang and NSLog on this file because I need to test it

print("")

x! // CHECK-NEXT-L: error: No force unwrap

// NEAL: runonly X on the next line because I need to test it
x!

x! // CHECK-NEXT-L: error: No force unwrap
// NEAL: runonly Bang on the previous line because I need to test it

NSLog("astr") // CHECK-NEXT-L: warning: Don't call `NSLog`

// NEAL: runonly Bang on the next line because I need to test it
NSLog("astr") // CHECK-NOT-L: warning: Don't call `NSLog`
