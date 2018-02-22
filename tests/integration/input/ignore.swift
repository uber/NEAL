// RUN: %not %neal %args | %check

// NEAL: skip X, Print and Z on this file because I need to test it
// NEAL: skip X and Y on this file because I need to test it
// NEAL: skip X on this file because I need to test it

print("")

x! // NEAL: skip all rules on this line because I need to test it

// NEAL: skip X and Bang on the next line because I need to test it
x!

x!
// NEAL: skip X, Y and Bang on the previous line because I need to test it

x! // CHECK: error: No force unwrap
