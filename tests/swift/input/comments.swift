// RUN: %neal %args

/// header doc
func f() {}

// regular comment - this should be fine
func g() {}

/// another header doc
func h() {}

/*
 * this should also be fine
 */
func i() {}
