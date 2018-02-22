// RUN: %not %neal %args | %check

// CHECK: error: Do not include 'created by' comments
// Created By Tadeu Zagallo

// CHECK: error: Do not include 'created by' comments
/* Created By Tadeu Zagallo */

// CHECK: error: Do not include 'created by' comments
/**
 * Created By Tadeu Zagallo
 **/
