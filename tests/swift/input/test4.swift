// RUN: %not %neal %args | %check

// CHECK: error:\d+ Do not include 'created by' comments
// Created By Tadeu Zagallo

// CHECK: error:\d+ Do not include 'created by' comments
/* Created By Tadeu Zagallo */

// CHECK: error:\d+ Do not include 'created by' comments
/**
 * Created By Tadeu Zagallo
 **/
