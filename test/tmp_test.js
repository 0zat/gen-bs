// Generated by BUCKLESCRIPT VERSION 1.8.1, PLEASE EDIT WITH CARE
'use strict';


function test(arr) {
  return arr.map((function (x) {
                  return x + 1 | 0;
                })).forEach((function (x) {
                console.log(x);
                return /* () */0;
              }));
}

exports.test = test;
/* No side effect */