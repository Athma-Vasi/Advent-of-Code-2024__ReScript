// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";

function hystorianHysteria(locationIds1, locationIds2) {
  var sortedAsc1 = locationIds1.toSorted(Caml.int_compare);
  var sortedAsc2 = locationIds2.toSorted(Caml.int_compare);
  var _totalDistance = 0;
  var _index = 0;
  while(true) {
    var index = _index;
    var totalDistance = _totalDistance;
    if (index === sortedAsc1.length) {
      return totalDistance;
    }
    var id = sortedAsc1.at(index);
    var smallId1 = id !== undefined ? id : -1;
    var id$1 = sortedAsc2.at(index);
    var smallId2 = id$1 !== undefined ? id$1 : -1;
    var diff = smallId1 - smallId2 | 0;
    var absDiff = diff < 0 ? Math.imul(diff, -1) : diff;
    _index = index + 1 | 0;
    _totalDistance = totalDistance + absDiff | 0;
    continue ;
  };
}

var l1 = [
  3,
  4,
  2,
  1,
  3,
  3
];

var l2 = [
  4,
  3,
  5,
  3,
  9,
  3
];

var r1 = hystorianHysteria(l1, l2);

console.log("r1: ", r1);

export {
  hystorianHysteria ,
  l1 ,
  l2 ,
  r1 ,
}
/* r1 Not a pure module */
