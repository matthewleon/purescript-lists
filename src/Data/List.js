"use strict";

var Data_List_Types = require("../Data.List.Types");

var reverseImpl = (function () {
    var go = function (acc) {
        return function (v) {
            var $tco_var_acc = acc;
            var $tco_done = false;
            while (!$tco_done) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    continue;
                };
                if (v instanceof Data_List_Types.Cons) {
                    $tco_var_acc = new Data_List_Types.Cons(v.value0, $tco_var_acc);
                    v = v.value1;
                    continue;
                };
                throw new Error("Failed pattern match at Data.List line 368, column 3 - line 368, column 19: " + [ acc.constructor.name, v.constructor.name ]);
            };
            return $tco_var_acc;
        };
    };
    return go(Data_List_Types.Nil.value);
})();

exports["reverse'"] = reverseImpl;
