--  What the language's own words do, as a front end shows them beside a
--  completion or while a call is typed: the call's shape, its type (the
--  schemes of docs/ccl-type-system.md, section 5.5) and one line of what it
--  does. Host operations show their catalog signature instead.
package CCL.Hints with SPARK_Mode is
   --  The hint for a built-in, special form or operator; "" for any other name.
   function Hint (Name : String) return String;
end CCL.Hints;
