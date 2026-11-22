--  A SPARK instance of Debug_View_Walk.Walk, so gnatprove proves the
--  generic body (generics are proved through their instances). The tables
--  are arbitrary: every entry the walk reads is Tables (Index) xor its
--  table's frame, for any Tables.
pragma Ada_2022;
with Interfaces; use Interfaces;
with Debug_View_Walk; use Debug_View_Walk;
package Walk_Proof with SPARK_Mode is
   type Table is array (Table_Index) of Unsigned_64;
   function Exercise_Walk (Tables : Table; Root, Address, Physical_Last : Unsigned_64)
     return Result
     with Post =>
       (if Exercise_Walk'Result.Kind = Viewable then
          Address < User_Limit
          and then RAM_Page (Exercise_Walk'Result.Frame, Physical_Last)
          and then (Exercise_Walk'Result.Leaf and Required) = Required
          and then Write_Back (Exercise_Walk'Result.Leaf, Exercise_Walk'Result.Large)
        else Exercise_Walk'Result.Frame = 0);
end Walk_Proof;
