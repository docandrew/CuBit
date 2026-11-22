pragma Ada_2022;
package body Walk_Proof with SPARK_Mode is
   function Exercise_Walk (Tables : Table; Root, Address, Physical_Last : Unsigned_64)
     return Result
   is
      procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64)
      is
      begin
         Word := Tables (Index) xor Table_Frame;
      end Read_Entry;
      function Walk is new Debug_View_Walk.Walk (Read_Entry);
   begin
      return Walk (Root, Address, Physical_Last);
   end Exercise_Walk;
end Walk_Proof;
