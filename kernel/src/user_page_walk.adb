package body User_Page_Walk with SPARK_Mode is
   generic
      with procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64);
      Required : Unsigned_64;
   function Walk (Root, Address, Physical_Last : Unsigned_64) return Unsigned_64
     with Post => (if Walk'Result /= 0 then
       Address < User_Limit and then RAM_Page (Walk'Result, Physical_Last));

   function Walk (Root, Address, Physical_Last : Unsigned_64)
     return Unsigned_64
   is
      Table_Frame : Unsigned_64 := Root;
      Word, Frame : Unsigned_64;
   begin
      if Address >= User_Limit or else not RAM_Page (Root, Physical_Last) then
         return 0;
      end if;
      for L in Level loop
         pragma Loop_Invariant (RAM_Page (Table_Frame, Physical_Last));
         declare
            Shift : constant Natural :=
              (case L is when P4_Level => 39, when P3_Level => 30,
                         when P2_Level => 21, when P1_Level => 12);
            Index : constant Table_Index := Natural (Shift_Right (Address, Shift) and 511);
         begin
            Read_Entry (Table_Frame, Index, Word);
         end;
         if (Word and Required) /= Required then
            return 0;
         elsif L /= P1_Level and then (Word and Large_Page_Bit) /= 0 then
            -- Never interpret a huge/large data page as a next-level table.
            return 0;
         end if;
         Frame := Word and Frame_Mask;
         if not RAM_Page (Frame, Physical_Last) then return 0; end if;
         if L = P1_Level then return Frame; end if;
         Table_Frame := Frame;
      end loop;
      return 0;
   end Walk;

   function Readable_Frame (Root, Address, Physical_Last : Unsigned_64)
     return Unsigned_64
   is
      function Readable is new Walk (Read_Entry, Present_Bit or User_Bit);
   begin
      return Readable (Root, Address, Physical_Last);
   end Readable_Frame;

   function Writable_Frame (Root, Address, Physical_Last : Unsigned_64)
     return Unsigned_64
   is
      function Writable is new Walk
        (Read_Entry, Present_Bit or User_Bit or Writable_Bit);
   begin
      return Writable (Root, Address, Physical_Last);
   end Writable_Frame;
end User_Page_Walk;
