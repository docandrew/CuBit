with Interfaces; use Interfaces;

package User_Page_Walk with SPARK_Mode, Pure is
   type Level is (P4_Level, P3_Level, P2_Level, P1_Level);
   subtype Table_Index is Natural range 0 .. 511;
   Page_Size : constant Unsigned_64 := 4096;
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   Present_Bit : constant Unsigned_64 := 1;
   Writable_Bit : constant Unsigned_64 := 2;
   User_Bit : constant Unsigned_64 := 4;
   Large_Page_Bit : constant Unsigned_64 := 128;
   Frame_Mask : constant Unsigned_64 := 16#000F_FFFF_FFFF_F000#;

   function RAM_Page (Frame, Physical_Last : Unsigned_64) return Boolean is
     (Frame /= 0 and then Frame mod Page_Size = 0 and then
      Frame <= Physical_Last and then Page_Size - 1 <= Physical_Last - Frame);

   generic
      -- Reads one coherent entry from an existing, lifetime-protected table.
      with procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64);
   function Readable_Frame (Root, Address, Physical_Last : Unsigned_64)
     return Unsigned_64
     with Post => (if Readable_Frame'Result /= 0 then
       Address < User_Limit and then RAM_Page (Readable_Frame'Result, Physical_Last));

   -- As Readable_Frame, but every level must also permit user writes.
   generic
      with procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64);
   function Writable_Frame (Root, Address, Physical_Last : Unsigned_64)
     return Unsigned_64
     with Post => (if Writable_Frame'Result /= 0 then
       Address < User_Limit and then RAM_Page (Writable_Frame'Result, Physical_Last));
end User_Page_Walk;
