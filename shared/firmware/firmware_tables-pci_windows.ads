pragma Ada_2022;
with Firmware_Tables.MCFG;
with Interfaces;
-- MCFG resource policy, separate from the structural table decoder.
-- Results are address metadata only. Kernel classification, ownership and
-- handle derivation must independently admit the selected pages before use.
package Firmware_Tables.PCI_Windows with Pure, SPARK_Mode is
   package M renames Firmware_Tables.MCFG;
   use type M.Bus_Number, M.Segment_Number;
   use type Address_Value, Interfaces.Unsigned_32;
   Page_Bytes : constant := 4096;
   Devices_Per_Bus : constant := 32;
   Functions_Per_Device : constant := 8;
   Function_Bytes : constant := Page_Bytes;
   Bus_Bytes : constant := Devices_Per_Bus * Functions_Per_Device * Function_Bytes;
   Bus_Pages : constant := Bus_Bytes / Page_Bytes;
   type Access_Mode is (Read_Only, Read_Write);
   function Upper_Bytes (Item : M.Allocation) return Address_Value is
     ((Address_Value (Item.Last_Bus) + 1) * Bus_Bytes);
   -- MCFG Base denotes bus zero, even when First_Bus is nonzero.
   -- Check the whole advertised allocation against the exclusive limit.
   function Valid (Item : M.Allocation; Limit : Address_Value) return Boolean is
     (Item.First_Bus <= Item.Last_Bus
      and then Item.Reserved = 0
      and then Item.Base mod Page_Bytes = 0
      and then Limit >= Upper_Bytes (Item)
      and then Item.Base <= Limit - Upper_Bytes (Item));
   type Outcome is (Ready, Invalid_Allocation, Wrong_Segment, Outside_Buses, Write_Refused);
   type Selection (Status : Outcome := Invalid_Allocation) is record
      case Status is
         when Ready =>
            Segment : M.Segment_Number;
            Bus : M.Bus_Number;
            First : Address_Value;
            Pages : Positive;
            Mode : Access_Mode;
         when others => null;
      end case;
   end record;
   function Select_Bus
     (Item : M.Allocation; Segment : M.Segment_Number; Bus : M.Bus_Number;
      Mode : Access_Mode; Limit : Address_Value) return Selection
     with Global => null,
       Post =>
         (if Select_Bus'Result.Status = Ready then
            Valid (Item, Limit) and then Segment = Item.Segment
            and then Bus >= Item.First_Bus and then Bus <= Item.Last_Bus
            and then Mode = Read_Only and then Select_Bus'Result.Mode = Read_Only
            and then Select_Bus'Result.Segment = Segment
            and then Select_Bus'Result.Bus = Bus
            and then Select_Bus'Result.Pages = Bus_Pages
            and then Select_Bus'Result.First = Item.Base + Address_Value (Bus) * Bus_Bytes
            and then Select_Bus'Result.First mod Page_Bytes = 0
            and then Select_Bus'Result.First >= Item.Base + Address_Value (Item.First_Bus) * Bus_Bytes
            and then Select_Bus'Result.First <= Limit - Bus_Bytes
            and then Select_Bus'Result.First + Bus_Bytes <= Item.Base + Upper_Bytes (Item))
         and then (if Mode = Read_Write then Select_Bus'Result.Status = Write_Refused)
         and then (if Mode = Read_Only and then not Valid (Item, Limit)
            then Select_Bus'Result.Status = Invalid_Allocation)
         and then (if Mode = Read_Only and then Valid (Item, Limit) and then Segment /= Item.Segment
            then Select_Bus'Result.Status = Wrong_Segment)
         and then (if Mode = Read_Only and then Valid (Item, Limit) and then Segment = Item.Segment
                   and then (Bus < Item.First_Bus or else Bus > Item.Last_Bus)
            then Select_Bus'Result.Status = Outside_Buses);
end Firmware_Tables.PCI_Windows;
