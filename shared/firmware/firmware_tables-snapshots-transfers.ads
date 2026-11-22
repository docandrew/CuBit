pragma Ada_2022;
-- Bounded copies of a selected immutable SDT. No address, mapping, allocation
-- or capability API. The caller supplies trusted disjoint destination storage.
package Firmware_Tables.Snapshots.Transfers with SPARK_Mode, Pure is
   type Transfer_Status is
     (Source_Not_Ready, No_Table, Offset_Out_Of_Range, Buffer_Empty, Transferred);
   type Transfer_Result is record
      Status : Transfer_Status := Source_Not_Ready;
      Copied : Natural := 0;
   end record;
   -- Offset is relative to one admitted table, never physical memory. EOF is
   -- successful with zero bytes; an empty buffer before EOF is a refusal.
   -- Non-copied bytes, including every failure result, are zeroed.
   procedure Copy_Part
     (S : State; Index, Offset : Natural; Destination : out Bytes;
      Result : out Transfer_Result)
     with Global => null,
       Post => Result.Copied <= Destination'Length
         and then (if Result.Status = Transferred then
           Current (S) = Ready and then Index in 1 .. Count (S)
           and then Offset <= Item (S, Index).Extent
           and then Result.Copied = Natural'Min
             (Destination'Length, Item (S, Index).Extent - Offset)
           and then (Result.Copied > 0 or else Offset = Item (S, Index).Extent)
           and then (for all I in Destination'Range =>
             Destination (I) =
               (if I - Destination'First < Result.Copied
                then Value (S, Index, Offset + (I - Destination'First)) else 0))
         else Result.Copied = 0
           and then (for all B of Destination => B = 0));
end Firmware_Tables.Snapshots.Transfers;
