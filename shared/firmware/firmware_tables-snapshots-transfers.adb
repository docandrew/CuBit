pragma Ada_2022;
package body Firmware_Tables.Snapshots.Transfers with SPARK_Mode is
   procedure Copy_Part
     (S : State; Index, Offset : Natural; Destination : out Bytes;
      Result : out Transfer_Result) is
      Amount : Natural;
   begin
      Destination := [others => 0];
      Result := (Status => Source_Not_Ready, Copied => 0);
      if Current (S) /= Ready then return; end if;
      Result.Status := No_Table;
      if Index not in 1 .. Count (S) then return; end if;
      Result.Status := Offset_Out_Of_Range;
      if Offset > Item (S, Index).Extent then return; end if;
      Amount := Natural'Min (Destination'Length, Item (S, Index).Extent - Offset);
      Result.Status := Buffer_Empty;
      if Amount = 0 and then Offset /= Item (S, Index).Extent then return; end if;
      if Amount > 0 then
         for J in 0 .. Amount - 1 loop
            Destination (Destination'First + J) := Value (S, Index, Offset + J);
            pragma Loop_Invariant
              (for all K in Destination'Range =>
                Destination (K) =
                  (if K - Destination'First <= J
                   then Value (S, Index, Offset + (K - Destination'First)) else 0));
         end loop;
      end if;
      Result := (Status => Transferred, Copied => Amount);
   end Copy_Part;
end Firmware_Tables.Snapshots.Transfers;
