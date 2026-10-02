with Interfaces; use Interfaces;
with Intel_GPU_Device_Query; use Intel_GPU_Device_Query;
procedure Device_Query_Tests is
   Data : Snapshot := (16#46D2#, 17, True, 1, 16#FFFF#);
   Request : Words := [Version, Identity, 0, 0];
   Result : Words;
begin
   pragma Assert (Respond
     (Data, Label, 4, 0, 0, [Version, Virtual_Memory, 0, 0]) =
       [Unavailable, Version, 0, 0]);
   pragma Assert (Respond
     (Data, Label, 4, 0, 0, [Version, Virtual_Memory, 0, 0],
      VM_Policy => Private_PPGTT_48) = [OK, Version, 48, 1]);
   for Index in Request'Range loop
      for Bit in 0 .. 63 loop
         Request := [Version, Virtual_Memory, 0, 0];
         Request (Index) := Request (Index) xor Shift_Left (Unsigned_64'(1), Bit);
         Result := Respond (Data, Label, 4, 0, 0, Request,
                            VM_Policy => Private_PPGTT_48);
         -- Flipping selector bit2 selects identity; no malformed request
         -- may retain a successful VM-contract payload.
         pragma Assert (Result /= [OK, Version, 48, 1]);
      end loop;
   end loop;
   Request := [Version, Identity, 0, 0];
   for Policy in Memory_Contract loop
      Result := Respond (Data, Label, 4, 0, 0, [Version, Memory, 0, 0],
                         Memory_Policy => Policy);
      pragma Assert (Result =
        (if Policy = Not_Admitted then [Unavailable, Version, 0, 0]
         else [OK, Version, Memory_Contract'Pos (Policy), 0]));
   end loop;
   pragma Assert (Respond (Data, Label, 4, 0, 0, [Version, Memory, 0, 0]) =
     [Unavailable, Version, 0, 0]);
   pragma Assert (Respond (Data, Label, 4, 0, 0, [Version, Timestamp, 0, 0]) =
     [Unavailable, Version, 0, 0]);
   pragma Assert (Respond (Data, Label, 4, 0, 0, [Version, Timestamp, 0, 0], 12_000_000) =
     [OK, Version, 12_000_000, 0]);
   pragma Assert (Respond (Data, Label, 4, 0, 0, [Version, Timestamp, 0, 0], Unsigned_32'Last) =
     [Unavailable, Version, 0, 0]);
   pragma Assert (Respond (Data, Label, 4, 0, 0, Request) =
     [OK, Version, 16#11_46D2_8086#, 0]);
   Request (1) := Topology;
   for Mask in Unsigned_16 loop
      Data.EU_Mask := Mask;
      Result := Respond (Data, Label, 4, 0, 0, Request);
      pragma Assert ((Result (0) = OK) =
        (Mask /= 0 and (Mask and 16#5555#) =
           (Shift_Right (Mask, 1) and 16#5555#)));
      if Result (0) = OK then
         pragma Assert (Result = [OK, Version, 1, Unsigned_64 (Mask)]);
      else
         pragma Assert (Result = [Unavailable, Version, 0, 0]);
      end if;
   end loop;
   Data.EU_Mask := 16#FFFF#;
   for Mask in Unsigned_8 loop
      Data.DSS_Mask := Mask;
      pragma Assert ((Respond (Data, Label, 4, 0, 0, Request) (0) = OK) =
        (Mask in 1 .. 63));
   end loop;
   Data.DSS_Mask := 1;
   Data.Topology_Observed := False;
   pragma Assert (Respond (Data, Label, 4, 0, 0, Request) =
     [Unavailable, Version, 0, 0]);
   Data.Topology_Observed := True;
   for Device in Unsigned_16 loop
      Data.Device := Device;
      pragma Assert ((Respond (Data, Label, 4, 0, 0, Request) (0) = OK) =
        (Device = 16#46D2#));
   end loop;
   Data.Device := 16#46D2#;
   for Byte in Unsigned_8 loop
      pragma Assert ((Respond (Data, Label, Byte, 0, 0, Request) (0) = OK) =
        (Byte = 4));
      pragma Assert ((Respond (Data, Label, 4, Byte, 0, Request) (0) = OK) =
        (Byte = 0));
   end loop;
   for Value in Unsigned_16 loop
      pragma Assert ((Respond (Data, Label, 4, 0, Value, Request) (0) = OK) =
        (Value = 0));
   end loop;
   for Index in Request'Range loop
      for Bit in 0 .. 63 loop
         Request := [Version, Topology, 0, 0];
         Request (Index) := Request (Index) xor Shift_Left (Unsigned_64'(1), Bit);
         Result := Respond (Data, Label, 4, 0, 0, Request);
         pragma Assert ((Result (0) = OK) = (Index = 1 and Bit = 0));
         if Result (0) /= OK then
            pragma Assert (Result =
              (if Index = 1 and Bit = 1 then [Unavailable, Version, 0, 0]
               else [Bad_Request, Version, 0, 0]));
         end if;
      end loop;
   end loop;
   pragma Assert (Respond (Data, 0, 4, 0, 0, Request) =
     [Unsupported, Version, 0, 0]);
end Device_Query_Tests;
