package body Intel_GPU_VM_Image.Growth is
   use Intel_GPU_ADLN_PPGTT;
   function Inspect (Object : Image; GPU, Bytes : Unsigned_64) return Requirements is
      Missing : Natural := 0;
      Last : array (1 .. 3) of Unsigned_64 := [others => Unsigned_64'Last];
      Current, Next, First_Missing : Natural;
      Address, Prefix : Unsigned_64;
   begin
      if not Object.Valid or else not Object.Frozen or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 48 - GPU or else
        Bytes / 4096 > Unsigned_64 (Capacity * 512)
      then return (others => <>); end if;
      for I in 0 .. Natural (Bytes / 4096) - 1 loop
         Address := GPU + Unsigned_64 (I) * 4096;
         declare
            W : constant Walk := Locate (Address);
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            Current := 1; First_Missing := 0;
            for Depth in Route'Range loop
               if Raw_Word (Object, Current, Route (Depth)) = 0 then
                  First_Missing := Depth; exit;
               end if;
               Next := 0;
               for P in 2 .. Object.Count loop
                  if Raw_Word (Object, Current, Route (Depth)) = Encode_Directory (Object.DMA (P)) then
                     Next := P; exit;
                  end if;
               end loop;
               if Next = 0 then return (others => <>); end if;
               Current := Next;
            end loop;
            if First_Missing = 0 then
               if Raw_Word (Object, Current, W.PT) /= 0 then
                  return (Status => Occupied, others => <>);
               end if;
            else
               for Depth in First_Missing .. 3 loop
                  Prefix := Shift_Right (Address, 48 - 9 * Depth);
                  if Prefix /= Last (Depth) then
                     Missing := Missing + 1;
                     Last (Depth) := Prefix;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      return (Ready, Missing, Missing <= Metadata_Capacity (Object) - Object.Count);
   end Inspect;
   procedure Describe
     (Object : Image; GPU, Bytes : Unsigned_64; Nodes : out Node_List;
      Count : out Natural; Accepted : out Boolean)
   is
      Required : constant Requirements := Inspect (Object, GPU, Bytes);
      Last_Prefix : array (1 .. 3) of Unsigned_64 := [others => Unsigned_64'Last];
      Last_Node : array (1 .. 3) of Natural := [others => 0];
      Current, Next, First_Missing : Natural;
      Address, Prefix : Unsigned_64;
      Made : Natural := 0;
   begin
      Nodes := [others => (others => <>)]; Count := 0; Accepted := False;
      if Required.Status /= Ready or else Required.Additional_Tables > Nodes'Length
      then return; end if;
      for I in 0 .. Natural (Bytes / 4096) - 1 loop
         Address := GPU + Unsigned_64 (I) * 4096;
         declare
            W : constant Walk := Locate (Address);
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            Current := 1; First_Missing := 0;
            for Depth in Route'Range loop
               if Raw_Word (Object, Current, Route (Depth)) = 0 then
                  First_Missing := Depth; exit;
               end if;
               Next := 0;
               for P in 2 .. Object.Count loop
                  if Raw_Word (Object, Current, Route (Depth)) = Encode_Directory (Object.DMA (P)) then
                     Next := P; exit;
                  end if;
               end loop;
               if Next = 0 then return; end if; -- excluded by Inspect/stability
               Current := Next;
            end loop;
            if First_Missing /= 0 then
               for Depth in First_Missing .. 3 loop
                  Prefix := Shift_Right (Address, 48 - 9 * Depth);
                  if Prefix /= Last_Prefix (Depth) then
                     Made := Made + 1;
                     Nodes (Nodes'First + Made - 1) :=
                       (Existing_Parent => (if Depth = First_Missing then Current else 0),
                        New_Parent => (if Depth = First_Missing then 0 else Last_Node (Depth - 1)),
                        Index => Route (Depth), Level => 3 - Depth);
                     Last_Prefix (Depth) := Prefix; Last_Node (Depth) := Made;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      Count := Made;
      Accepted := Made = Required.Additional_Tables;
   end Describe;
end Intel_GPU_VM_Image.Growth;
