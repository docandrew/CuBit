package body Intel_GPU_VM_Image.Growth is
   use Intel_GPU_ADLN_PPGTT;
   function Inspect_State
     (Object : Image; GPU, Bytes : Unsigned_64; Frozen : Boolean) return Requirements is
      Missing : Natural := 0;
      Last : array (1 .. 3) of Unsigned_64 := [others => Unsigned_64'Last];
      Current, Next, First_Missing : Natural;
      Address, Prefix : Unsigned_64;
      Remaining : Natural;
   begin
      if not Object.Valid or else Object.Frozen /= Frozen or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 48 - GPU or else
        Bytes / 4096 > Unsigned_64 (Capacity * 512)
      then return (others => <>); end if;
      Address := GPU; Remaining := Natural (Bytes / 4096);
      while Remaining /= 0 loop
         declare
            W : constant Walk := Locate (Address);
            Span : constant Positive := Natural'Min (Remaining, 512 - Natural (W.PT));
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            Current := 1; First_Missing := 0;
            for Depth in Route'Range loop
               if Raw_Word (Object, Current, Route (Depth)) = 0 then
                  First_Missing := Depth; exit;
               end if;
               Next := 0;
               for P in 2 .. Object.Count loop
                  if Raw_Word (Object, Current, Route (Depth)) = Encode_Directory (Descriptor (Object, P).DMA) then
                     Next := P; exit;
                  end if;
               end loop;
               if Next = 0 then return (others => <>); end if;
               Current := Next;
            end loop;
            if First_Missing = 0 then
               -- One directory walk per leaf-table span, but no skipped leaf
               -- occupancy checks: a conflict at the final word still rejects.
               for I in Natural (W.PT) .. Natural (W.PT) + Span - 1 loop
                  if Raw_Word (Object, Current, Table_Index (I)) /= 0 then
                     return (Status => Occupied, others => <>);
                  end if;
               end loop;
            else
               for Depth in First_Missing .. 3 loop
                  Prefix := Shift_Right (Address, 48 - 9 * Depth);
                  if Prefix /= Last (Depth) then
                     Missing := Missing + 1;
                     Last (Depth) := Prefix;
                  end if;
               end loop;
            end if;
            Address := Address + Unsigned_64 (Span) * 4096;
            Remaining := Remaining - Span;
         end;
      end loop;
      return (Status => Ready, Additional_Tables => Missing,
              Fits_Reserved => Missing <= Metadata_Capacity (Object) - Object.Count,
              Required_Tables => Object.Count + Missing,
              Fits_Quota => Missing <= Capacity - Object.Count);
   end Inspect_State;
   function Inspect (Object : Image; GPU, Bytes : Unsigned_64) return Requirements is
     (Inspect_State (Object, GPU, Bytes, True));
   function Inspect_Offline (Object : Image; GPU, Bytes : Unsigned_64)
     return Offline_Requirements
   is
      Needed : constant Requirements := Inspect_State (Object, GPU, Bytes, False);
      Backed : constant Natural := Object.Backed;
      Available : Natural;
   begin
      if Needed.Status /= Ready then return (Needed, 0); end if;
      if Backed < Object.Count then return (others => <>); end if;
      Available := Backed - Object.Count;
      return (Needed, (if Needed.Additional_Tables > Available then
        Needed.Additional_Tables - Available else 0));
   end Inspect_Offline;
   procedure Describe_Into
     (Object : Image; GPU, Bytes : Unsigned_64; Output_Capacity : Natural;
      Count : out Natural; Accepted : out Boolean)
   is
      Required : constant Requirements := Inspect (Object, GPU, Bytes);
      Last_Prefix : array (1 .. 3) of Unsigned_64 := [others => Unsigned_64'Last];
      Last_Node : array (1 .. 3) of Natural := [others => 0];
      Current, Next, First_Missing : Natural;
      Address, Prefix : Unsigned_64;
      Made : Natural := 0;
      Remaining : Natural;
      OK : Boolean;
      Epoch : constant Unsigned_64 := Revision (Object);
      Root : constant Unsigned_64 := Root_DMA (Object);
      function Owner_Current return Boolean is
        (Authorized and then Sealed (Object) and then Revision (Object) = Epoch
         and then Root_DMA (Object) = Root);
   begin
      Count := 0; Accepted := False;
      if not Owner_Current or else Required.Status /= Ready or else
        Required.Additional_Tables > Output_Capacity
      then return; end if;
      Address := GPU; Remaining := Natural (Bytes / 4096);
      while Remaining /= 0 loop
         declare
            W : constant Walk := Locate (Address);
            Span : constant Positive := Natural'Min (Remaining, 512 - Natural (W.PT));
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            Current := 1; First_Missing := 0;
            for Depth in Route'Range loop
               if Raw_Word (Object, Current, Route (Depth)) = 0 then
                  First_Missing := Depth; exit;
               end if;
               Next := 0;
               for P in 2 .. Object.Count loop
                  if Raw_Word (Object, Current, Route (Depth)) = Encode_Directory (Descriptor (Object, P).DMA) then
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
                     Emit (Made,
                       (Existing_Parent => (if Depth = First_Missing then Current else 0),
                        New_Parent => (if Depth = First_Missing then 0 else Last_Node (Depth - 1)),
                        Index => Route (Depth), Level => 3 - Depth), OK);
                     if not OK or else not Owner_Current then return; end if;
                     Last_Prefix (Depth) := Prefix; Last_Node (Depth) := Made;
                  end if;
               end loop;
            end if;
            Address := Address + Unsigned_64 (Span) * 4096;
            Remaining := Remaining - Span;
         end;
      end loop;
      Accepted := Owner_Current and then Made = Required.Additional_Tables;
      if Accepted then Count := Made; end if;
   end Describe_Into;
   procedure Describe
     (Object : Image; GPU, Bytes : Unsigned_64; Nodes : out Node_List;
      Count : out Natural; Accepted : out Boolean) is
      function Authorized return Boolean is (True);
      procedure Emit (Ordinal : Positive; Item : Node; OK : out Boolean) is
      begin
         Nodes (Nodes'First + (Ordinal - 1)) := Item;
         OK := True;
      end Emit;
      procedure Stream is new Describe_Into (Authorized, Emit);
   begin
      Nodes := [others => (others => <>)];
      Stream (Object, GPU, Bytes, Nodes'Length, Count, Accepted);
      if not Accepted then Nodes := [others => (others => <>)]; end if;
   end Describe;
end Intel_GPU_VM_Image.Growth;
