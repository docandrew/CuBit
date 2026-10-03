package body Intel_GPU_Extent_Directory is
   use type System.Address;
   Block_Bytes : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
   function Ready (Object : Directory) return Boolean is
     (Object.Started and not Object.Failed);
   function Committed_Bytes (Object : Directory) return Unsigned_64 is
     (if Ready (Object) then Unsigned_64 (Object.Count) * Block_Bytes else 0);
   function Metadata_Capacity (Object : Directory) return Positive is
     (Entries.Capacity (Object.Items));
   function Last_Admission_Probes (Object : Directory) return Natural is (Object.Probes);
   procedure Initialize
     (Object : in out Directory; Byte_Quota, DMA_Limit : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Started or Object.Failed then return; end if;
      if Byte_Quota = 0 or else Byte_Quota mod Block_Bytes /= 0 or else
        Byte_Quota / Block_Bytes > Unsigned_64 (Natural'Last) or else
        DMA_Limit < 2 * Block_Bytes or else DMA_Limit mod Block_Bytes /= 0
      then return; end if;
      Object.Quota := Byte_Quota;
      Object.Limit := DMA_Limit;
      Object.Started := True;
      Accepted := True;
   end Initialize;
   procedure Extend_Metadata
     (Object : in out Directory; Base, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Ready (Object) then return; end if;
      Entries.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend_Metadata;
   procedure Append
     (Object : in out Directory; DMA : Unsigned_64; Accepted : out Boolean) is
      Cursor : Unsigned_32 := Object.Root;
      Parent : Unsigned_32 := 0;
      Right : Boolean := False;
      Item : Extent_Entry;
   begin
      Accepted := False;
      Object.Probes := 0;
      if not Ready (Object) or else Object.Count = Entries.Capacity (Object.Items)
        or else Unsigned_64 (Object.Count) >= Object.Quota / Block_Bytes or else
        DMA = 0 or else DMA mod Block_Bytes /= 0 or else
        DMA > Object.Limit - Block_Bytes
      then return; end if;
      -- At depth D, all descendants match the first D significant key bits.
      -- Every node also stores one complete key. Aligned keys need no branch
      -- below bit21: after43 decisions any existing key must be equal.
      for Depth in 0 .. Max_Admission_Probes - 1 loop
         if Cursor = 0 then
            Entries.Put (Object.Items, Object.Count + 1, (DMA, 0, 0));
            if Parent = 0 then
               Object.Root := Unsigned_32 (Object.Count + 1);
            else
               Item := Entries.Get (Object.Items, Positive (Parent));
               if Right then Item.Right := Unsigned_32 (Object.Count + 1);
               else Item.Left := Unsigned_32 (Object.Count + 1); end if;
               Entries.Put (Object.Items, Positive (Parent), Item);
            end if;
            Object.Count := Object.Count + 1;
            Accepted := True;
            return;
         end if;
         if Cursor > Unsigned_32 (Object.Count) then return; end if;
         Object.Probes := Object.Probes + 1;
         Item := Entries.Get (Object.Items, Positive (Cursor));
         if Item.DMA = DMA then return; end if;
         if Depth = Max_Admission_Probes - 1 then return; end if;
         Parent := Cursor;
         Right := (DMA and Shift_Left (Unsigned_64'(1), 63 - Depth)) /= 0;
         Cursor := (if Right then Item.Right else Item.Left);
      end loop;
   end Append;
   function Snapshot (Object : Directory) return View is
     (if Ready (Object) and Object.Count /= 0 then
        (Root => Object'Address, Count => Object.Count) else (others => <>));
   function Valid (Object : Directory; Snapshot : View) return Boolean is
     (Ready (Object) and then Snapshot.Root = Object'Address and then
      Snapshot.Count /= 0 and then Snapshot.Count <= Object.Count);
   function Byte_Count (Object : Directory; Snapshot : View) return Unsigned_64 is
     (if Valid (Object, Snapshot) then Unsigned_64 (Snapshot.Count) * Block_Bytes else 0);
   function Resolve
     (Object : Directory; Snapshot : View; Offset, Bytes : Unsigned_64)
      return Intel_GPU_Physical_Extents.Span is
      Limit : constant Unsigned_64 := Byte_Count (Object, Snapshot);
   begin
      if Limit = 0 or else Bytes = 0 or else Offset >= Limit or else Bytes > Limit - Offset
      then return (others => <>); end if;
      return (Valid => True,
        Address => Entries.Get (Object.Items, Positive (Offset / Block_Bytes + 1)).DMA + Offset mod Block_Bytes,
        Bytes => Unsigned_64'Min (Bytes, Block_Bytes - Offset mod Block_Bytes));
   end Resolve;
   procedure Quarantine (Object : in out Directory) is
   begin Object.Failed := True; end Quarantine;
   function Borrow (Object : aliased Directory) return Borrowed_View is
   begin
      if not Ready (Object) or else Object.Count = 0 then return (others => <>); end if;
      -- Service owns storage beyond every BO lifetime. Ada's local formal
      -- accessibility cannot express that external retention obligation.
      return (Owner => Object'Unchecked_Access, Count => Object.Count);
   end Borrow;
   function Valid (Object : Borrowed_View) return Boolean is
     (Object.Owner /= null and then Ready (Object.Owner.all) and then
      Object.Count /= 0 and then Object.Count <= Object.Owner.Count);
   function Byte_Count (Object : Borrowed_View) return Unsigned_64 is
     (if Valid (Object) then Unsigned_64 (Object.Count) * Block_Bytes else 0);
   function Same_Owner (Left, Right : Borrowed_View) return Boolean is
     (Valid (Left) and then Valid (Right) and then Left.Owner = Right.Owner);
   function Resolve
     (Object : Borrowed_View; Offset, Bytes : Unsigned_64)
      return Intel_GPU_Physical_Extents.Span is
   begin
      if not Valid (Object) then return (others => <>); end if;
      return Resolve (Object.Owner.all,
        (Root => Object.Owner.all'Address, Count => Object.Count), Offset, Bytes);
   end Resolve;
end Intel_GPU_Extent_Directory;
