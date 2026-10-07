package body Intel_GPU_Extent_Allocator is
   use Intel_GPU_Buffer_Reply.Layout;
   package Directory renames Intel_GPU_Extent_Directory;
   function Last_Allocation_Reason (Object : Pool) return Allocation_Reason is
     (Object.Allocation_Check);
   procedure Reset_Allocation_Diagnostic (Object : in out Pool) is
   begin
      Object.Allocation_Check := Not_Attempted;
   end Reset_Allocation_Diagnostic;
   procedure Quarantine (Object : in out Pool) is
   begin
      Object.Broken := True;
      Directory.Quarantine (Object.Directory);
      -- Retain all physical and metadata storage. Invalidating lookup is not
      -- evidence of GPU/TLB/CPU retirement and must never trigger reclamation.
   end Quarantine;
   procedure Configure_Heap
     (Object : in out Pool; Byte_Quota, DMA_Limit : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Configured or else Object.Attempted or else Object.Broken or else
        not Owner_Ready or else not Intel_GPU_Buffer_Reply.Layout.Heap_Geometry_Valid
          (Byte_Quota, DMA_Limit, Intel_GPU_Buffer_Reply.Layout.CPU_Base)
      then return; end if;
      Object.Limit := Byte_Quota;
      Object.DMA_Limit := DMA_Limit;
      Object.Configured := True;
      Accepted := True;
   end Configure_Heap;
   function DMA_Ceiling (Object : Pool) return Unsigned_64 is
     (Object.DMA_Limit);
   function Extent_Capacity (Object : Pool) return Positive is
     (Directory.Metadata_Capacity (Object.Directory));
   function Required_Extent_Metadata (Object : Pool) return Natural is
     (if Object.Broken or else not Owner_Ready or else
         Object.Metadata_Required <= Extent_Capacity (Object)
      then 0 else Object.Metadata_Required);
   procedure Extend_Extents
     (Object : in out Pool; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Broken or else not Object.Attempted or else not Owner_Ready then return; end if;
      Directory.Extend_Metadata (Object.Directory, Base, Bytes, Accepted);
   end Extend_Extents;
   function Record_Capacity (Object : Pool) return Positive is
     (Records.Capacity (Object.Items));
   function Snapshot (Object : Pool) return Directory.Borrowed_View is
      Empty, Retained : Directory.Borrowed_View;
   begin
      if Object.Broken or else not Owner_Ready then return Empty; end if;
      Retained := Directory.Borrow (Object.Directory);
      return Retained;
   end Snapshot;
   procedure Extend_Records
     (Object : in out Pool; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
      Before : constant Positive := Records.Capacity (Object.Items);
   begin
      Accepted := False;
      if Object.Broken or else not Owner_Ready then return; end if;
      Records.Extend (Object.Items, Base, Bytes, Accepted);
      if Accepted then
         Object.Unassigned := Object.Unassigned +
           (Records.Capacity (Object.Items) - Before);
      end if;
   end Extend_Records;
   -- Ordered live slices; empty generation tombstones are not linked. Walking
   -- gaps needs no capacity-sized stack snapshot. Serialized supervisor only.
   procedure Find_Gap
     (Object : Pool; Bytes : Unsigned_64; Offset : out Unsigned_64;
      Previous, Following : out Natural; Found : out Boolean) is
      Cursor : Natural := Object.First_Extent;
      Position : Unsigned_64 := 0;
   begin
      Offset := 0; Previous := 0; Following := 0; Found := False;
      for Visit in 1 .. Records.Capacity (Object.Items) loop
         exit when Cursor = 0;
         if Cursor > Records.Capacity (Object.Items) then return; end if;
         declare Item : constant Entry_Record := Records.Get (Object.Items, Cursor); begin
            if Item.Bytes = 0 or else Item.Offset < Position or else
              Item.Offset > Object.Limit or else Item.Bytes > Object.Limit - Item.Offset
            then return; end if;
            if Bytes <= Item.Offset - Position then
               Offset := Position; Following := Cursor; Found := True; return;
            end if;
            Position := Item.Offset + Item.Bytes;
            Previous := Cursor; Cursor := Item.Next;
         end;
      end loop;
      if Cursor = 0 and then Bytes <= Object.Limit - Position then
         Offset := Position; Found := True;
      end if;
   end Find_Gap;
   function Memory_Budget (Object : Pool) return Budget is
      Result : Budget;
   begin
      if Object.Broken or else not Owner_Ready or else
        Directory.Committed_Bytes (Object.Directory) = 0 or else Object.Used > Object.Limit
      then return Result; end if;
      Result.Known := True;
      Result.Capacity := Object.Limit;
      Result.Committed := Directory.Committed_Bytes (Object.Directory);
      Result.Retained := Object.Used;
      Result.Available := Object.Limit - Object.Used;
      Result.Unassigned_Slots := Object.Unassigned;
      return Result;
   end Memory_Budget;

   procedure Acquire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success : out Boolean) is
      package Views renames Intel_GPU_Buffer_Reply;
      Empty : Views.Extent_View;
      Backing : Directory.Borrowed_View;
      Requested : constant Unsigned_64 := Unsigned_64 (Pages) * 4096;
      Previous, Following : Natural;
      Offset : Unsigned_64;
      Found : Boolean;
   begin
      Buffer := Empty;
      Success := False;
      Object.Allocation_Check := Request_Check;
      if Index > Record_Capacity (Object) or else Arena_ID = 0 or else Generation = 0 or else
        (Object.Identity /= 0 and then Object.Identity /= Arena_ID)
      then return; end if;
      Object.Allocation_Check := Generation_Check;
      if Records.Get (Object.Items, Index).Bytes = 0 then
         if Records.Get (Object.Items, Index).Generation = Unsigned_32'Last or else
           Generation /= Records.Get (Object.Items, Index).Generation + 1 then return; end if;
      elsif Records.Get (Object.Items, Index).Generation /= Generation or else
        Records.Get (Object.Items, Index).Bytes /= Requested then return; end if;
      if Records.Get (Object.Items, Index).Bytes = 0 then
         Object.Allocation_Check := Quota_Check;
         if Requested > Object.Limit - Object.Used then return; end if;
         Object.Allocation_Check := Gap_Check;
         Find_Gap (Object, Requested, Offset, Previous, Following, Found);
         if not Found then return; end if;
      else
         Offset := Records.Get (Object.Items, Index).Offset;
         Previous := 0; Following := 0;
      end if;
      Acquire (Object, Views.Layout.CPU_Base, Backing, Success, Offset + Requested);
      if not Success then return; end if;
      Success := False;
      if Records.Get (Object.Items, Index).Bytes = 0 then
         Records.Put (Object.Items, Index,
           (Offset, Requested, Generation, Previous, Following));
         if Previous = 0 then Object.First_Extent := Index;
         else
            Records.Put (Object.Items, Previous,
              (Records.Get (Object.Items, Previous) with delta Next => Index));
         end if;
         if Following /= 0 then
            Records.Put (Object.Items, Following,
              (Records.Get (Object.Items, Following) with delta Previous => Index));
         end if;
         Object.Used := Object.Used + Requested;
         Object.Unassigned := Object.Unassigned - 1;
      elsif Records.Get (Object.Items, Index).Bytes /= Requested or else
        Records.Get (Object.Items, Index).Generation /= Generation then
         return;
      end if;
      Object.Identity := Arena_ID;
      Object.Allocation_Check := View_Check;
      Buffer := Views.From_Extents
        (Directory.Borrow (Object.Directory), Arena_ID, Records.Get (Object.Items, Index).Offset, Requested);
      Success := Views.Valid (Buffer);
      if Success then Object.Allocation_Check := Ready; end if;
   end Acquire_Buffer;

   procedure Step_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count; Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success, Pending : out Boolean) is
      Empty : Intel_GPU_Buffer_Reply.Extent_View;
      Backing : Directory.Borrowed_View;
      Bytes : constant Unsigned_64 := Unsigned_64 (Pages) * 4096;
      Offset, Target, Committed : Unsigned_64;
      Previous, Following : Natural;
      Found : Boolean;
   begin
      Buffer := Empty; Success := False; Pending := False;
      Object.Metadata_Required := 0;
      Object.Allocation_Check := Request_Check;
      if Object.Broken or else Index > Record_Capacity (Object) or else
        Arena_ID = 0 or else Generation = 0 or else
        (Object.Identity /= 0 and then Object.Identity /= Arena_ID) then return; end if;
      if Records.Get (Object.Items, Index).Bytes /= 0 then
         Acquire_Buffer (Object, Arena_ID, Index, Pages, Generation, Buffer, Success);
         return;
      end if;
      Object.Allocation_Check := Generation_Check;
      if Records.Get (Object.Items, Index).Generation = Unsigned_32'Last or else
        Generation /= Records.Get (Object.Items, Index).Generation + 1 then return; end if;
      Object.Allocation_Check := Quota_Check;
      if Bytes > Object.Limit - Object.Used then return; end if;
      Object.Allocation_Check := Gap_Check;
      Find_Gap (Object, Bytes, Offset, Previous, Following, Found);
      if not Found then return; end if;
      Target := Offset + Bytes;
      Committed := Directory.Committed_Bytes (Object.Directory);
      if Target > Committed then
         if Committed / E.Block_Bytes >= Unsigned_64 (Extent_Capacity (Object)) then
            Object.Allocation_Check := Metadata_Check;
            Object.Metadata_Required := Natural (Committed / E.Block_Bytes) + 1;
            Pending := True;
            return;
         end if;
         Acquire (Object, Intel_GPU_Buffer_Reply.Layout.CPU_Base, Backing, Success,
           Unsigned_64'Min (Target, Committed + E.Block_Bytes));
         if not Success then return; end if;
         if Directory.Byte_Count (Backing) < Target then
            Success := False; Pending := True; return;
         end if;
      end if;
      Acquire_Buffer (Object, Arena_ID, Index, Pages, Generation, Buffer, Success);
   end Step_Buffer;

   procedure Retire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Positive; Generation : Unsigned_32;
      All_References_Retired : Boolean; Success : out Boolean) is
      Item, Before, After : Entry_Record;
      Previous, Following : Natural;
   begin
      Success := False;
      if Object.Broken or else Index > Record_Capacity (Object) then return; end if;
      if not Owner_Ready then
         if Object.Attempted then Quarantine (Object); end if;
         return;
      end if;
      if not All_References_Retired or else Arena_ID = 0 or else
        Arena_ID /= Object.Identity or else Generation = 0 or else
        Generation = Unsigned_32'Last or else
        Records.Get (Object.Items, Index).Generation /= Generation or else
        Records.Get (Object.Items, Index).Bytes = 0 then return; end if;
      Item := Records.Get (Object.Items, Index);
      Previous := Item.Previous; Following := Item.Next;
      -- Validate both reciprocal links before changing any list member.
      -- This is bounded work, independent of the number of live BOs.
      if Previous > Record_Capacity (Object) or else
        Following > Record_Capacity (Object) or else
        Previous = Index or else Following = Index or else
        Item.Offset > Object.Limit or else Item.Bytes > Object.Limit - Item.Offset
      then Quarantine (Object); return; end if;
      if Previous = 0 then
         if Object.First_Extent /= Index then Quarantine (Object); return; end if;
      else
         Before := Records.Get (Object.Items, Previous);
         if Before.Bytes = 0 or else Before.Next /= Index or else
           Before.Offset > Item.Offset or else Before.Bytes > Item.Offset - Before.Offset
         then Quarantine (Object); return; end if;
      end if;
      if Following /= 0 then
         After := Records.Get (Object.Items, Following);
         if After.Bytes = 0 or else After.Previous /= Index or else
           After.Offset < Item.Offset + Item.Bytes
         then Quarantine (Object); return; end if;
      end if;
      if Previous = 0 then Object.First_Extent := Following;
      else
         Records.Put (Object.Items, Previous,
           (Before with delta Next => Following));
      end if;
      if Following /= 0 then
         Records.Put (Object.Items, Following,
           (After with delta Previous => Previous));
      end if;
      Object.Used := Object.Used - Records.Get (Object.Items, Index).Bytes;
      Object.Unassigned := Object.Unassigned + 1;
      Records.Put (Object.Items, Index,
        (Item with delta Bytes => 0, Offset => 0, Previous => 0, Next => 0));
      -- Preserve the generation tombstone. No underlying DMA page is freed.
      Success := True;
   end Retire_Buffer;

   procedure Acquire
     (Object : in out Pool; CPU_Base : Unsigned_64;
      Backing : out Directory.Borrowed_View; Success : out Boolean;
      Required_Bytes : Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.Default_Heap.Byte_Quota) is
      Value : Unsigned_64;
      Empty : Directory.Borrowed_View;
      Required_Blocks : Natural;
      Committed_Blocks : Natural;
   begin
      Backing := Empty;
      Success := False;
      Object.Allocation_Check := Backing_Check;
      if Object.Broken then return; end if;
      Object.Allocation_Check := Owner_Check;
      if not Owner_Ready then
         if Object.Attempted then Quarantine (Object); end if;
         return;
      end if;
      -- Geometry only: address-space reservation/authority is the adapter's
      -- responsibility. Restrict to low canonical, aligned user addresses.
      Object.Allocation_Check := Geometry_Check;
      if Required_Bytes = 0 or else Required_Bytes > Object.Limit or else
        CPU_Base = 0 or else CPU_Base mod E.Block_Bytes /= 0 or else
        CPU_Base > 2 ** 47 - Object.Limit
      then return; end if;
      if Object.Attempted then
         if CPU_Base /= Object.CPU then return; end if;
      else
         Object.Attempted := True;
         Object.CPU := CPU_Base;
         Object.Allocation_Check := Directory_Check;
         Directory.Initialize (Object.Directory, Object.Limit, Object.DMA_Limit, Success);
         if not Success then Quarantine (Object); return; end if;
         Success := False;
      end if;
      Required_Blocks := Natural ((Required_Bytes - 1) / E.Block_Bytes + 1);
      -- Metadata pressure is recoverable before any physical callback. The
      -- serialized caller may grow its independently budgeted directory and
      -- resume this request; no ambiguous physical allocation is replayed.
      Object.Allocation_Check := Metadata_Check;
      if Required_Blocks > Extent_Capacity (Object) then return; end if;
      Committed_Blocks := Natural (Directory.Committed_Bytes (Object.Directory) / E.Block_Bytes);
      if Required_Blocks > Committed_Blocks then
         for I in Committed_Blocks .. Required_Blocks - 1 loop
            Object.Allocation_Check := Owner_Check;
            if not Owner_Ready then Quarantine (Object); return; end if;
            Object.Allocation_Check := Physical_Call;
            Value := Allocate (CPU_Base + Unsigned_64 (I) * E.Block_Bytes);
            Object.Allocation_Check := Owner_Check;
            if not Owner_Ready then
               Quarantine (Object);
               return;
            end if;
            Object.Allocation_Check := Physical_Result_Check;
            Directory.Append (Object.Directory, Value, Success);
            if not Success then
               Quarantine (Object);
               return;
            end if;
            Success := False;
         end loop;
      end if;
      Object.Allocation_Check := Owner_Check;
      if not Owner_Ready then Quarantine (Object); Success := False; return; end if;
      Object.Allocation_Check := View_Check;
      Backing := Directory.Borrow (Object.Directory);
      Success := Directory.Valid (Backing);
      if Success then Object.Allocation_Check := Ready; end if;
   end Acquire;
end Intel_GPU_Extent_Allocator;
