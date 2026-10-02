with Intel_GPU_VA_Placement;
package body Intel_GPU_Extent_Allocator is
   function Memory_Budget (Object : Pool) return Budget is
      Result : Budget;
   begin
      if Object.Broken or else not Owner_Ready or else
        not E.Ready (Object.Mapping) or else Object.Used > E.Capacity
      then return Result; end if;
      Result.Known := True;
      Result.Capacity := E.Capacity;
      Result.Retained := Object.Used;
      Result.Available := E.Capacity - Object.Used;
      for Item of Object.Items loop
         if Item.Bytes = 0 and then Item.Generation /= Unsigned_32'Last then
            Result.Unassigned_Slots := Result.Unassigned_Slots + 1;
         end if;
      end loop;
      return Result;
   end Memory_Budget;

   procedure Acquire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Intel_GPU_Buffer_Reply.Layout.Slot;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success : out Boolean) is
      package Views renames Intel_GPU_Buffer_Reply;
      Empty : Views.Extent_View;
      Backing : E.Map;
      Requested : constant Unsigned_64 := Unsigned_64 (Pages) * 4096;
      Used : Intel_GPU_VA_Placement.Extents (1 .. Object.Items'Length);
      Count : Natural := 0;
      Offset : Unsigned_64;
      Found : Boolean;
   begin
      Buffer := Empty;
      Success := False;
      if Arena_ID = 0 or else Generation = 0 or else
        (Object.Identity /= 0 and then Object.Identity /= Arena_ID)
      then return; end if;
      if Object.Items (Index).Bytes = 0 then
         if Object.Items (Index).Generation = Unsigned_32'Last or else
           Generation /= Object.Items (Index).Generation + 1 then return; end if;
      elsif Object.Items (Index).Generation /= Generation or else
        Object.Items (Index).Bytes /= Requested then return; end if;
      Acquire (Object, Views.Layout.CPU_Base, Backing, Success);
      if not Success then return; end if;
      Success := False;
      if Object.Items (Index).Bytes = 0 then
         if Requested > E.Capacity - Object.Used then return; end if;
         for Item of Object.Items loop
            if Item.Bytes /= 0 then
               Count := Count + 1;
               Used (Count) := (Item.Offset, Item.Offset + Item.Bytes);
            end if;
         end loop;
         Intel_GPU_VA_Placement.Find
           ((0, E.Capacity), Used (1 .. Count), Requested, 4096, Offset, Found);
         if not Found then return; end if;
         Object.Items (Index) := (Offset, Requested, Generation);
         Object.Used := Object.Used + Requested;
      elsif Object.Items (Index).Bytes /= Requested or else
        Object.Items (Index).Generation /= Generation then
         return;
      end if;
      Object.Identity := Arena_ID;
      Buffer := Views.From_Extents
        (Backing, Arena_ID, Object.Items (Index).Offset, Requested);
      Success := Views.Valid (Buffer);
   end Acquire_Buffer;

   procedure Retire_Buffer
     (Object : in out Pool; Arena_ID : Unsigned_64;
      Index : Intel_GPU_Buffer_Reply.Layout.Slot; Generation : Unsigned_32;
      All_References_Retired : Boolean; Success : out Boolean) is
   begin
      Success := False;
      if Object.Broken then return; end if;
      if not Owner_Ready then
         if Object.Attempted then Object.Broken := True; end if;
         return;
      end if;
      if not All_References_Retired or else Arena_ID = 0 or else
        Arena_ID /= Object.Identity or else Generation = 0 or else
        Generation = Unsigned_32'Last or else
        Object.Items (Index).Generation /= Generation or else
        Object.Items (Index).Bytes = 0 then return; end if;
      Object.Used := Object.Used - Object.Items (Index).Bytes;
      Object.Items (Index).Bytes := 0;
      Object.Items (Index).Offset := 0;
      -- Preserve the generation tombstone. No underlying DMA page is freed.
      Success := True;
   end Retire_Buffer;

   procedure Acquire
     (Object : in out Pool; CPU_Base : Unsigned_64;
      Backing : out E.Map; Success : out Boolean) is
      Value : Unsigned_64;
      Empty : E.Map;
   begin
      Backing := Empty;
      Success := False;
      if Object.Broken then return; end if;
      if not Owner_Ready then
         if Object.Attempted then Object.Broken := True; end if;
         return;
      end if;
      -- Geometry only: address-space reservation/authority is the adapter's
      -- responsibility. Restrict to low canonical, aligned user addresses.
      if CPU_Base = 0 or else CPU_Base mod E.Block_Bytes /= 0 or else
        CPU_Base > 2 ** 47 - E.Capacity
      then return; end if;
      if Object.Attempted then
         if CPU_Base /= Object.CPU then return; end if;
      else
         Object.Attempted := True;
         Object.CPU := CPU_Base;
         for I in E.Block_Index loop
            if not Owner_Ready then Object.Broken := True; return; end if;
            Value := Allocate (CPU_Base + Unsigned_64 (I) * E.Block_Bytes);
            Object.Bases (I) := Value;
            if not Owner_Ready or else Value = 0 or else
              Value mod E.Block_Bytes /= 0 or else Value > 2 ** 32 - E.Block_Bytes
            then Object.Broken := True; return; end if;
            for J in E.Block_Index loop
               if J < I and then Object.Bases (J) = Value then
                  Object.Broken := True; return;
               end if;
            end loop;
         end loop;
         E.Admit (Object.Bases, Object.Mapping, Success);
         if not Success then Object.Broken := True; return; end if;
      end if;
      if not Owner_Ready then Object.Broken := True; Success := False; return; end if;
      Backing := Object.Mapping;
      Success := E.Ready (Backing);
   end Acquire;
end Intel_GPU_Extent_Allocator;
