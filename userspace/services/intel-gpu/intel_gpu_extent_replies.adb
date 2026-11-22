package body Intel_GPU_Extent_Replies with SPARK_Mode is
   package Directory renames Intel_GPU_Extent_Directory;
   procedure Cancel (Object : in out Assembly) is
   begin
      Object.Broken := True;
      Directory.Quarantine (Object.Directory);
   end Cancel;
   function Metadata_Capacity (Object : Assembly) return Positive is
     (Directory.Metadata_Capacity (Object.Directory));
   procedure Extend_Metadata
     (Object : in out Assembly; Base, Bytes : Unsigned_64; Success : out Boolean) is
   begin
      Success := False;
      if not Object.Started or else Object.Broken then return; end if;
      Directory.Extend_Metadata (Object.Directory, Base, Bytes, Success);
   end Extend_Metadata;

   procedure Start
     (Object : in out Assembly; CPU_Base, Arena_ID : Unsigned_64;
      Success : out Boolean; Required_Blocks : Natural :=
        Natural (Intel_GPU_Buffer_Backing.Default_Heap.Byte_Quota / E.Block_Bytes);
      Byte_Quota : Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.Byte_Quota;
      DMA_Limit : Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.DMA_Limit) is
   begin
      Success := False;
      if Object.Started or else Object.Broken then return; end if;
      Object.Started := True;
      if not Intel_GPU_Buffer_Backing.Heap_Geometry_Valid (Byte_Quota, DMA_Limit, CPU_Base) or else
        Required_Blocks = 0 or else
        Unsigned_64 (Required_Blocks) > Byte_Quota / E.Block_Bytes or else
        Arena_ID = 0
      then Cancel (Object); return; end if;
      Object.CPU := CPU_Base;
      Object.Identity := Arena_ID;
      Object.Wanted := Required_Blocks;
      Object.Limit := Natural (Byte_Quota / E.Block_Bytes);
      Object.DMA_Limit := DMA_Limit;
      Directory.Initialize (Object.Directory, Byte_Quota, DMA_Limit, Success);
      if not Success then Cancel (Object); end if;
   end Start;

   procedure Extend
     (Object : in out Assembly; Required_Blocks : Natural; Success : out Boolean) is
   begin
      Success := False;
      if not Object.Started or else Object.Broken or else
        Object.Count /= Object.Wanted or else not Directory.Ready (Object.Directory) or else
        Required_Blocks <= Object.Count or else Required_Blocks > Object.Limit
      then Cancel (Object); return; end if;
      Object.Wanted := Required_Blocks;
      Success := True;
   end Extend;

   procedure Accept_Reply
     (Object : in out Assembly; Data : Words; Success : out Boolean) is
   begin
      Success := False;
      if not Object.Started or else Object.Broken or else
        Object.Count >= Object.Wanted or else Object.Count >= Object.Limit then
         Cancel (Object); return;
      end if;
      if Data (0) /= Unsigned_64 (Object.Count) or else
        Data (3) /= Object.Identity or else
        Data (2) /= Object.CPU + Unsigned_64 (Object.Count) * E.Block_Bytes or else
        Data (1) = 0 or else Data (1) mod E.Block_Bytes /= 0 or else
        Data (1) > Object.DMA_Limit - E.Block_Bytes
      then Cancel (Object); return; end if;
      Directory.Append (Object.Directory, Data (1), Success);
      if not Success then Cancel (Object); return; end if;
      Object.Count := Object.Count + 1;
      -- Append validates each immutable entry. Result publishes the prefix
      -- only when complete; no second fixed-size map needs rebuilding.
      Success := True;
   end Accept_Reply;

   function Result (Object : Assembly) return Directory.Borrowed_View is
      Empty : Directory.Borrowed_View;
      Retained : Directory.Borrowed_View;
   begin
      if not Object.Started or else Object.Broken or else Object.Count /= Object.Wanted then
         return Empty;
      end if;
      -- Borrow's trusted stable-owner contract applies here: Assembly remains
      -- alive until every produced BO is retired. Capture the descriptor as a
      -- value, not an aliased-formal function call in the return expression.
      Retained := Directory.Borrow (Object.Directory);
      return Retained;
   end Result;
end Intel_GPU_Extent_Replies;
