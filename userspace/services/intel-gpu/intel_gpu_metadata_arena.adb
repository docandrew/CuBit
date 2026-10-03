package body Intel_GPU_Metadata_Arena is
   function Snapshot (Object : Arena) return View is (Object.Data);
   procedure Open (Object : in out Arena; Limit : Unsigned_64; Accepted : out Boolean) is
      Base : Unsigned_64;
   begin
      Accepted := False;
      if Object.Data.Phase /= Empty or else Limit = 0 or else
        Limit mod Page_Bytes /= 0 then return; end if;
      Object.Data.Limit := Limit;
      Object.Data.Phase := Failed;
      Base := Reserve (Limit);
      if Base = 0 or else Base mod Page_Bytes /= 0 or else
        Base > Unsigned_64'Last - Limit then return; end if;
      Object.Data.Base := Base;
      Object.Data.Phase := Reserved;
      Accepted := True;
   end Open;
   procedure Request (Object : in out Arena; Bytes : Unsigned_64; Accepted : out Boolean) is
      Rounded : Unsigned_64;
   begin
      Accepted := False;
      if Object.Data.Phase not in Reserved | Ready or else Bytes = 0 or else
        Bytes > Object.Data.Limit then return; end if;
      -- Limit is page aligned. Subtraction avoids overflow near U64'Last.
      Rounded := Bytes - 1;
      Rounded := Rounded - Rounded mod Page_Bytes + Page_Bytes;
      if Rounded <= Object.Data.Published then Accepted := True; return; end if;
      Object.Data.Wanted := Rounded;
      Object.Data.Phase := Growing;
      Accepted := True;
   end Request;
   procedure Step (Object : in out Arena) is
      Bytes, Offset : Unsigned_64;
   begin
      if Object.Data.Phase /= Growing then return; end if;
      Offset := Object.Data.Committed;
      Bytes := Unsigned_64'Min (Step_Bytes, Object.Data.Wanted - Offset);
      -- Mark failure before invoking callbacks: a failed commit/initialization
      -- is never silently replayed, nor does it publish uninitialized records.
      Object.Data.Phase := Failed;
      if not Commit (Object.Data.Base, Offset, Bytes) then return; end if;
      Object.Data.Committed := Offset + Bytes;
      if not Initialize (Object.Data.Base + Offset, Bytes) then return; end if;
      if Object.Data.Committed = Object.Data.Wanted then
         Object.Data.Published := Object.Data.Wanted;
         Object.Data.Phase := Ready;
      else
         Object.Data.Phase := Growing;
      end if;
   end Step;
   function Address (Object : Arena; Offset, Bytes : Unsigned_64) return Unsigned_64 is
     (if Bytes /= 0 and then Offset <= Object.Data.Published and then
         Bytes <= Object.Data.Published - Offset
      then Object.Data.Base + Offset else 0);
end Intel_GPU_Metadata_Arena;
