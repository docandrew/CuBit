package body Intel_GPU_Buffer_Reply with SPARK_Mode is
   function Valid (Object : Extent_View) return Boolean is (Object.Accepted);
   function From_Extents
     (Map : Intel_GPU_Physical_Extents.Map; Arena_ID, Offset, Bytes : Unsigned_64)
      return Extent_View is
      Empty : Extent_View;
   begin
      if not Intel_GPU_Physical_Extents.Ready (Map) or else Arena_ID = 0 or else
        Bytes = 0 or else (Offset or Bytes) mod 4096 /= 0 or else
        Offset >= Intel_GPU_Physical_Extents.Capacity or else
        Bytes > Intel_GPU_Physical_Extents.Capacity - Offset
      then return Empty; end if;
      return (True, Arena_ID, Offset, Bytes, Map, 0);
   end From_Extents;
   function Slice (Parent : Extent_View; Offset, Bytes : Unsigned_64) return Extent_View is
      Empty : Extent_View;
   begin
      if not Valid (Parent) or else Offset >= Parent.Length or else
        Bytes > Parent.Length - Offset then return Empty; end if;
      if Bytes = 0 or else (Offset or Bytes) mod 4096 /= 0 then return Empty; end if;
      return (True, Parent.Identity, Parent.First + Offset, Bytes,
              Parent.Map, Parent.Linear_Base);
   end Slice;
   function Page_Address (Parent : Extent_View; Offset : Unsigned_64) return Unsigned_64 is
      Span : Intel_GPU_Physical_Extents.Span;
   begin
      --  The admitted map owns the backing geometry. Page lookup checks only
      --  this view's bounds; it does not reconstruct or revalidate the arena.
      if not Valid (Parent) or else Offset mod 4096 /= 0 or else
        Offset >= Parent.Length or else Parent.Length - Offset < 4096
      then return 0; end if;
      if Parent.Linear_Base /= 0 then
         return Parent.Linear_Base + Parent.First + Offset;
      end if;
      Span := Intel_GPU_Physical_Extents.Resolve
        (Parent.Map, Parent.First + Offset, 4096);
      return (if Span.Valid and then Span.Bytes = 4096 then Span.Address else 0);
   end Page_Address;
   function CPU_Address (Object : Extent_View) return Unsigned_64 is
     (if Valid (Object) then Layout.CPU_Base + Object.First else 0);
   function Byte_Count (Object : Extent_View) return Unsigned_64 is
     (if Valid (Object) then Object.Length else 0);
   function Same_Arena (Left, Right : Extent_View) return Boolean is
      use type Intel_GPU_Physical_Extents.Map;
   begin
      return Valid (Left) and then Valid (Right) and then
        Left.Identity = Right.Identity and then Left.Map = Right.Map and then
        Left.Linear_Base = Right.Linear_Base;
   end Same_Arena;

   function Valid (Object : Backing) return Boolean is
     (Object.Ready and then Valid (Object.View) and then Object.Bytes > 0 and then Object.Bytes mod 4096 = 0
      and then Object.Bytes <= Unsigned_64 (Layout.Page_Count'Last) * 4096
      and then Object.CPU_Address = CPU_Address (Object.View)
      and then Object.Bytes = Byte_Count (Object.View));
   function From_View (View : Extent_View) return Backing is
   begin
      if not Valid (View) or else Byte_Count (View) >
        Unsigned_64 (Layout.Page_Count'Last) * 4096
      then return (Ready => False); end if;
      return (True, CPU_Address (View), Byte_Count (View), View);
   end From_View;
   function From_Linear (DMA, CPU, Bytes, Arena_DMA : Unsigned_64) return Backing is
      View : Extent_View;
   begin
      if Bytes = 0 or else Bytes mod 4096 /= 0 or else
        Bytes > Unsigned_64 (Layout.Page_Count'Last) * 4096 or else
        not Valid (Layout.Page_Count (Bytes / 4096), DMA, CPU, Bytes) or else
        Arena_DMA /= DMA - (CPU - Layout.CPU_Base)
      then return (Ready => False); end if;
      View.Accepted := True;
      View.Identity := Arena_DMA;
      View.First := CPU - Layout.CPU_Base;
      View.Length := Bytes;
      View.Linear_Base := Arena_DMA;
      return From_View (View);
   end From_Linear;
   function Overlaps_DMA (Object : Extent_View; First, Bytes : Unsigned_64)
     return Boolean is
      Offset : Unsigned_64 := 0;
      Part : Intel_GPU_Physical_Extents.Span;
   begin
      if not Valid (Object) or else Bytes = 0 or else
        First > Unsigned_64'Last - (Bytes - 1)
      then return True; end if;
      if Object.Linear_Base /= 0 then
         declare
            Address : constant Unsigned_64 := Object.Linear_Base + Object.First;
         begin
            return (if First >= Address then First - Address < Object.Length
                    else Address - First < Bytes);
         end;
      end if;
      while Offset < Object.Length loop
         Part := Intel_GPU_Physical_Extents.Resolve
           (Object.Map, Object.First + Offset, Object.Length - Offset);
         if not Part.Valid or else Part.Bytes = 0 then return True; end if;
         if (if First >= Part.Address then First - Part.Address < Part.Bytes
             else Part.Address - First < Bytes)
         then return True; end if;
         Offset := Offset + Part.Bytes;
      end loop;
      return False;
   end Overlaps_DMA;
   function Same_Arena (Left, Right : Backing) return Boolean is
     (Valid (Left) and then Valid (Right) and then Same_Arena (Left.View, Right.View));
   function Overlaps_DMA (Object : Backing; First, Bytes : Unsigned_64)
     return Boolean is
   begin
      if not Valid (Object) or else Bytes = 0 or else First > Unsigned_64'Last - (Bytes - 1)
      then return True; end if;
      return Overlaps_DMA (Object.View, First, Bytes);
   end Overlaps_DMA;
   function Page_Address (Parent : Backing; Offset : Unsigned_64)
     return Unsigned_64 is
   begin
      return (if Valid (Parent) then Page_Address (Parent.View, Offset) else 0);
   end Page_Address;

   function Slice (Parent : Backing; Offset, Bytes : Unsigned_64) return Backing is
   begin
      if not Valid (Parent) or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Offset mod 4096 /= 0 or else
        Offset >= Parent.Bytes or else Bytes > Parent.Bytes - Offset
      then return (Ready => False); end if;
      return From_View (Slice (Parent.View, Offset, Bytes));
   end Slice;

   function Classify
     (Index : Layout.Slot; Pages : Layout.Page_Count;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Words) return Outcome is
   begin
      if Flags /= 0 or Reserved /= 0 then return Invalid; end if;
      if Length = 0 and Data = [0, 0, 0, 0] then
         if Label = 16#F002# then return Retry;
         elsif Label = 16#F001# then return Denied; end if;
      end if;
      if Label = 16#F000# and Length = 4 and
        Valid (Pages, Data (0), Data (1), Data (2)) and
        Data (3) = Unsigned_64 (Index) then return Granted; end if;
      return Invalid;
   end Classify;
   function Decode
     (Index : Layout.Slot; Pages : Layout.Page_Count;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Words) return Backing is
   begin
      if Classify (Index, Pages, Label, Length, Flags, Reserved, Data) /= Granted then
         return (Ready => False);
      end if;
      return From_Linear (Data (0), Data (1), Data (2),
              Data (0) - (Data (1) - Layout.CPU_Base));
   end Decode;
end Intel_GPU_Buffer_Reply;
