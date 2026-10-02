with Interfaces; use Interfaces;
with Mesa_Cache;
with Compositor_Formats; use Compositor_Formats;
function Native_Target_Retirement_Test return Boolean is
   type Pixels is array (Natural range <>) of Unsigned_32;
   Source : aliased Pixels (0 .. 15);
   A, B : aliased Pixels (0 .. 63);
   Cache : Mesa_Cache.State;
   Source_Index : Mesa_Cache.Source_Slot;
   OK : Boolean;
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   procedure Report with Import, Convention => C,
     External_Name => "compositor_target_retirement_report";
begin
   Mesa_Cache.Initialize (Cache, True);
   for Cycle in 1 .. 32 loop
      declare
         Stride : constant Natural := (if Cycle mod 2 = 0 then 16 else 8);
         Height : constant Word := (if Cycle mod 2 = 0 then 4 else 8);
         D : constant Draw := (0, 0, 4, 4, 2, 0, 4, 4, 2, 0, 4, 4, 0);
      begin
         for I in Source'Range loop Source (I) := 16#FF00_0000# + Unsigned_32 (Cycle * 256 + I); end loop;
         A := (others => Sentinel); B := (others => Sentinel);
         Mesa_Cache.Ensure (Cache, 0, (A'Address, 8, Height, Word (Stride * 4), 1), 256, OK);
         if not OK then return False; end if;
         Mesa_Cache.Ensure (Cache, 1, (B'Address, 8, Height, Word (Stride * 4), 1), 256, OK);
         if not OK then return False; end if;
         Mesa_Cache.Ensure_Source (Cache, (Source'Address, 4, 4, 16, 0), 64, Source_Index, OK);
         if not OK then return False; end if;
         for Target in Mesa_Cache.Target_Slot loop
            Mesa_Cache.Render (Cache, Target, Source_Index, D, OK);
            if not OK then return False; end if;
         end loop;
         for I in A'Range loop
            declare
               X : constant Natural := I mod Stride;
               Y : constant Natural := I / Stride;
               Expected : constant Unsigned_32 :=
                 (if X in 2 .. 5 and Y < 4 then Source (Y * 4 + X - 2) else Sentinel);
            begin
               if A (I) /= Expected or B (I) /= Expected then return False; end if;
            end;
         end loop;
         Mesa_Cache.Forget_Targets (Cache);
         if not Mesa_Cache.Can_Retire (Cache) or else not Mesa_Cache.Targets_Clear (Cache) or else
           Mesa_Cache.Empty (Cache, Source_Index)
         then return False; end if;
         -- Reuse the exact storage at the next iteration with changed layout
         -- and pixels, while keeping the source view and context alive.
         Mesa_Cache.Forget_Targets (Cache);
         if not Mesa_Cache.Can_Retire (Cache) then return False; end if;
      end;
   end loop;
   Mesa_Cache.Shutdown (Cache);
   if not Mesa_Cache.Can_Retire (Cache) then return False; end if;
   Report;
   return True;
end Native_Target_Retirement_Test;
