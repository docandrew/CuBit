------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  devmgr's authority - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.IPC;

package body Devmgr_Authority is

   package BR renames CuBit.Handles.Boot_Roots;
   use type H.ABI.Kind;

   Held : BR.Roots;

   type Extent is record
      First, Bytes : Unsigned_64 := 0;
   end record;
   Extents : array (BR.Root_Index) of Extent;

   procedure Initialize is
   begin
      Held := BR.Get;
      Extents := [others => (others => <>)];
      if not Held.Valid then
         return;
      end if;
      for R in 1 .. Held.Count loop
         declare
            D : constant CuBit.Handles.IPC.Description :=
              CuBit.Handles.IPC.Describe (Held.Memory (R));
         begin
            if D.Current and then D.Kind = H.ABI.Device_Memory then
               Extents (R) := (First => D.Object, Bytes => D.Parameter);
            end if;
         end;
      end loop;
   end Initialize;

   function Valid return Boolean is (Held.Valid);
   function Roots return BR.Roots is (Held);

   function Device_Region (Physical, Bytes : Unsigned_64) return H.Device_Memory is
   begin
      if Bytes = 0 or else Physical > Unsigned_64'Last - Bytes then
         return H.No_Device_Memory;
      end if;
      for R in 1 .. Held.Count loop
         if Extents (R).Bytes /= 0
           and then Physical >= Extents (R).First
           and then Physical - Extents (R).First <= Extents (R).Bytes
           and then Bytes <= Extents (R).Bytes - (Physical - Extents (R).First)
         then
            return Held.Memory (R);
         end if;
      end loop;
      return H.No_Device_Memory;
   end Device_Region;

   function Device_Copy (Physical, Bytes : Unsigned_64; Kept : H.Rights) return H.Derivation is
     (H.Narrowed (Device_Region (Physical, Bytes), Kept, Physical, Bytes));

end Devmgr_Authority;
