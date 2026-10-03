with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Application_State is
   use type Interfaces.Unsigned_64;
   function Update_Storage_Bytes return Interfaces.Unsigned_64 is
     (((Interfaces.Unsigned_64 (Update_Record'Object_Size) + 7) / 8 + 4095) / 4096 * 4096);
   procedure Install_Fresh_Update
     (Index : Positive; Base, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean) is
      function Pointer is new Ada.Unchecked_Conversion (System.Address, Update_Access);
      Needed : constant Interfaces.Unsigned_64 := Update_Storage_Bytes;
      Other : Interfaces.Unsigned_64;
   begin
      Accepted := False;
      if Index <= Bootstrap_Updates or else Index > Update_Capacity or else
        Has_Update (Index) or else Base = 0 or else Base mod 4096 /= 0 or else
        Base mod Interfaces.Unsigned_64 (Update_Record'Alignment) /= 0 or else
        Bytes < Needed or else Bytes mod 4096 /= 0 or else
        Base > Interfaces.Unsigned_64'Last - Bytes then return; end if;
      for I in 1 .. Update_Capacity loop
         if Has_Update (I) then
            Other := Interfaces.Unsigned_64 (To_Integer (Updates (I).all'Address));
            if (Base <= Other and then Other - Base < Bytes) or else
              (Other < Base and then Base - Other < Needed) then return; end if;
         end if;
      end loop;
      declare
         -- Deliberately not Import: placement elaboration applies the type's
         -- defaults, rather than assuming cleared bytes represent every field.
         Fresh : Update_Record := (others => <>)
           with Address => To_Address (Integer_Address (Base));
         pragma Unreferenced (Fresh);
      begin
         Update_References.Put (References, Index, Pointer (To_Address (Integer_Address (Base))));
      end;
      Accepted := True;
   end Install_Fresh_Update;
   function Update_Capacity return Positive is
     (Update_References.Capacity (References));
   function Updates (Index : Positive) return Update_Access is
   begin
      if Index <= Bootstrap_Updates then
         return Inline_Updates (Index)'Access;
      elsif Index <= Update_Capacity then
         return Update_References.Get (References, Index);
      else return null;
      end if;
   end Updates;
   function Has_Update (Index : Positive) return Boolean is
     (Updates (Index) /= null);
   procedure Extend_Update_Index
     (Base, Bytes : Interfaces.Unsigned_64; Accepted : out Boolean) is
   begin
      Update_References.Extend (References, Base, Bytes, Accepted);
   end Extend_Update_Index;
   procedure Install_Update
     (Index : Positive; Item : Update_Access; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Item = null or else Index <= Bootstrap_Updates or else
        Index > Update_Capacity or else Has_Update (Index)
      then return; end if;
      for I in 1 .. Update_Capacity loop
         if Updates (I) = Item then return; end if;
      end loop;
      Update_References.Put (References, Index, Item);
      Accepted := True;
   end Install_Update;
end Intel_GPU_Application_State;
