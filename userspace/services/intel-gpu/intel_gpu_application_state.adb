with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Application_State is
   use type Interfaces.Unsigned_64;
   Record_Bytes : constant Interfaces.Unsigned_64 :=
     (Interfaces.Unsigned_64 (Update_Record'Object_Size) + 7) / 8;
   function Update_Storage_Bytes return Interfaces.Unsigned_64 is
     (((Interfaces.Unsigned_64 (Update_Record'Object_Size) + 7) / 8 + 4095) / 4096 * 4096);
   function State (Object : Installation) return Installation_Phase is (Object.Phase);
   function Checked (Object : Installation) return Natural is (Object.Probes);
   procedure Begin_Fresh_Update
     (Object : in out Installation; Index : Positive; Base, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean) is
      Needed : constant Interfaces.Unsigned_64 := Update_Storage_Bytes;
   begin
      Accepted := False;
      if Object.Phase /= Idle then return; end if;
      Object.Phase := Failed;
      if not Ranges_Ready or else Index <= Bootstrap_Updates or else Index > Update_Capacity or else
        Has_Update (Index) or else Base = 0 or else Base mod 4096 /= 0 or else
        Base mod Interfaces.Unsigned_64 (Update_Record'Alignment) /= 0 or else
        Bytes < Needed or else Bytes mod 4096 /= 0 or else
        Base > Interfaces.Unsigned_64'Last - Bytes or else
        Registry_Epoch = Interfaces.Unsigned_64'Last then return; end if;
      Object.Index := Index; Object.Base := Base; Object.Bytes := Bytes;
      Object.Epoch := Registry_Epoch;
      Object.Phase := Checking; Accepted := True;
   end Begin_Fresh_Update;
   procedure Step_Fresh_Update (Object : in out Installation) is
      function Pointer is new Ada.Unchecked_Conversion (System.Address, Update_Access);
      Checking_Now : constant Boolean := Object.Phase = Checking;
      Overlaps, OK : Boolean;
      Visits : Natural;
      Base : constant Interfaces.Unsigned_64 := Object.Base;
      Bytes : constant Interfaces.Unsigned_64 := Object.Bytes;
   begin
      if Object.Phase not in Checking | Publishing then return; end if;
      Object.Phase := Failed;
      if Object.Epoch /= Registry_Epoch or else Has_Update (Object.Index) then return; end if;
      if Checking_Now then
         Intel_GPU_Metadata_Ranges.Conflict (Ranges, Base, Bytes, Overlaps, Object.Probes);
         if not Overlaps then Object.Phase := Publishing; end if;
         return;
      end if;
      declare
         -- Deliberately not Import: placement elaboration applies the type's
         -- defaults, rather than assuming cleared bytes represent every field.
         Fresh : Update_Record := (others => <>)
           with Address => To_Address (Integer_Address (Base));
         Item : constant Update_Access := Pointer (To_Address (Integer_Address (Base)));
         pragma Unreferenced (Fresh);
      begin
         Intel_GPU_Metadata_Ranges.Insert
           (Ranges, Item.Metadata_Range'Access, Base, Bytes, OK, Visits);
         if not OK then return; end if;
         Update_References.Put (References, Object.Index, Item);
      end;
      Registry_Epoch := Registry_Epoch + 1;
      Object.Phase := Complete;
   end Step_Fresh_Update;
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
      Accepted := False;
      if Registry_Epoch = Interfaces.Unsigned_64'Last then return; end if;
      Update_References.Extend (References, Base, Bytes, Accepted);
      if Accepted then Registry_Epoch := Registry_Epoch + 1; end if;
   end Extend_Update_Index;
   procedure Install_Update
     (Index : Positive; Item : Update_Access; Accepted : out Boolean) is
      Visits : Natural;
   begin
      Accepted := False;
      if not Ranges_Ready or else Registry_Epoch = Interfaces.Unsigned_64'Last or else
        Item = null or else Index <= Bootstrap_Updates or else
        Index > Update_Capacity or else Has_Update (Index)
      then return; end if;
      Intel_GPU_Metadata_Ranges.Insert (Ranges, Item.Metadata_Range'Access,
        Interfaces.Unsigned_64 (To_Integer (Item.all'Address)), Record_Bytes,
        Accepted, Visits);
      if not Accepted then return; end if;
      Update_References.Put (References, Index, Item);
      Registry_Epoch := Registry_Epoch + 1;
      Accepted := True;
   end Install_Update;
begin
   -- Fixed bootstrap only; dynamic images register their embedded nodes on
   -- installation. Exact object spans avoid inventing page-padding ownership
   -- for adjacent statically allocated records.
   declare OK : Boolean; Visits : Natural; begin
      Ranges_Ready := True;
      for I in Inline_Updates'Range loop
         Intel_GPU_Metadata_Ranges.Insert (Ranges,
           Inline_Updates (I).Metadata_Range'Access,
           Interfaces.Unsigned_64 (To_Integer (Inline_Updates (I)'Address)),
           Record_Bytes, OK, Visits);
         if not OK then Ranges_Ready := False; exit; end if;
      end loop;
   end;
end Intel_GPU_Application_State;
