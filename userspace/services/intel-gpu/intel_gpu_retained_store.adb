with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Retained_Store is
   use type Storage.State;
   use type System.Address;
   Pointer_Bytes : constant Unsigned_64 := Element_Access'Object_Size / 8;
   function Element_Bytes return Unsigned_64 is
     ((Unsigned_64 (Element'Object_Size / 8) + 4095) / 4096 * 4096);
   pragma Compile_Time_Error
     (Element'Object_Size mod 8 /= 0 or Element'Object_Size = 0 or
      Element'Object_Size > 65536 * 8 or Element'Alignment > 4096,
      "retained record initialization must fit one bounded metadata step");
   function State (Object : Store) return Phase is (Object.Status);
   function Charged_Bytes (Object : Store) return Unsigned_64 is (Object.Charged);
   function Lookup (Object : Store; Index : Positive) return Element_Access is
     (if Owner_Ready and then Index <= Pointers.Capacity (Object.Items)
      then Pointers.Get (Object.Items, Index) else null);
   function Disjoint (A, B : Storage.View) return Boolean is
     (A.Base = 0 or else B.Base = 0 or else
      (if A.Base <= B.Base then A.Limit <= B.Base - A.Base
       else B.Limit <= A.Base - B.Base));
   procedure Request
     (Object : in out Store; Account : not null access Budget; Index : Positive;
      Index_Byte_Quota, Element_Byte_Quota : Unsigned_64; Accepted : out Boolean)
   is
      Needed : constant Unsigned_64 :=
        (if Index <= Bootstrap_Count then 0 else
         Unsigned_64 (Index - Bootstrap_Count) * Pointer_Bytes);
   begin
      Accepted := False;
      if Object.Status /= Idle or else not Owner_Ready or else
        (Object.Budget_Address /= System.Null_Address and then
         Object.Budget_Address /= Account.all'Address) or else
        Index_Byte_Quota = 0 or else Index_Byte_Quota mod Storage.Page_Bytes /= 0 or else
        Element_Byte_Quota < Element_Bytes or else
        Element_Byte_Quota mod Storage.Page_Bytes /= 0 or else
        Needed > Index_Byte_Quota or else
        (Object.Index_Limit /= 0 and then Object.Index_Limit /= Index_Byte_Quota) or else
        (Object.Element_Limit /= 0 and then Object.Element_Limit /= Element_Byte_Quota)
      then return; end if;
      Object.Index_Limit := Index_Byte_Quota;
      Object.Budget_Address := Account.all'Address;
      Object.Element_Limit := Element_Byte_Quota;
      Object.Target := Index;
      Object.Index_Wanted := (if Needed = 0 then 0 else (Needed - 1) / 4096 * 4096 + 4096);
      Object.Status := Checking;
      Accepted := True;
   end Request;
   procedure Step (Object : in out Store; Account : not null access Budget) is
      OK : Boolean;
      V : Storage.View;
      Base : Unsigned_64;
      Delta_Bytes : Unsigned_64;
      function Reserve_Charge (Bytes : Unsigned_64) return Boolean is
      begin
         if Bytes = 0 or else Bytes > Unsigned_64'Last - Object.Charged or else
           not Charge (Account.all, Bytes) then return False; end if;
         Object.Charged := Object.Charged + Bytes;
         return Owner_Ready;
      end Reserve_Charge;
      function Pointer is new Ada.Unchecked_Conversion (System.Address, Element_Access);
   begin
      if Object.Status in Idle | Failed then return; end if;
      if not Owner_Ready or else Object.Budget_Address /= Account.all'Address then
         Object.Status := Failed; return;
      end if;
      case Object.Status is
         when Checking =>
            if Object.Target > Pointers.Capacity (Object.Items) then
               Object.Status := (if Storage.Snapshot (Object.Index_Arena).Phase = Storage.Empty
                                 then Opening_Index else Requesting_Index);
            elsif Pointers.Get (Object.Items, Object.Target) /= null then
               Object.Status := Idle;
            else
               Object.Status := (if Storage.Snapshot (Object.Element_Arena).Phase = Storage.Empty
                                 then Opening_Elements else Requesting_Element);
            end if;
         when Opening_Index =>
            Object.Status := Failed;
            Storage.Open (Object.Index_Arena, Object.Index_Limit, OK);
            if OK and then Disjoint (Storage.Snapshot (Object.Index_Arena),
                                      Storage.Snapshot (Object.Element_Arena))
            then Object.Status := Requesting_Index; end if;
         when Requesting_Index =>
            Object.Status := Failed;
            V := Storage.Snapshot (Object.Index_Arena);
            if V.Published >= Object.Index_Wanted then return; end if;
            Delta_Bytes := Unsigned_64'Min (Storage.Step_Bytes, Object.Index_Wanted - V.Published);
            if not Reserve_Charge (Delta_Bytes) then return; end if;
            Storage.Request (Object.Index_Arena, V.Published + Delta_Bytes, OK);
            if OK then Object.Status := Committing_Index; end if;
         when Committing_Index =>
            Object.Status := Failed;
            Storage.Step (Object.Index_Arena);
            if Storage.Snapshot (Object.Index_Arena).Phase = Storage.Ready then
               Object.Status := Publishing_Index;
            end if;
         when Publishing_Index =>
            Object.Status := Failed;
            V := Storage.Snapshot (Object.Index_Arena);
            Pointers.Extend (Object.Items, V.Base, V.Published, OK);
            if OK then Object.Status := Checking; end if;
         when Opening_Elements =>
            Object.Status := Failed;
            Storage.Open (Object.Element_Arena, Object.Element_Limit, OK);
            if OK and then Disjoint (Storage.Snapshot (Object.Index_Arena),
                                      Storage.Snapshot (Object.Element_Arena))
            then Object.Status := Requesting_Element; end if;
         when Requesting_Element =>
            Object.Status := Failed;
            V := Storage.Snapshot (Object.Element_Arena);
            if V.Published > Object.Element_Limit or else
              Element_Bytes > Object.Element_Limit - V.Published then return; end if;
            Object.Element_Offset := V.Published;
            if not Reserve_Charge (Element_Bytes) then return; end if;
            Storage.Request (Object.Element_Arena, V.Published + Element_Bytes, OK);
            if OK then Object.Status := Committing_Element; end if;
         when Committing_Element =>
            Object.Status := Failed;
            Storage.Step (Object.Element_Arena);
            if Storage.Snapshot (Object.Element_Arena).Phase = Storage.Ready then
               Object.Status := Installing;
            end if;
         when Installing =>
            Object.Status := Failed;
            Base := Storage.Address (Object.Element_Arena, Object.Element_Offset, Element_Bytes);
            if Base = 0 or else Pointers.Get (Object.Items, Object.Target) /= null then return; end if;
            declare
               Location : constant System.Address := To_Address (Integer_Address (Base));
               -- Placement elaboration applies typed defaults, including those
               -- of limited arenas. Clearing bytes alone is not initialization.
               Fresh : Element with Address => Location;
               pragma Unreferenced (Fresh);
            begin
               Pointers.Put (Object.Items, Object.Target, Pointer (Location));
            end;
            Object.Status := Idle;
         when Idle | Failed => null;
      end case;
      if not Owner_Ready then Object.Status := Failed; end if;
   end Step;
end Intel_GPU_Retained_Store;
