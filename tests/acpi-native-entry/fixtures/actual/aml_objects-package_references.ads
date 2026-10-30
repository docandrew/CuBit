pragma Ada_2022;
with AML_Index_Handles;
package AML_Objects.Package_References with SPARK_Mode, Pure is
   -- Internal arena-local descriptors, not IPC capabilities. Keep the owning
   -- arena alive and never use a descriptor with a different/reset arena.
   subtype Reference is AML_Index_Handles.Package_Reference;
   use type Reference;
   No_Reference : Reference renames AML_Index_Handles.No_Package_Reference;
   type Result_Status is (Ready, Invalid_Object, Wrong_Kind, Out_Of_Bounds, Invalid_Reference, Invalid_Value);
   function Owner (Ref : Reference) return Object_ID;
   function Offset (Ref : Reference) return Natural;
   function Is_Valid (Store : State; Ref : Reference) return Boolean
     with Pre => Valid (Store);
   procedure Make
     (Store : State; Source : Object_ID; Index : AML_Decode.Integer_Value;
      Ref : out Reference; Status : out Result_Status)
   with Pre => Valid (Store),
     Post => (if Status = Ready then
       Is_Valid (Store, Ref) and then Owner (Ref) = Source
       and then AML_Decode.Integer_Value (Offset (Ref)) = Index
       else Ref = No_Reference);
   procedure Read
     (Store : State; Ref : Reference; Value : out Object_ID;
      Status : out Result_Status)
   with Pre => Valid (Store),
     Post => (if Is_Valid (Store, Ref) then
       Status = Ready and then Value = Element (Store, Owner (Ref), Offset (Ref))
       else Status = Invalid_Reference and then Value = 0);
   procedure Write
     (Store : in out State; Ref : Reference; Value : Object_ID;
      Status : out Result_Status)
   with Pre => Valid (Store),
     Post => Valid (Store)
       and then Slot_Bound (Store) = Slot_Bound (Store'Old)
       and then Usage_Of (Store) = Usage_Of (Store'Old)
       and then (for all J in 1 .. Slot_Bound (Store) => (if Is_Live (Store, J) then
         Kind (Store, J) = Kind (Store'Old, J)
         and then Length (Store, J) = Length (Store'Old, J)
         and then (if Kind (Store'Old, J) = Integer_Object then
           Integer_Data (Store, J) = Integer_Data (Store'Old, J))))
       and then (if Is_Valid (Store'Old, Ref) and then (Value = No_Object or else Is_Live (Store'Old, Value)) then
         Status = Ready and then Is_Valid (Store, Ref)
         and then Element_Updated (Store, Store'Old, Owner (Ref), Offset (Ref), Value)
         and then Element (Store, Owner (Ref), Offset (Ref)) = Value
         else Status = (if Is_Valid (Store'Old, Ref) then Invalid_Value else Invalid_Reference)
           and then Store = Store'Old);
private
   function Owner (Ref : Reference) return Object_ID is (AML_Index_Handles.Owner (Ref));
   function Offset (Ref : Reference) return Natural is (AML_Index_Handles.Offset (Ref));
   function Is_Valid (Store : State; Ref : Reference) return Boolean is
     (AML_Index_Handles.Present (Ref) and then Matches_Address (Store, AML_Index_Handles.Address (Ref)) and then Is_Live (Store, Owner (Ref))
      and then Kind (Store, Owner (Ref)) = Package_Object
      and then Offset (Ref) < Length (Store, Owner (Ref)));
end AML_Objects.Package_References;
