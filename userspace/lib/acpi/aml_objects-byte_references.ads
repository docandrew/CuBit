pragma Ada_2022;
package AML_Objects.Byte_References with SPARK_Mode, Pure is
   -- Internal arena-local descriptors, not IPC capabilities. Keep the owning
   -- arena alive and never use a descriptor with a different/reset arena.
   type Reference is private;
   No_Reference : constant Reference;
   type Result_Status is (Ready, Invalid_Object, Wrong_Kind, Out_Of_Bounds, Invalid_Reference);
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
     (Store : State; Ref : Reference; Value : out AML_Decode.Byte;
      Status : out Result_Status)
   with Pre => Valid (Store),
     Post => (if Is_Valid (Store, Ref) then
       Status = Ready and then Value = Stored_Byte (Store, Owner (Ref), Offset (Ref))
       else Status = Invalid_Reference and then Value = 0);
   procedure Write
     (Store : in out State; Ref : Reference; Value : AML_Decode.Byte;
      Status : out Result_Status)
   with Pre => Valid (Store),
     Post => Valid (Store)
       and then Count (Store) = Count (Store'Old)
       and then Usage_Of (Store) = Usage_Of (Store'Old)
       and then (for all J in 1 .. Count (Store) =>
         Kind (Store, J) = Kind (Store'Old, J)
         and then Length (Store, J) = Length (Store'Old, J))
       and then (if Is_Valid (Store'Old, Ref) then
         Status = Ready and then Is_Valid (Store, Ref)
         and then Stored_Byte_Updated (Store, Store'Old, Owner (Ref), Offset (Ref), Value)
         and then Stored_Byte (Store, Owner (Ref), Offset (Ref)) = Value
         else Status = Invalid_Reference and then Store = Store'Old);
private
   type Reference is record
      Present : Boolean := False;
      Source : Object_ID := No_Object;
      Index : Natural := 0;
   end record;
   No_Reference : constant Reference := (others => <>);
   function Owner (Ref : Reference) return Object_ID is (Ref.Source);
   function Offset (Ref : Reference) return Natural is (Ref.Index);
   function Is_Valid (Store : State; Ref : Reference) return Boolean is
     (Ref.Present and then Ref.Source > 0 and then Ref.Source <= Count (Store)
      and then Kind (Store, Ref.Source) in Byte_Kind
      and then Ref.Index < Length (Store, Ref.Source));
end AML_Objects.Byte_References;
