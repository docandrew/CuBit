with AML_Object_Identifiers;
package AML_Index_Handles with SPARK_Mode, Pure is
   -- Structural arena-local metadata only; owner validates kind and live bounds.
   subtype Object_ID is AML_Object_Identifiers.Object_ID;
   subtype Object_Address is AML_Object_Identifiers.Object_Address;
   type Byte_Reference is private;
   No_Byte_Reference : constant Byte_Reference;
   function Bind_Byte (Source : Object_Address; Index : Natural) return Byte_Reference;
   function Present (Ref : Byte_Reference) return Boolean;
   function Owner (Ref : Byte_Reference) return Object_ID;
   function Address (Ref : Byte_Reference) return Object_Address;
   function Offset (Ref : Byte_Reference) return Natural;
   type Package_Reference is private;
   No_Package_Reference : constant Package_Reference;
   function Bind_Package (Source : Object_Address; Index : Natural) return Package_Reference;
   function Present (Ref : Package_Reference) return Boolean;
   function Owner (Ref : Package_Reference) return Object_ID;
   function Address (Ref : Package_Reference) return Object_Address;
   function Offset (Ref : Package_Reference) return Natural;
private
   type Byte_Reference is record
      Source : Object_Address := AML_Object_Identifiers.No_Address;
      Index : Natural := 0;
   end record;
   No_Byte_Reference : constant Byte_Reference := (others => <>);
   function Bind_Byte (Source : Object_Address; Index : Natural) return Byte_Reference is
     (if not AML_Object_Identifiers.Present (Source) then No_Byte_Reference else (Source, Index));
   function Present (Ref : Byte_Reference) return Boolean is (AML_Object_Identifiers.Present (Ref.Source));
   function Owner (Ref : Byte_Reference) return Object_ID is (AML_Object_Identifiers.Slot_Of (Ref.Source));
   function Address (Ref : Byte_Reference) return Object_Address is (Ref.Source);
   function Offset (Ref : Byte_Reference) return Natural is (Ref.Index);
   type Package_Reference is record
      Source : Object_Address := AML_Object_Identifiers.No_Address;
      Index : Natural := 0;
   end record;
   No_Package_Reference : constant Package_Reference := (others => <>);
   function Bind_Package (Source : Object_Address; Index : Natural) return Package_Reference is
     (if not AML_Object_Identifiers.Present (Source) then No_Package_Reference else (Source, Index));
   function Present (Ref : Package_Reference) return Boolean is (AML_Object_Identifiers.Present (Ref.Source));
   function Owner (Ref : Package_Reference) return Object_ID is (AML_Object_Identifiers.Slot_Of (Ref.Source));
   function Address (Ref : Package_Reference) return Object_Address is (Ref.Source);
   function Offset (Ref : Package_Reference) return Natural is (Ref.Index);
end AML_Index_Handles;
