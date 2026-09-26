--  Native schema metadata for authenticated provisioning, not a serialized
--  Registry memory dump. No private Ada representations cross this boundary.
--  A successful import validates shape; it does NOT authenticate the claimed
--  identity or grant authority to register/use the schema. The caller must
--  establish its approved source and reject conflicting bindings for a key.
package CCL.Objects.Schemas with SPARK_Mode is
   Version : constant Unsigned_32 := 1;
   Native_Schema_Bytes : constant := 7 * 4096;
   --  Explicit wire IDs, independent of enum representation. Declared types
   --  are numbered 7..38 in declaration order, with backward references only.
   Integer_ID : constant Unsigned_32 := 1;
   Boolean_ID : constant Unsigned_32 := 2;
   String_ID : constant Unsigned_32 := 3;
   Character_ID : constant Unsigned_32 := 4;
   Handler_ID : constant Unsigned_32 := 5;
   Unit_ID : constant Unsigned_32 := 6;
   Product_Form : constant Unsigned_32 := 1;
   Sum_Form : constant Unsigned_32 := 2;
   type Byte_Array is array (Positive range <>) of Unsigned_8 with Component_Size => 8;
   type Native_Name is record
      Length : Unsigned_32 := 0;
      Text : String (1 .. Types.Maximum_Name_Length) := [others => Character'Val (0)];
   end record with Size => 36 * 8;
   for Native_Name use record
      Length at 0 range 0 .. 31;
      Text at 4 range 0 .. 255;
   end record;
   type Native_Part is record
      Identifier : Native_Name;
      Payload : Unsigned_32 := 0;
   end record with Size => 40 * 8;
   for Native_Part use record
      Identifier at 0 range 0 .. 287;
      Payload at 36 range 0 .. 31;
   end record;
   type Part_Array is array (Types.Component_Index) of Native_Part with Component_Size => 40 * 8;
   type Native_Definition is record
      Identifier : Native_Name;
      Form, Count, Reserved : Unsigned_32 := 0;
      Parts : Part_Array;
      Padding : Byte_Array (1 .. 80) := [others => 0];
   end record with Size => 768 * 8;
   for Native_Definition use record
      Identifier at 0 range 0 .. 287;
      Form at 36 range 0 .. 31;
      Count at 40 range 0 .. 31;
      Reserved at 44 range 0 .. 31;
      Parts at 48 range 0 .. 16 * 40 * 8 - 1;
      Padding at 688 range 0 .. 80 * 8 - 1;
   end record;
   type Definition_Array is array (1 .. Types.Maximum_Declarations) of Native_Definition
     with Component_Size => 768 * 8;
   type Image is record
      Key : Schema_Key := No_Schema;
      Format : Unsigned_32 := Version;
      Root, Count, Reserved : Unsigned_32 := 0;
      Padding : Byte_Array (1 .. 4096 - 48) := [others => 0];
      Definitions : Definition_Array;
   end record with Size => Native_Schema_Bytes * 8, Alignment => 4096;
   for Image use record
      Key at 0 range 0 .. 255;
      Format at 32 range 0 .. 31;
      Root at 36 range 0 .. 31;
      Count at 40 range 0 .. 31;
      Reserved at 44 range 0 .. 31;
      Padding at 48 range 0 .. (4096 - 48) * 8 - 1;
      Definitions at 4096 range 0 .. 32 * 768 * 8 - 1;
   end record;
   --  Snapshot imported shared memory first, then call Read on owned data.
   -- Export only the root's transitive data definitions, with translated IDs.
   -- Unrelated visible resources/types do not belong in a persisted schema.
   procedure Write (Contract : Binding; Data : out Image; Accepted : out Boolean);
   procedure Read (Data : Image; Contract : out Binding; Accepted : out Boolean);
   --  Read routes definitions through Types.Define and Bind, including bounded
   --  layout, no forward/cyclic references, and no handlers in persisted data.
private
   --  Shared by the native metadata and portable persistence encoders.
   function Root_Closure (Contract : Binding) return Binding;
   function Wire_ID (Ref : Types.Type_Reference) return Unsigned_32;
end CCL.Objects.Schemas;
