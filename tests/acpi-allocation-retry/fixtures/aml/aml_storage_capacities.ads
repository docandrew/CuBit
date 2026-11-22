with AML_Objects;
package AML_Storage_Capacities with SPARK_Mode, Pure is
   subtype Input_Byte_Count is Natural;
   type Limits is record
      Objects : AML_Objects.Storage_Capacity := AML_Objects.Max_Objects;
      Bytes : AML_Objects.Storage_Capacity := AML_Objects.Max_Bytes;
      Elements : AML_Objects.Storage_Capacity := AML_Objects.Max_Elements;
   end record;
   -- Initial allocation policy, not an upper bound on what AML can allocate.
   -- Preserve the seed proportions, rounded up, against immutable catalog
   -- capacity plus explicitly provisioned copied-table capacity. Runtime loops
   -- can still exhaust any finite store; that remains a typed resource refusal.
   Source_Bytes_Per_Object : constant Positive := AML_Objects.Max_Bytes / AML_Objects.Max_Objects;
   Source_Bytes_Per_Element : constant Positive := AML_Objects.Max_Bytes / AML_Objects.Max_Elements;
   function Supported (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0) return Boolean;
   -- Unsupported configurations return seed shapes for safe representation;
   -- admission MUST reject them using Supported before allocating an owner.
   function Object_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return AML_Objects.Storage_Capacity;
   function Byte_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return AML_Objects.Storage_Capacity;
   function Element_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return AML_Objects.Storage_Capacity;
   function For_Input (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0) return Limits;
end AML_Storage_Capacities;
