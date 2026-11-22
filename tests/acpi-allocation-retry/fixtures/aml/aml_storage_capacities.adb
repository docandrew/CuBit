package body AML_Storage_Capacities with SPARK_Mode is
   subtype Capacity is AML_Objects.Storage_Capacity;
   function Supported (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0) return Boolean is
     (Table_Bytes <= Capacity'Last and then Copied_Bytes <= Capacity'Last - Table_Bytes);
   function Basis (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count) return Capacity is
     (if Supported (Table_Bytes, Copied_Bytes) then Table_Bytes + Copied_Bytes
      else AML_Objects.Max_Bytes);
   function Round_Up (Bytes : Capacity; Unit_Size : Positive) return Capacity is
     (1 + (Bytes - 1) / Unit_Size);
   function Object_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return Capacity is
     (Capacity'Max (AML_Objects.Max_Objects,
       Round_Up (Basis (Table_Bytes, Copied_Bytes), Source_Bytes_Per_Object)));
   function Byte_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return Capacity is
     (Capacity'Max (AML_Objects.Max_Bytes, Basis (Table_Bytes, Copied_Bytes)));
   function Element_Limit (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0)
      return Capacity is
     (Capacity'Max (AML_Objects.Max_Elements,
       Round_Up (Basis (Table_Bytes, Copied_Bytes), Source_Bytes_Per_Element)));
   function For_Input (Table_Bytes : Positive; Copied_Bytes : Input_Byte_Count := 0) return Limits is
     (Objects => Object_Limit (Table_Bytes, Copied_Bytes),
      Bytes => Byte_Limit (Table_Bytes, Copied_Bytes),
      Elements => Element_Limit (Table_Bytes, Copied_Bytes));
end AML_Storage_Capacities;
