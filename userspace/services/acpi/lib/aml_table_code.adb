with Firmware_Tables;
package body AML_Table_Code with SPARK_Mode is
   procedure Visit_Body (Input : aliased AML_Table_Backing.State;
     Table : Positive; Status : out View_Status) is
   begin
      Status := Invalid_Span;
      if Input.Count > Input.Table_Capacity or else Table > Input.Count
        or else not AML_Table_Backing.Valid_Span (Input, Table)
      then return; end if;
      -- Valid_Span establishes header <= extent <= capacity - offset.
      -- Offset is zero-based and remains valid for an empty body at capacity.
      Consume (Input,
        Input.Tables (Table).Offset + Firmware_Tables.Table_Header_Size,
        Input.Tables (Table).Extent - Firmware_Tables.Table_Header_Size);
      Status := Body_Visited;
   end Visit_Body;
end AML_Table_Code;
