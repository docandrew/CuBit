with Device_Vector_ABI; use Device_Vector_ABI;
package body Device_Model is
   type Table_Entry is record
      Address_Low, Address_High, Data : Unsigned_32 := 0;
      Vector_Control : Unsigned_32 := VECTOR_MASKED;
      Pending : Boolean := False;
   end record;
   Table : array (Model_Entry) of Table_Entry;

   Is_MSIX : Boolean := True;
   --  MSI-X message control (table size TABLE_ENTRIES) or MSI control
   --  (64-bit capable, one message).
   Control : Unsigned_16 := 0;
   MSI_Address_Low, MSI_Address_High : Unsigned_32 := 0;
   MSI_Data : Unsigned_16 := 0;
   MSI_Pending : Boolean := False;

   procedure Reset_Device (MSIX : Boolean) is
   begin
      Is_MSIX := MSIX;
      Table := [others => (others => <>)];
      Control := (if MSIX then Unsigned_16 (TABLE_ENTRIES - 1)
                  else MSI_64_BIT_ADDRESS);
      MSI_Address_Low := 0; MSI_Address_High := 0; MSI_Data := 0;
      MSI_Pending := False;
   end Reset_Device;

   procedure Send (Address_Low, Address_High, Data : Unsigned_32) is
      use Device_Vectors;
      Vector : constant Natural := Natural (Data and MESSAGE_VECTOR_MASK);
      Owner : Owner_Key;
   begin
      if Address_High /= 0 or else
        (Address_Low and not MESSAGE_ADDRESS_WINDOW) /= MESSAGE_ADDRESS_BASE
      then
         Wrong_Address := Wrong_Address + 1;
         return;
      end if;
      if Vector not in Device_Vector then
         Stray := Stray + 1;
         return;
      end if;
      Owner := Recipient (Kernel, Vector);
      if Owner = NO_OWNER then
         Stray := Stray + 1;
      else
         Delivered (Owner) := Delivered (Owner) + 1;
      end if;
   end Send;

   function Function_Open return Boolean is
     ((Control and MSIX_ENABLE) /= 0 and then (Control and MSIX_FUNCTION_MASK) = 0);

   procedure Flush_Pending is
   begin
      if Is_MSIX and then Function_Open then
         for E of Table loop
            if E.Pending and then (E.Vector_Control and VECTOR_MASKED) = 0 then
               E.Pending := False;
               Send (E.Address_Low, E.Address_High, E.Data);
            end if;
         end loop;
      end if;
   end Flush_Pending;

   procedure Write_Entry
     (Index : Entry_Index; Register : Entry_Register; Value : Unsigned_32) is
   begin
      if Index > Model_Entry'Last then
         raise Program_Error with "entry outside the table";
      end if;
      declare
         E : Table_Entry renames Table (Index);
      begin
         if Register /= Vector_Control and then
           (E.Vector_Control and VECTOR_MASKED) = 0
         then
            Hazards := Hazards + 1;
         end if;
         case Register is
            when Address_Low    => E.Address_Low := Value;
            when Address_High   => E.Address_High := Value;
            when Data           => E.Data := Value;
            when Vector_Control => E.Vector_Control := Value;
         end case;
      end;
      Flush_Pending;
   end Write_Entry;

   function Read_Config_16 (Offset : Natural) return Unsigned_16 is
   begin
      if Offset = MSIX_CONTROL_OFFSET then
         return Control;
      end if;
      raise Program_Error with "unexpected 16-bit read";
   end Read_Config_16;

   procedure Write_Config_16 (Offset : Natural; Value : Unsigned_16) is
   begin
      if Offset = MSIX_CONTROL_OFFSET then
         if not Is_MSIX and then (Value and MSI_ENABLE) /= 0 and then
           (Value and MSI_MULTIPLE_ENABLE) /= 0
         then
            raise Program_Error with "more than one MSI message enabled";
         end if;
         Control := Value;
         if Is_MSIX then
            Flush_Pending;
         elsif (Control and MSI_ENABLE) /= 0 and then MSI_Pending then
            MSI_Pending := False;
            Send (MSI_Address_Low, MSI_Address_High, Unsigned_32 (MSI_Data));
         end if;
      elsif not Is_MSIX and then Offset = MSI_DATA_64_OFFSET then
         if (Control and MSI_ENABLE) /= 0 then
            Hazards := Hazards + 1;
         end if;
         MSI_Data := Value;
      else
         raise Program_Error with "unexpected 16-bit write";
      end if;
   end Write_Config_16;

   procedure Write_Config_32 (Offset : Natural; Value : Unsigned_32) is
   begin
      if Is_MSIX then
         raise Program_Error with "unexpected 32-bit write";
      end if;
      if (Control and MSI_ENABLE) /= 0 then
         Hazards := Hazards + 1;
      end if;
      case Offset is
         when MSI_ADDRESS_LOW_OFFSET  => MSI_Address_Low := Value;
         when MSI_ADDRESS_HIGH_OFFSET => MSI_Address_High := Value;
         when others => raise Program_Error with "unexpected 32-bit offset";
      end case;
   end Write_Config_32;

   procedure Raise_Interrupt (Index : Model_Entry) is
   begin
      if not Is_MSIX then
         if (Control and MSI_ENABLE) /= 0 then
            Send (MSI_Address_Low, MSI_Address_High, Unsigned_32 (MSI_Data));
         else
            MSI_Pending := True;
         end if;
         return;
      end if;
      if (Control and MSIX_ENABLE) = 0 then
         return;   --  MSI-X off: the function signals nothing
      end if;
      Table (Index).Pending := True;
      Flush_Pending;
   end Raise_Interrupt;

   function Pending (Index : Model_Entry) return Boolean is
     (Table (Index).Pending);
end Device_Model;
