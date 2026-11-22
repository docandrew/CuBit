------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  PCI MSI and MSI-X encodings and programming plans - implementation
------------------------------------------------------------------------------
package body PCI_Message_Interrupts with
   SPARK_Mode => On
is
   function Program_Entry
     (Destination : Destination_ID; Vector : Device_Vector; Unmask : Boolean)
      return Entry_Plan
   is
      P : Entry_Plan;
   begin
      P.Writes (1) := (Vector_Control, VECTOR_MASKED);
      P.Writes (2) := (Address_Low, Message_Address (Destination));
      P.Writes (3) := (Address_High, 0);
      P.Writes (4) := (Data, Message_Data (Vector));
      if Unmask then
         P.Writes (5) := (Vector_Control, VECTOR_UNMASKED);
         P.Length := 5;
      else
         P.Length := 4;
      end if;
      return P;
   end Program_Entry;

   function Mask_Entry return Entry_Plan is
      P : Entry_Plan;
   begin
      P.Writes (1) := (Vector_Control, VECTOR_MASKED);
      P.Length := 1;
      return P;
   end Mask_Entry;

   function Program_MSI
     (Message_Control : Unsigned_16; Destination : Destination_ID; Vector : Device_Vector)
      return Config_Plan
   is
      P : Config_Plan;
   begin
      P.Writes (1) :=
        (Control, MSI_CONTROL_OFFSET, Word_16, Unsigned_32 (MSI_Disabled (Message_Control)));
      P.Writes (2) :=
        (Address_Low, MSI_ADDRESS_LOW_OFFSET, Word_32, Message_Address (Destination));
      if Is_64_Bit (Message_Control) then
         P.Writes (3) := (Address_High, MSI_ADDRESS_HIGH_OFFSET, Word_32, 0);
         P.Writes (4) := (Data, MSI_DATA_64_OFFSET, Word_16, Message_Data (Vector));
         P.Writes (5) := (Control, MSI_CONTROL_OFFSET, Word_16,
                          Unsigned_32 (MSI_Enabled_Single (Message_Control)));
         P.Length := 5;
      else
         P.Writes (3) := (Data, MSI_DATA_32_OFFSET, Word_16, Message_Data (Vector));
         P.Writes (4) := (Control, MSI_CONTROL_OFFSET, Word_16,
                          Unsigned_32 (MSI_Enabled_Single (Message_Control)));
         P.Length := 4;
      end if;
      return P;
   end Program_MSI;

   function Prepare_MSI
     (Message_Control : Unsigned_16; Destination : Destination_ID; Vector : Device_Vector)
      return Config_Plan
   is
      P : Config_Plan;
   begin
      P.Writes (1) :=
        (Control, MSI_CONTROL_OFFSET, Word_16, Unsigned_32 (MSI_Disabled (Message_Control)));
      P.Writes (2) :=
        (Address_Low, MSI_ADDRESS_LOW_OFFSET, Word_32, Message_Address (Destination));
      if Is_64_Bit (Message_Control) then
         P.Writes (3) := (Address_High, MSI_ADDRESS_HIGH_OFFSET, Word_32, 0);
         P.Writes (4) := (Data, MSI_DATA_64_OFFSET, Word_16, Message_Data (Vector));
         P.Length := 4;
      else
         P.Writes (3) := (Data, MSI_DATA_32_OFFSET, Word_16, Message_Data (Vector));
         P.Length := 3;
      end if;
      return P;
   end Prepare_MSI;

   function Disable_MSI (Message_Control : Unsigned_16) return Config_Plan is
      P : Config_Plan;
   begin
      P.Writes (1) :=
        (Control, MSI_CONTROL_OFFSET, Word_16, Unsigned_32 (MSI_Disabled (Message_Control)));
      P.Length := 1;
      return P;
   end Disable_MSI;
end PCI_Message_Interrupts;
