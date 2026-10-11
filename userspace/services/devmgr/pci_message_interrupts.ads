------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  PCI MSI and MSI-X encodings and programming plans (docs/msi-interrupts.md)
--
--  @description
--  Pure: register layouts, x86 message address/data, and the exact sequence
--  of writes devmgr performs. A plan is data; devmgr's executor writes it in
--  order. The proved rule (gnatprove level 2, tests/msi-vectors) is that no
--  message address or data is ever written while the device may send it:
--   * MSI-X: every entry write other than Vector Control follows a write
--     that masks the entry, with no unmask in between;
--   * MSI: every address/data write follows a control write that clears
--     MSI Enable, with no control write in between; MSI has one message
--     (Multiple Message Enable = 0) and no per-vector mask is relied on.
--  Sources: PCI Local Bus 3.0 6.8.1-6.8.2; Intel SDM Vol. 3 11.11.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Device_Vector_ABI; use Device_Vector_ABI;

package PCI_Message_Interrupts with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Capability IDs (PCI Code and ID Assignment, 2.1)
   ---------------------------------------------------------------------------
   MSI_CAPABILITY_ID  : constant Unsigned_8 := 16#05#;
   MSIX_CAPABILITY_ID : constant Unsigned_8 := 16#11#;

   ---------------------------------------------------------------------------
   --  x86 message address and data: fixed delivery, edge, physical
   --  destination mode, no redirection hint.
   ---------------------------------------------------------------------------
   MESSAGE_ADDRESS_BASE   : constant Unsigned_32 := 16#FEE0_0000#;
   MESSAGE_ADDRESS_WINDOW : constant Unsigned_32 := 16#000F_FFFF#;
   MESSAGE_DESTINATION_SHIFT : constant := 12;
   MESSAGE_VECTOR_MASK    : constant Unsigned_32 := 16#FF#;

   function Message_Address (Destination : Destination_ID) return Unsigned_32 is
     (MESSAGE_ADDRESS_BASE or
      Shift_Left (Unsigned_32 (Destination), MESSAGE_DESTINATION_SHIFT))
   with Post =>
     (Message_Address'Result and not MESSAGE_ADDRESS_WINDOW) = MESSAGE_ADDRESS_BASE
     and then (Shift_Right (Message_Address'Result, MESSAGE_DESTINATION_SHIFT)
                 and 16#FF#) = Unsigned_32 (Destination);

   --  Delivery mode Fixed (bits 8-10 = 0), trigger Edge (bit 15 = 0).
   function Message_Data (Vector : Device_Vector) return Unsigned_32 is
     (Unsigned_32 (Vector))
   with Post => (Message_Data'Result and MESSAGE_VECTOR_MASK) = Unsigned_32 (Vector)
     and then (Message_Data'Result and not MESSAGE_VECTOR_MASK) = 0;

   ---------------------------------------------------------------------------
   --  MSI capability (offsets from the capability's start)
   ---------------------------------------------------------------------------
   MSI_CONTROL_OFFSET      : constant := 2;
   MSI_ADDRESS_LOW_OFFSET  : constant := 4;
   MSI_ADDRESS_HIGH_OFFSET : constant := 8;
   MSI_DATA_32_OFFSET      : constant := 8;    --  32-bit address layout
   MSI_DATA_64_OFFSET      : constant := 12;   --  64-bit address layout

   MSI_ENABLE                : constant Unsigned_16 := 16#0001#;
   MSI_MULTIPLE_ENABLE       : constant Unsigned_16 := 16#0070#;
   MSI_64_BIT_ADDRESS        : constant Unsigned_16 := 16#0080#;

   function Is_64_Bit (Control : Unsigned_16) return Boolean is
     ((Control and MSI_64_BIT_ADDRESS) /= 0);

   --  MSI off; everything else as it was.
   function MSI_Disabled (Control : Unsigned_16) return Unsigned_16 is
     (Control and not MSI_ENABLE);

   --  MSI on with exactly one message (Multiple Message Enable = 0).
   function MSI_Enabled_Single (Control : Unsigned_16) return Unsigned_16 is
     ((Control and not MSI_MULTIPLE_ENABLE) or MSI_ENABLE)
   with Post => (MSI_Enabled_Single'Result and MSI_ENABLE) /= 0
     and then (MSI_Enabled_Single'Result and MSI_MULTIPLE_ENABLE) = 0;

   ---------------------------------------------------------------------------
   --  MSI-X capability
   ---------------------------------------------------------------------------
   MSIX_CONTROL_OFFSET : constant := 2;
   MSIX_TABLE_OFFSET   : constant := 4;
   MSIX_PBA_OFFSET     : constant := 8;

   MSIX_TABLE_SIZE_MASK : constant Unsigned_16 := 16#07FF#;
   MSIX_FUNCTION_MASK   : constant Unsigned_16 := 16#4000#;
   MSIX_ENABLE          : constant Unsigned_16 := 16#8000#;

   --  Table and PBA locator: BAR indicator in bits 0-2, QWORD-aligned offset.
   LOCATOR_BIR_MASK    : constant Unsigned_32 := 16#0000_0007#;
   LOCATOR_OFFSET_MASK : constant Unsigned_32 := 16#FFFF_FFF8#;
   BAR_COUNT : constant := 6;

   MAX_TABLE_ENTRIES : constant := 2_048;
   subtype Table_Entry_Count is Positive range 1 .. MAX_TABLE_ENTRIES;
   subtype Entry_Index is Natural range 0 .. MAX_TABLE_ENTRIES - 1;
   ENTRY_BYTES : constant := 16;

   function Table_Size (Control : Unsigned_16) return Table_Entry_Count is
     (Natural (Control and MSIX_TABLE_SIZE_MASK) + 1);

   function Locator_BAR (Locator : Unsigned_32) return Natural is
     (Natural (Locator and LOCATOR_BIR_MASK));
   function Locator_Offset (Locator : Unsigned_32) return Unsigned_64 is
     (Unsigned_64 (Locator and LOCATOR_OFFSET_MASK));

   --  Enabled with the whole function masked: the state for programming.
   function MSIX_Enabled_Masked (Control : Unsigned_16) return Unsigned_16 is
     (Control or MSIX_ENABLE or MSIX_FUNCTION_MASK);
   --  Function mask dropped; per-entry masks now decide.
   function MSIX_Function_Unmasked (Control : Unsigned_16) return Unsigned_16 is
     (Control and not MSIX_FUNCTION_MASK);

   --  One table entry's registers, at their byte offsets in the entry.
   type Entry_Register is (Address_Low, Address_High, Data, Vector_Control);
   for Entry_Register use
     (Address_Low => 0, Address_High => 4, Data => 8, Vector_Control => 12);

   VECTOR_MASKED   : constant Unsigned_32 := 16#0000_0001#;
   VECTOR_UNMASKED : constant Unsigned_32 := 16#0000_0000#;

   --  Byte offset of a register of entry Index from the table's start.
   function Entry_Offset (Index : Entry_Index; Register : Entry_Register)
     return Unsigned_64 is
     (Unsigned_64 (Index) * ENTRY_BYTES +
      Unsigned_64 (Entry_Register'Enum_Rep (Register)));

   ---------------------------------------------------------------------------
   --  MSI-X entry plans
   ---------------------------------------------------------------------------
   type Entry_Write is record
      Register : Entry_Register := Vector_Control;
      Value    : Unsigned_32 := VECTOR_MASKED;
   end record;

   MAX_ENTRY_WRITES : constant := 5;
   subtype Entry_Write_Count is Natural range 0 .. MAX_ENTRY_WRITES;
   subtype Entry_Write_Index is Positive range 1 .. MAX_ENTRY_WRITES;
   type Entry_Writes is array (Entry_Write_Index) of Entry_Write;

   type Entry_Plan is record
      Length : Entry_Write_Count := 0;
      Writes : Entry_Writes := [others => (others => <>)];
   end record;

   function Masks (W : Entry_Write) return Boolean is
     (W.Register = Vector_Control and then W.Value = VECTOR_MASKED);

   --  Every address or data write follows a mask with no Vector Control
   --  write in between.
   function Masked_While_Writing (P : Entry_Plan) return Boolean is
     (for all I in 1 .. P.Length =>
        (if P.Writes (I).Register /= Vector_Control then
           (for some J in 1 .. I - 1 =>
              Masks (P.Writes (J))
              and then (for all K in J + 1 .. I - 1 =>
                          P.Writes (K).Register /= Vector_Control))));

   --  Point entry at Vector on Destination: mask, address, data, and
   --  unmask only when Unmask (devmgr keeps the function mask meanwhile).
   function Program_Entry
     (Destination : Destination_ID; Vector : Device_Vector; Unmask : Boolean)
      return Entry_Plan
   with Post =>
     Masked_While_Writing (Program_Entry'Result)
     and then Program_Entry'Result.Length = (if Unmask then 5 else 4)
     and then Masks (Program_Entry'Result.Writes (1))
     and then Program_Entry'Result.Writes (2)
                = (Address_Low, Message_Address (Destination))
     and then Program_Entry'Result.Writes (3) = (Address_High, 0)
     and then Program_Entry'Result.Writes (4) = (Data, Message_Data (Vector))
     and then (if Unmask then
                 Program_Entry'Result.Writes (5) = (Vector_Control, VECTOR_UNMASKED));

   --  Teardown: the one write that stops an entry before its vector is
   --  released.
   function Mask_Entry return Entry_Plan
   with Post => Mask_Entry'Result.Length = 1
     and then Masks (Mask_Entry'Result.Writes (1))
     and then Masked_While_Writing (Mask_Entry'Result);

   ---------------------------------------------------------------------------
   --  MSI configuration plans (offsets from the capability's start)
   ---------------------------------------------------------------------------
   type Config_Register is (Control, Address_Low, Address_High, Data);
   type Write_Width is (Word_16, Word_32);

   type Config_Write is record
      Register : Config_Register := Control;
      Offset   : Natural range 0 .. MSI_DATA_64_OFFSET := MSI_CONTROL_OFFSET;
      Width    : Write_Width := Word_16;
      Value    : Unsigned_32 := 0;
   end record;

   MAX_CONFIG_WRITES : constant := 5;
   subtype Config_Write_Count is Natural range 0 .. MAX_CONFIG_WRITES;
   subtype Config_Write_Index is Positive range 1 .. MAX_CONFIG_WRITES;
   type Config_Writes is array (Config_Write_Index) of Config_Write;

   type Config_Plan is record
      Length : Config_Write_Count := 0;
      Writes : Config_Writes := [others => (others => <>)];
   end record;

   function Disables (W : Config_Write) return Boolean is
     (W.Register = Control and then (W.Value and Unsigned_32 (MSI_ENABLE)) = 0);

   --  Every address or data write follows a control write that clears MSI
   --  Enable, with no control write in between.
   function Disabled_While_Writing (P : Config_Plan) return Boolean is
     (for all I in 1 .. P.Length =>
        (if P.Writes (I).Register /= Control then
           (for some J in 1 .. I - 1 =>
              Disables (P.Writes (J))
              and then (for all K in J + 1 .. I - 1 =>
                          P.Writes (K).Register /= Control))));

   --  Point the single MSI message at Vector on Destination, then enable it.
   function Program_MSI
     (Message_Control : Unsigned_16; Destination : Destination_ID; Vector : Device_Vector)
      return Config_Plan
   with Post =>
     Disabled_While_Writing (Program_MSI'Result)
     and then Program_MSI'Result.Length = (if Is_64_Bit (Message_Control) then 5 else 4)
     and then Disables (Program_MSI'Result.Writes (1))
     and then Program_MSI'Result.Writes (2)
                = (Address_Low, MSI_ADDRESS_LOW_OFFSET, Word_32,
                   Message_Address (Destination))
     and then Program_MSI'Result.Writes (Program_MSI'Result.Length)
                = (Control, MSI_CONTROL_OFFSET, Word_16,
                   Unsigned_32 (MSI_Enabled_Single (Message_Control)))
     and then Program_MSI'Result.Writes (Program_MSI'Result.Length - 1)
                = (Data, (if Is_64_Bit (Message_Control) then MSI_DATA_64_OFFSET
                          else MSI_DATA_32_OFFSET), Word_16,
                   Message_Data (Vector));

   --  Program_MSI without the final enable: MSI stays off until its driver
   --  has the device's own interrupt sources ready (intel-gpu).
   function Prepare_MSI
     (Message_Control : Unsigned_16; Destination : Destination_ID; Vector : Device_Vector)
      return Config_Plan
   with Post =>
     Disabled_While_Writing (Prepare_MSI'Result)
     and then Prepare_MSI'Result.Length = (if Is_64_Bit (Message_Control) then 4 else 3)
     and then Disables (Prepare_MSI'Result.Writes (1))
     and then (for all I in 2 .. Prepare_MSI'Result.Length =>
                 Prepare_MSI'Result.Writes (I).Register /= Control);

   --  Teardown: MSI Enable cleared, nothing else touched.
   function Disable_MSI (Message_Control : Unsigned_16) return Config_Plan
   with Post => Disable_MSI'Result.Length = 1
     and then Disables (Disable_MSI'Result.Writes (1))
     and then Disabled_While_Writing (Disable_MSI'Result);
end PCI_Message_Interrupts;
