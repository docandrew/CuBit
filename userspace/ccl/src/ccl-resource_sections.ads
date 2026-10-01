with Interfaces; use Interfaces;

--  The .cubit.resources ELF section: device resources and real-time
--  scheduling that an executable requests (docs/ccl-driver-manifests.md).
--  CCL.Manifests writes it; startup reads it with Decode, which treats the
--  bytes as untrusted and accepts exactly what the compiler can produce.
--  A decoded section is still only a request: policy decides what is granted.
package CCL.Resource_Sections with Pure, SPARK_Mode => On is
   MAGIC : constant := 16#5352_4243#;   --  "CBRS" little-endian
   VERSION : constant := 1;
   HEADER_BYTES : constant := 8;
   MATCH_BYTES : constant := 16;
   ENTRY_BYTES : constant := 24;
   MAX_ENTRIES : constant := 32;
   MAX_SECTION_BYTES : constant := HEADER_BYTES + MATCH_BYTES + MAX_ENTRIES * ENTRY_BYTES;

   type Match_Kind is (No_Match, PCI_Class_Match, PCI_ID_Match, Platform_Match);
   for Match_Kind use
     (No_Match => 0, PCI_Class_Match => 1, PCI_ID_Match => 2, Platform_Match => 3);
   --  Legacy devices with fixed resources, described by devmgr's platform
   --  catalog; resource indexes refer to that catalog's entry.
   type Platform_Device is (PS2_Controller, ATA_Primary, CMOS_RTC);
   for Platform_Device use (PS2_Controller => 1, ATA_Primary => 2, CMOS_RTC => 3);
   type Resource_Kind is (Device_Memory, IO_Ports, Interrupt, DMA, Scheduling);
   for Resource_Kind use
     (Device_Memory => 16, IO_Ports => 17, Interrupt => 18, DMA => 19, Scheduling => 20);
   subtype Device_Resource is Resource_Kind range Device_Memory .. DMA;
   type Interrupt_Mode is (MSI_X, MSI, Line, Platform_Line);
   for Interrupt_Mode use (MSI_X => 1, MSI => 2, Line => 3, Platform_Line => 4);
   type Rights_Kind is (Read_Only, Write_Only, Read_Write);
   for Rights_Kind use (Read_Only => 1, Write_Only => 2, Read_Write => 3);

   PAGE_BYTES : constant := 4_096;
   PCI_BAR_COUNT : constant := 6;
   MAX_PLATFORM_RESOURCES : constant := 8;
   MAX_DEVICE_MEMORY_BYTES : constant := 256 * 1_024 * 1_024;
   MAX_DMA_BYTES : constant := 64 * 1_024 * 1_024;
   IO_PORT_SPACE : constant := 65_536;
   MAX_INTERRUPT_VECTORS : constant := 32;
   PCI_ID_LAST : constant := 16#FFFF#;
   PCI_CODE_LAST : constant := 16#FF#;

   --  Slot 0 is bootstrap, 62 a driver's saved reply capability, 63 the
   --  reply slot: a resource never lands in any of them.
   subtype Slot_Number is Natural range 1 .. 61;

   subtype Match_Value_Index is Positive range 1 .. 3;
   type Match_Values is array (Match_Value_Index) of Unsigned_16;
   type Match_Info is record
      Kind : Match_Kind := No_Match;
      Values : Match_Values := [others => 0];
   end record;

   type Resource is record
      Kind : Resource_Kind := Scheduling;
      Rights : Rights_Kind := Read_Only;
      Slot : Slot_Number := Slot_Number'First;
      --  BAR or platform resource index.
      Index : Unsigned_32 := 0;
      --  Bytes, ports, vectors, or the scheduling budget in microseconds.
      Amount : Unsigned_64 := 0;
      --  The interrupt mode, or the scheduling period in microseconds.
      Extra : Unsigned_64 := 0;
   end record;

   subtype Entry_Count is Natural range 0 .. MAX_ENTRIES;
   type Resource_Array is array (Positive range 1 .. MAX_ENTRIES) of Resource;
   type Section_Plan is record
      Match : Match_Info;
      Count : Entry_Count := 0;
      Entries : Resource_Array := [others => (others => <>)];
   end record;

   type Decode_Status is
     (Decoded, Invalid_Length, Invalid_Header, Invalid_Match, Invalid_Entry,
      Duplicate_Slot, Device_Without_Match, Match_Without_Device);

   --  Whether Match is one the compiler can emit.
   function Valid_Match (Match : Match_Info) return Boolean;
   --  Whether Item is a resource the compiler can emit for Match.
   function Valid_Resource (Match : Match_Info; Item : Resource) return Boolean;

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   procedure Decode
     (Data : Byte_Array; Plan : out Section_Plan; Status : out Decode_Status)
   with Pre  => Data'First = 1 and then Data'Length <= MAX_SECTION_BYTES,
        Post => (if Status = Decoded then
                   Plan.Count >= 1
                   and then Valid_Match (Plan.Match)
                   and then (for all I in 1 .. Plan.Count =>
                               Valid_Resource (Plan.Match, Plan.Entries (I)))
                   and then (for all I in 1 .. Plan.Count =>
                               (for all J in 1 .. Plan.Count =>
                                  I = J or else
                                  Plan.Entries (I).Slot /= Plan.Entries (J).Slot))
                   and then (Plan.Match.Kind /= No_Match) =
                     (for some I in 1 .. Plan.Count =>
                        Plan.Entries (I).Kind in Device_Resource));
end CCL.Resource_Sections;
