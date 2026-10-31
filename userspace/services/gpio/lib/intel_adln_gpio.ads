pragma Ada_2022;
with Interfaces;
package Intel_ADLN_GPIO with SPARK_Mode, Pure is
   Target_HID : constant String := "INTC1057";
   type ACPI_Pin is range 0 .. 65_535;
   type Pad_Number is range 0 .. 256;
   type Community is (Community_0, Community_1, Community_4, Community_5);
   type Memory_Resource_Ordinal is range 0 .. 3;
   type Group_ID is (GPP_B, GPP_T, GPP_A, GPP_S, GPP_I, GPP_H,
      GPP_D, Virtual_GPIO, GPP_C, GPP_F, GPP_E, GPP_R);
   type Group_Bit is range 0 .. 31;
   type Owner_Shift is range 0 .. 28;
   type Register_Offset is range 0 .. 16#FFFF_FFFF#;
   subtype Word is Interfaces.Unsigned_32;
   subtype Span is Interfaces.Unsigned_64;
   Register_Bytes : constant Register_Offset := 4;
   Revision_Register : constant Register_Offset := 16#000#;
   PADBAR_Register : constant Register_Offset := 16#00C#;
   PAD_OWN_Base : constant Register_Offset := 16#020#;
   PADCFGLOCK_Base : constant Register_Offset := 16#080#;
   HOSTSW_OWN_Base : constant Register_Offset := 16#0B0#;
   GPI_Status_Base : constant Register_Offset := 16#100#;
   GPI_Enable_Base : constant Register_Offset := 16#120#;
   type Location (Found : Boolean := False) is record
      case Found is
         when True =>
            Pad : Pad_Number;
            In_Community : Community;
            Resource : Memory_Resource_Ordinal;
            Group : Group_ID;
            Bit_In_Group : Group_Bit;
            Community_Pad : Pad_Number;
            Interrupt_Mask : Word;
            Owner_Register, Host_Register, Lock_Register, TX_Lock_Register,
              Status_Register, Enable_Register : Register_Offset;
            Owner_Field_Shift : Owner_Shift;
         when False => null;
      end case;
   end record;
   function Mapped_Pin (Pin : ACPI_Pin) return Boolean;
   function Locate (Pin : ACPI_Pin) return Location
     with Post => Locate'Result.Found = Mapped_Pin (Pin);
   type Pin_Search (Found : Boolean := False) is record
      case Found is when True => Pin : ACPI_Pin; when False => null; end case;
   end record;
   function First_At_Or_After (From : ACPI_Pin) return Pin_Search
     with Post => (if First_At_Or_After'Result.Found then
        First_At_Or_After'Result.Pin >= From and then Locate (First_At_Or_After'Result.Pin).Found);
   -- Bounded profile enumeration, geometry only: skips unmapped GPIO gaps.

   type Outcome is (Ready, Unsupported_Controller, No_GPIO_Pin,
      Community_Absent, Invalid_PADBAR, Offset_Overflow, Window_Too_Small);
   type Pad_Stride is (Two_Words, Four_Words);
   type Plan_Result (Status : Outcome := No_GPIO_Pin) is record
      case Status is
         when Ready =>
            Selected : Location (True);
            Stride : Pad_Stride;
            PADCFG0, PADCFG1, PADCFG2 : Register_Offset;
            Has_PADCFG2 : Boolean;
            End_Exclusive : Span;
         when others => null;
      end case;
   end record;
   function Plan
     (Controller_HID : String; Pin : ACPI_Pin; Revision, PADBAR : Word;
      Window_Bytes : Span) return Plan_Result;
   -- Geometry only. Window_Bytes must describe the launcher-admitted resource
   -- at Selected.Resource, not an arbitrary firmware address. No permission or
   -- native mapping is granted here. PADBAR/revision come from that same window.
   -- Unmapped HVCMOS physical pads have no GPIO pin; gaps never alias pads.
   function Interrupt_Eligible
     (Selected : Location; Owner_Value, Host_Value, Lock_Value : Word) return Boolean;
   -- Additional observed ownership/lock gate only, not a pin capability. Caller
   -- must also hold the launcher-issued exclusive pin/region/interrupt rights.
private
   type Group_Size is range 1 .. 32;
   type Register_Slot is range 0 .. 4;
   type Owner_Word_Prefix is range 0 .. 10;
   type Description is record
      GPIO_First : ACPI_Pin;
      Pad_First : Pad_Number;
      Size : Group_Size;
      In_Community : Community;
      Resource : Memory_Resource_Ordinal;
      Community_First : Pad_Number;
      Slot : Register_Slot;
      Owner_Prefix : Owner_Word_Prefix;
   end record with Dynamic_Predicate => Description.Community_First <= Description.Pad_First;
   Groups : constant array (Group_ID) of Description :=
     [GPP_B => (0, 0, 26, Community_0, 0, 0, 0, 0),
      GPP_T => (32, 26, 16, Community_0, 0, 0, 1, 4),
      GPP_A => (64, 42, 25, Community_0, 0, 0, 2, 6),
      GPP_S => (96, 67, 8, Community_1, 1, 67, 0, 0),
      GPP_I => (128, 75, 20, Community_1, 1, 67, 1, 1),
      GPP_H => (160, 95, 24, Community_1, 1, 67, 2, 4),
      GPP_D => (192, 119, 21, Community_1, 1, 67, 3, 7),
      Virtual_GPIO => (224, 140, 29, Community_1, 1, 67, 4, 10),
      GPP_C => (256, 169, 24, Community_4, 2, 169, 0, 0),
      GPP_F => (288, 193, 25, Community_4, 2, 169, 1, 3),
      -- Unmapped HVCMOS consumes one PAD_OWN word and register slot2.
      GPP_E => (320, 224, 25, Community_4, 2, 169, 3, 8),
      GPP_R => (352, 249, 8, Community_5, 3, 249, 0, 0)];
   function Mapped_Pin (Pin : ACPI_Pin) return Boolean is
     (for some Group in Group_ID =>
        Pin >= Groups (Group).GPIO_First and then
        Pin - Groups (Group).GPIO_First < ACPI_Pin (Groups (Group).Size));
end Intel_ADLN_GPIO;
