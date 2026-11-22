with Interfaces;
with Firmware_Tables;
with ACPI_HID_I2C_Resources;
with HID_I2C_Descriptors;
generic
   Maximum_Span_Bytes : Positive := 256;
package ACPI_HID_I2C_Candidate with Pure, SPARK_Mode is
   package Resources renames ACPI_HID_I2C_Resources;
   package R renames Resources.R;
   package S renames Resources.S;
   package G renames Resources.G;
   subtype Span_Length is Natural range 0 .. Maximum_Span_Bytes;
   type Owned_Span is record
      Length : Span_Length := 0;
      Data : Firmware_Tables.Bytes (1 .. Maximum_Span_Bytes) := [others => 0];
   end record;
   type Candidate is record
      Descriptor_Register : HID_I2C_Descriptors.Register_Number := 0;
      I2C : S.Connection_Information (S.I2C_Bus) :=
        (S.I2C_Bus, S.Seven_Bit, 0, 0, 0);
      I2C_Revision : S.Descriptor_Revision := 1;
      I2C_Type_Revision : Firmware_Tables.Byte := 0;
      I2C_Source_Index : Firmware_Tables.Byte := 0;
      I2C_Specific_Flags : Interfaces.Unsigned_16 := 0;
      I2C_Role : S.Resource_Role := S.Consumes_Resource;
      Initiated_By : S.Initiator := S.Controller_Initiated;
      I2C_Shared : Boolean := False;
      Interrupt : G.Connection_Information (G.Interrupt_Connection) :=
        (G.Interrupt_Connection, G.Level_Triggered, G.Active_High, False);
      Interrupt_Role : G.Resource_Role := G.Consumes_Resource;
      Interrupt_Shared : Boolean := False;
      Configuration : G.Pin_Configuration := 0;
      Drive : G.Drive_Hundredths_Milliampere := 0;
      Debounce : G.Debounce_Hundredths_Millisecond := 0;
      Pin : G.Pin_Number := G.No_Pin;
      I2C_Source, I2C_Vendor, GPIO_Source, GPIO_Vendor : Owned_Span;
   end record;
   Empty_Candidate : constant Candidate := (others => <>);
   type Outcome is (Copied, Analysis_Failed, Invalid_Span, Capacity_Refused, Pin_Unavailable);
   type Span_Kind is (No_Span, I2C_Source, I2C_Vendor, GPIO_Source, GPIO_Vendor);
   type Report is record
      Status : Outcome := Analysis_Failed;
      Analysis : Resources.Analysis_Outcome := Resources.Invalid_Template;
      At_Span : Span_Kind := No_Span;
      Required : R.Byte_Count := 0;
      Value : Candidate := Empty_Candidate;
   end record;
   -- Reanalyze one exact copied _CRS prefix; never trust caller-fabricated
   -- spans or reports. Own every source/vendor byte without truncation, NUL
   -- interpretation, path resolution, bus-policy selection or authorization.
   -- Maximum_Span_Bytes is an instance budget, not an ACPI descriptor limit.
   -- Initialization/identity/_DEP and descriptor-register provenance remain
   -- the trusted discovery owner's obligations. Zero register and No_Pin are
   -- preserved observations; the controller must separately admit them.
   procedure Copy (Data : Firmware_Tables.Bytes;
      Descriptor_Register : HID_I2C_Descriptors.Register_Number;
      Result : out Report; Maximum_Items : R.Item_Count := R.Default_Item_Limit)
     with Global => null,
       Post => (if Result.Status /= Copied then Result.Value = Empty_Candidate);
end ACPI_HID_I2C_Candidate;
