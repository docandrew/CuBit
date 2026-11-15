pragma Ada_2022;
with ACPI_HID_I2C_Channels;
package body ACPI_HID_Offer_Builder with SPARK_Mode is
   package Channels is new ACPI_HID_I2C_Channels (Candidates);
   package S renames Candidates.S;
   package G renames Candidates.G;
   use type Channels.Outcome;
   procedure Build (Facts : Candidates.Candidate; Provenance : Context;
      Result : out Report) is
      Prepared : constant Channels.Preparation := Channels.Prepare (Facts);
      Value : Wire.Request := Wire.Empty_Request;
      function Fits (Span : Candidates.Owned_Span) return Boolean is
        (Span.Length <= Wire.Max_Span_Bytes);
      procedure Copy (From : Candidates.Owned_Span; Into : out Wire.Owned_Span)
        with Pre => Fits (From) is
      begin
         Into := (others => <>);
         Into.Length := Wire.Span_Length (From.Length);
         for I in 1 .. Into.Length loop
            Into.Data (I) := Wire.Byte (From.Data (I));
         end loop;
      end Copy;
   begin
      Result := (others => <>);
      if Prepared.Status /= Channels.Prepared then return; end if;
      if not Fits (Facts.I2C_Source) or else not Fits (Facts.I2C_Vendor)
        or else not Fits (Facts.GPIO_Source) or else not Fits (Facts.GPIO_Vendor)
      then Result.Status := Span_Too_Long; return; end if;
      Value.Epoch := Provenance.Epoch;
      Value.Catalog_Revision := Provenance.Catalog_Revision;
      Value.Observation_Sequence := Provenance.Observation_Sequence;
      Value.PCI := Provenance.PCI;
      Value.GPIO_UID_Kind := Provenance.GPIO_UID_Kind;
      Value.GPIO_UID := Provenance.GPIO_UID;
      Value.GPIO_HID := Provenance.GPIO_HID;
      Value.Table := Provenance.Table;
      Value.Table_ID := Provenance.Table_ID;
      Value.Table_Digest := Provenance.Table_Digest;
      Value.Initializer_Count := Provenance.Initializer_Count;
      Value.Initializer_Digest := Provenance.Initializer_Digest;
      Value.Resource_Digest := Provenance.Resource_Digest;
      Value.Device_Path := Provenance.Device_Path;
      Value.I2C_Path := Provenance.I2C_Path;
      Value.GPIO_Path := Provenance.GPIO_Path;
      Value.Maximum_Hz := Wire.Small_Word (Facts.I2C.Connection_Speed);
      Value.Address := Wire.Half (Facts.I2C.Address);
      Value.Descriptor_Register := Wire.Half (Facts.Descriptor_Register);
      Value.Pin := Wire.Half (Facts.Pin);
      Value.I2C_Revision := Wire.Byte (Facts.I2C_Revision);
      Value.I2C_Type_Revision := Wire.Byte (Facts.I2C_Type_Revision);
      Value.I2C_Source_Index := Wire.Byte (Facts.I2C_Source_Index);
      Value.I2C_Specific_Flags := Wire.Half (Facts.I2C_Specific_Flags);
      Value.I2C_Role := Wire.Consumer;
      Value.Initiated_By := Wire.Controller_Initiated;
      Value.I2C_Shared := Facts.I2C_Shared;
      Value.Addressing := (case Facts.I2C.Addressing is
         when S.Seven_Bit => Wire.Seven_Bit, when S.Ten_Bit => Wire.Ten_Bit);
      Value.IRQ_Trigger := (case Facts.Interrupt.Mode is
         when G.Level_Triggered => Wire.Level, when G.Edge_Triggered => Wire.Edge);
      Value.IRQ_Polarity := (case Facts.Interrupt.Polarity is
         when G.Active_High => Wire.Active_High,
         when G.Active_Low => Wire.Active_Low,
         when G.Active_Both => Wire.Active_Both);
      Value.IRQ_Shared := Facts.Interrupt_Shared;
      Value.Wake_Capable := Facts.Interrupt.Wake_Capable;
      Value.GPIO_Role := Wire.Consumer;
      Value.Pin_Configuration := Wire.Byte (Facts.Configuration);
      Value.Drive := Wire.Half (Facts.Drive);
      Value.Debounce := Wire.Half (Facts.Debounce);
      Copy (Facts.I2C_Source, Value.I2C_Source);
      Copy (Facts.I2C_Vendor, Value.I2C_Vendor);
      Copy (Facts.GPIO_Source, Value.GPIO_Source);
      Copy (Facts.GPIO_Vendor, Value.GPIO_Vendor);
      if not Wire.Valid (Value) then Result.Status := Invalid_Description; return; end if;
      Result := (Status => Built, Value => Value);
   end Build;
end ACPI_HID_Offer_Builder;
