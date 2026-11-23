pragma Ada_2022;
with Interfaces;
package body ACPI_CMOS_Manifest_Windows with SPARK_Mode is
   use type ST.Name_Bytes, ST.Capability_Kind, RS.Decode_Status, RS.Basis_Kind;
   use type RS.Resource_Kind, RS.Rights_Kind, DS.Decode_Status, DS.Match_Kind;
   use type RS.Platform_Device;
   use type Interfaces.Unsigned_16, Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   function Inspect
     (Declared : ST.Table; Resources : RS.Byte_Array; Devices : DS.Byte_Array)
      return Declaration
   is
      Plan : RS.Section_Plan; Matches : DS.Match_Table;
      Resource_Status : RS.Decode_Status; Device_Status : DS.Decode_Status;
      Binding : ST.Entry_Count := 0;
      Resource_Total : Natural := 0;
      Has_Hardware : Boolean := False;
   begin
      for I in 1 .. Declared.Count loop
         pragma Loop_Invariant (Resource_Total <= I - 1);
         pragma Loop_Invariant (Binding <= I - 1);
         if Declared.Entries (I).Kind = ST.ACPI_Hardware then Has_Hardware := True; end if;
         if Declared.Entries (I).Kind in ST.Device_Memory | ST.IO_Ports | ST.Interrupt | ST.DMA | ST.Scheduling then
            Resource_Total := Resource_Total + 1;
            if Declared.Entries (I).Kind = ST.IO_Ports
              and then Declared.Entries (I).Label.Length = 4
              and then Declared.Entries (I).Label.Text (1 .. 4) = "cmos"
            then Binding := I; end if;
         end if;
      end loop;
      if Binding = 0 then return (others => <>); end if;
      if not Has_Hardware or else Resource_Total /= 1
        or else Resources'First /= 1 or else Resources'Length > RS.MAX_SECTION_BYTES
        or else Devices'First /= 1 or else Devices'Length > DS.MAX_SECTION_BYTES
      then return (Status => Malformed_Declaration, others => <>); end if;
      RS.Decode (Resources, Plan, Resource_Status);
      DS.Decode (Devices, Matches, Device_Status);
      if Resource_Status /= RS.Decoded or else Device_Status /= DS.Decoded
        or else Plan.Count /= 1 or else Plan.Basis.Kind /= RS.Platform_Basis
        or else Plan.Basis.Values (1) /= RS.Platform_Device'Enum_Rep (RS.CMOS_RTC)
        or else Matches.Count /= 1 or else Matches.Entries (1).Kind /= DS.Platform
        or else Matches.Entries (1).Device /= RS.CMOS_RTC
        or else Plan.Entries (1).Kind /= RS.IO_Ports
        or else Plan.Entries (1).Rights /= RS.Read_Write
        or else Plan.Entries (1).Index /= 0 or else Plan.Entries (1).Amount not in 2 | 4
        or else Plan.Entries (1).Extra /= 0
      then return (Status => Malformed_Declaration, others => <>); end if;
      return (Status => Admitted, Binding => Binding,
              Maximum_Ports => Natural (Plan.Entries (1).Amount));
   end Inspect;
   function Select_Window
     (Item : Declaration; Approved, Receipt_Known, Recipient_Available : Boolean;
      Receipt : Wire.Receipt) return Selection
   is
      Ports : constant Natural := Natural (Wire.Port_Count (Receipt.Device));
   begin
      if Item.Status /= Admitted then return (Status => Item.Status, others => <>); end if;
      if Item.Binding = 0 or else Item.Maximum_Ports not in 2 | 4 then
         return (Status => Malformed_Declaration, others => <>);
      end if;
      if not Approved then return (Status => Not_Approved, others => <>); end if;
      if not Receipt_Known or else Receipt.Device = Wire.Unidentified or else Ports not in 2 | 4
        or else Ports > Item.Maximum_Ports
      then return (Status => Receipt_Unavailable, others => <>); end if;
      if not Recipient_Available then return (Status => Recipient_Busy, others => <>); end if;
      return (Status => Admitted, Kind => System_CMOS_Window, Binding => Item.Binding, Ports => Ports);
   end Select_Window;
end ACPI_CMOS_Manifest_Windows;
