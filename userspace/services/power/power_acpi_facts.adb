pragma Ada_2022;
package body Power_ACPI_Facts with SPARK_Mode is

   --  ACPI_Cached_Battery_Values.Acquisition_Reason has twelve members.
   Last_Reason : constant Word := 11;
   --  ACPI_Cached_Battery_Values.Information_Source has three members.
   Last_Information_Source : constant Word := 2;
   --  _BST's reserved state bits are a DWORD's.
   Last_Reserved_Bits : constant Word := Word (Unsigned_32'Last);
   --  The source summary's state codes.
   Unknown_Code : constant Word := 0;
   Offline_Code : constant Word := 1;
   Online_Code  : constant Word := 2;

   procedure Decode_Source (Data : Words; Facts : out AC_Facts; Status : out Availability; Valid : out Boolean) is
   begin
      Facts := No_AC;
      Status := Not_Ready;
      Valid := Valid_Availability (Data (1)) and then Data (2) <= Online_Code and then Data (3) <= 1;
      if not Valid then
         return;
      end if;
      Status := To_Availability (Data (1));
      --  Unavailable samples carry no state.
      if Status /= Available and then Data (2) /= Unknown_Code then
         Valid := False;
         return;
      end if;
      Facts.State := (case Data (2) is
                        when Offline_Code => Offline,
                        when Online_Code  => Online,
                        when others       => Unknown);
   end Decode_Source;

   procedure Decode_Inventory
     (Data : Words; Generation : out Word; Status : out Availability; Count : out Battery_Count;
      Valid : out Boolean) is
   begin
      Generation := Data (0);
      Status := Not_Ready;
      Count := 0;
      Valid := Valid_Availability (Data (1)) and then Data (2) <= Maximum_Batteries and then Data (3) = 0;
      if not Valid then
         return;
      end if;
      Status := To_Availability (Data (1));
      if Status = Available then
         Count := Battery_Count (Data (2));
      elsif Data (2) /= 0 then
         --  An unavailable inventory reveals no slots.
         Valid := False;
      end if;
   end Decode_Inventory;

   procedure Decode_Entry
     (Page : Battery_Subpage; Data : Words; Facts : in out Battery_Facts; Valid : out Boolean)
   is
      Status : Availability;
   begin
      Valid := Valid_Availability (Data (1));
      if not Valid then
         return;
      end if;
      Status := To_Availability (Data (1));
      if Page = Entry_Summary then
         Valid := Status = Available and then Data (2) <= Entry_Status'Pos (Entry_Status'Last)
           and then Data (3) <= Last_Reason;
         if Valid then
            Facts.Status := Entry_Status'Val (Data (2));
         end if;
         return;
      end if;
      --  An absent or failed entry's pages carry no payload.
      if Status /= Available then
         Valid := Status in Missing | Failure and then Data (2) = 0 and then Data (3) = 0;
         return;
      end if;
      case Page is
         when Information =>
            Valid := Data (2) <= Last_Information_Source and then Data (3) <= Unit_Kind'Pos (Unit_Kind'Last);
            if Valid then
               Facts.Units := Unit_Kind'Val (Data (3));
            end if;
         when Capacities =>
            Valid := Valid_Quantity (Data (2)) and then Valid_Quantity (Data (3));
            if Valid then
               Facts.Design := To_Quantity (Data (2));
               Facts.Last_Full := To_Quantity (Data (3));
            end if;
         when Design_Voltage =>
            Valid := Valid_Quantity (Data (2)) and then Data (3) = 0;
            if Valid then
               Facts.Design_Voltage := To_Quantity (Data (2));
            end if;
         when Dynamic_State =>
            Valid := Data (2) <= Direction'Pos (Direction'Last) and then Data (3) <= Known_Flags;
            if Valid then
               Facts.Dynamic_Known := True;
               Facts.Flow := Direction'Val (Data (2));
               Facts.Critical := (Data (3) and Critical_Flag) /= 0;
               Facts.Charge_Limiting := (Data (3) and Charge_Limiting_Flag) /= 0;
            end if;
         when Rate_And_Capacity =>
            Valid := Valid_Quantity (Data (2)) and then Valid_Quantity (Data (3));
            if Valid then
               Facts.Rate := To_Quantity (Data (2));
               Facts.Remaining := To_Quantity (Data (3));
            end if;
         when Dynamic_Voltage =>
            Valid := Valid_Quantity (Data (2)) and then Data (3) <= Last_Reserved_Bits;
            if Valid then
               Facts.Voltage := To_Quantity (Data (2));
            end if;
         when Inventory | Sample_Time | Entry_Summary =>
            Valid := False;
      end case;
   end Decode_Entry;
end Power_ACPI_Facts;
