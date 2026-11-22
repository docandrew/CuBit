pragma Ada_2022;
with Interfaces;
package Intel_LPSS_I2C_Profile with SPARK_Mode is
   type PCI_ID is new Interfaces.Unsigned_16;
   Intel_Vendor : constant PCI_ID := 16#8086#;
   Alder_Lake_N_I2C0 : constant PCI_ID := 16#54E8#;
   Alder_Lake_N_I2C1 : constant PCI_ID := 16#54E9#;
   type Hertz is range 0 .. 1_000_000_000;
   Root_Clock : constant Hertz := 133_000_000;
   Standard_Maximum : constant Hertz := 100_000;
   Fast_Maximum : constant Hertz := 400_000;
   Fast_Plus_Maximum : constant Hertz := 1_000_000;
   type Counter is range 0 .. 65_535;
   type Mode is (Standard_Mode, Fast_Mode);
   type Outcome is (Calculated, Unknown_Controller, Invalid_Speed, Counter_Overflow);
   type Timing is record
      Status : Outcome := Unknown_Controller;
      Selected_Mode : Mode := Standard_Mode;
      High_Count, Low_Count, TX_Hold : Counter := 0;
      RX_Hold : Counter := 0;
      Frequency_Upper_Bound : Hertz := 0;
   end record;
   function Supported (Vendor, Device : PCI_ID) return Boolean;
   function Derive (Vendor, Device : PCI_ID; Speed_Limit : Hertz) return Timing
     with Post =>
       (if Derive'Result.Status = Calculated then
          Supported (Vendor, Device) and then Speed_Limit > 0
          and then Derive'Result.High_Count > 0
          and then Derive'Result.Low_Count > 0
          and then Derive'Result.Frequency_Upper_Bound <= Speed_Limit);
   -- Conservative profile: round timing minima upwards and extend the low
   -- period so the modeled clock period never exceeds the _CRS speed ceiling.
   -- Actual electrical timing/counter interpretation must be hardware-validated.
end Intel_LPSS_I2C_Profile;
