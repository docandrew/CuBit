pragma Ada_2022;
package body Intel_LPSS_I2C_Profile with SPARK_Mode is
   -- Timing arithmetic must never wrap. This bound covers the largest
   -- root-clock * timing-nanoseconds product in the supported device profile.
   type Wide is range 0 .. 1_000_000_000_000;
   Nanoseconds_Per_Second : constant Wide := 1_000_000_000;
   SDA_Fall_NS : constant Wide := 171;
   SCL_Fall_NS : constant Wide := 208;
   SDA_Hold_NS : constant Wide := 42;
   Standard_High_NS : constant Wide := 4_000;
   Standard_Low_NS : constant Wide := 4_700;
   Fast_High_NS : constant Wide := 600;
   Fast_Low_NS : constant Wide := 1_300;
   Fast_Plus_High_NS : constant Wide := 260;
   Fast_Plus_Low_NS : constant Wide := 500;
   High_Counter_Offset : constant Wide := 3;
   Low_Counter_Offset : constant Wide := 1;
   function Supported (Vendor, Device : PCI_ID) return Boolean is
     (Vendor = Intel_Vendor and then Device in Alder_Lake_N_I2C0 | Alder_Lake_N_I2C1);
   function Cycles (NS : Wide) return Wide is
     ((Wide (Root_Clock) * NS + Nanoseconds_Per_Second - 1) / Nanoseconds_Per_Second)
     with Pre => NS <= Standard_Low_NS + SCL_Fall_NS;
   function Derive (Vendor, Device : PCI_ID; Speed_Limit : Hertz) return Timing is
      Result : Timing;
      High_NS, Low_NS, High_Cycles, Low_Cycles, Period : Wide;
   begin
      if not Supported (Vendor, Device) then return Result; end if;
      if Speed_Limit = 0 or else Speed_Limit > Fast_Plus_Maximum then
         Result.Status := Invalid_Speed; return Result;
      end if;
      if Speed_Limit <= Standard_Maximum then
         High_NS := Standard_High_NS; Low_NS := Standard_Low_NS;
      elsif Speed_Limit <= Fast_Maximum then
         High_NS := Fast_High_NS; Low_NS := Fast_Low_NS;
         Result.Selected_Mode := Fast_Mode;
      else
         High_NS := Fast_Plus_High_NS; Low_NS := Fast_Plus_Low_NS;
         Result.Selected_Mode := Fast_Mode;
      end if;
      High_Cycles := Cycles (High_NS + SDA_Fall_NS);
      Low_Cycles := Cycles (Low_NS + SCL_Fall_NS);
      Period := (Wide (Root_Clock) + Wide (Speed_Limit) - 1) / Wide (Speed_Limit);
      if High_Cycles + Low_Cycles < Period then Low_Cycles := Period - High_Cycles; end if;
      pragma Assert (High_Cycles + Low_Cycles >= Period);
      if High_Cycles <= High_Counter_Offset or else Low_Cycles <= Low_Counter_Offset
        or else High_Cycles - High_Counter_Offset > Wide (Counter'Last)
        or else Low_Cycles - Low_Counter_Offset > Wide (Counter'Last)
      then Result.Status := Counter_Overflow; return Result;
      end if;
      Result.High_Count := Counter (High_Cycles - High_Counter_Offset);
      Result.Low_Count := Counter (Low_Cycles - Low_Counter_Offset);
      Result.TX_Hold := Counter (Cycles (SDA_Hold_NS));
      Result.RX_Hold := 1;
      pragma Assert ((High_Cycles + Low_Cycles) * Wide (Speed_Limit) >= Wide (Root_Clock));
      Result.Frequency_Upper_Bound := Hertz
        (1 + (Wide (Root_Clock) - 1) / (High_Cycles + Low_Cycles));
      Result.Status := Calculated;
      return Result;
   end Derive;
end Intel_LPSS_I2C_Profile;
