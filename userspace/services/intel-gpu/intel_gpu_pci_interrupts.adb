with Interfaces; use Interfaces;
package body Intel_GPU_PCI_Interrupts with SPARK_Mode is
   function Valid (Plan : Disable_Plan) return Boolean is (Plan.Ready);
   function Count (Plan : Disable_Plan) return Natural is (Plan.Used);
   function Offset (Plan : Disable_Plan; Index : Positive) return Natural is
     (Plan.Words (Index).Address);
   function Before (Plan : Disable_Plan; Index : Positive) return Unsigned_16 is
     (Plan.Words (Index).Prior);
   function After (Plan : Disable_Plan; Index : Positive) return Unsigned_16 is
     (Plan.Words (Index).Proposed);
   function Plan_Disable (Data : Intel_GPU_PCI_Power.Configuration)
     return Disable_Plan
   is
      Result : Disable_Plan;
      State : constant Snapshot := Decode (Data);
      Pointer : Natural := Natural (Data (16#34#));
      Value : Unsigned_16;
      Failed : Boolean := False;
      function Word (Address : Natural) return Unsigned_16
      is (Unsigned_16 (Data (Address)) or Shift_Left (Unsigned_16 (Data (Address + 1)), 8))
        with Pre => Address <= 254;
      procedure Add (Address : Natural; Prior, Proposed : Unsigned_16) is
      begin
         if Prior = Proposed then return; end if;
         if Address > 254 or else Address mod 2 /= 0 or else Result.Used = 3 then
            Failed := True;
            return;
         end if;
         Result.Used := Result.Used + 1;
         Result.Words (Result.Used) := (Address, Prior, Proposed);
      end Add;
   begin
      if not State.Valid or else Word (4) = Unsigned_16'Last then return Result; end if;
      Value := Word (4);
      Add (4, Value, Value or 16#400#);
      if (Data (6) and 16#10#) = 0 then
         Result.Ready := True;
         return Result;
      end if;
      for Step in 1 .. 48 loop
         if Pointer = 0 then Result.Ready := True; return Result; end if;
         -- Decode already validates the complete chain; keep local bounds too.
         if Pointer < 16#40# or else Pointer > 16#FC# or else Pointer mod 4 /= 0 then
            return (others => <>);
         end if;
         if Data (Pointer) in 5 | 16#11# then
            Value := Word (Pointer + 2);
            if Value = Unsigned_16'Last then return (others => <>); end if;
            Add (Pointer + 2, Value,
              (if Data (Pointer) = 5 then Value and not 1
               else (Value and not 16#8000#) or 16#4000#));
            if Failed then return (others => <>); end if;
         end if;
         Pointer := Natural (Data (Pointer + 1));
      end loop;
      if Pointer = 0 then Result.Ready := True; return Result; end if;
      return (others => <>);
   end Plan_Disable;
   function Encoding_Valid (Bits : Unsigned_8) return Boolean is
     ((Bits and 16#80#) = 0 and then
      (Bits = 0 or else (Bits and 1) /= 0) and then
      ((Bits and 8) = 0 or else (Bits and 4) /= 0) and then
      ((Bits and 16#60#) = 0 or else (Bits and 16#10#) /= 0));

   function Pack (Value : Snapshot) return Unsigned_8 is
   begin
      if not Value.Valid or else
        (Value.MSI_Enabled and not Value.MSI_Present) or else
        ((Value.MSIX_Enabled or Value.MSIX_Masked) and not Value.MSIX_Present)
      then return 0; end if;
      return 1 or (if Value.INTx_Disabled then 2 else 0) or
        (if Value.MSI_Present then 4 else 0) or
        (if Value.MSI_Enabled then 8 else 0) or
        (if Value.MSIX_Present then 16 else 0) or
        (if Value.MSIX_Enabled then 32 else 0) or
        (if Value.MSIX_Masked then 64 else 0);
   end Pack;

   function Unpack (Bits : Unsigned_8) return Snapshot is
     (Valid => (Bits and 1) /= 0, INTx_Disabled => (Bits and 2) /= 0,
      MSI_Present => (Bits and 4) /= 0, MSI_Enabled => (Bits and 8) /= 0,
      MSIX_Present => (Bits and 16) /= 0, MSIX_Enabled => (Bits and 32) /= 0,
      MSIX_Masked => (Bits and 64) /= 0);

   function Decode (Data : Intel_GPU_PCI_Power.Configuration) return Snapshot is
      Bad : constant Snapshot := (others => False);
      Result : Snapshot := Bad;
      Pointer : Natural := Natural (Data (16#34#));
      Used : array (Natural range 0 .. 255) of Boolean := [others => False];
      Length : Natural;
   begin
      if (Data (0) = 16#FF# and Data (1) = 16#FF#) or else
        (Data (16#0E#) and 16#7F#) /= 0
      then return Bad; end if;
      Result.INTx_Disabled := (Data (5) and 4) /= 0;
      if (Data (6) and 16#10#) = 0 then
         Result.Valid := True;
         return Result;
      end if;
      for Step in 1 .. 48 loop
         if Pointer = 0 then
            Result.Valid := True;
            return Result;
         end if;
         if Pointer < 16#40# or else Pointer > 16#FC# or else
           Pointer mod 4 /= 0
         then return Bad; end if;
         Length := 2;
         case Data (Pointer) is
            when 5 =>
               if Result.MSI_Present then return Bad; end if;
               Result.MSI_Present := True;
               Result.MSI_Enabled := (Data (Pointer + 2) and 1) /= 0;
               -- 32/64-bit MSI, optionally per-vector mask/pending DWORDs.
               Length := (if (Data (Pointer + 2) and 16#80#) /= 0 then 14 else 10);
               if (Data (Pointer + 3) and 1) /= 0 then Length := Length + 10; end if;
            when 16#11# =>
               if Result.MSIX_Present then return Bad; end if;
               Result.MSIX_Present := True;
               Result.MSIX_Enabled := (Data (Pointer + 3) and 16#80#) /= 0;
               Result.MSIX_Masked := (Data (Pointer + 3) and 16#40#) /= 0;
               Length := 12;
            when others => null;
         end case;
         if Length > 256 - Pointer then return Bad; end if;
         for Byte in Pointer .. Pointer + Length - 1 loop
            if Used (Byte) then return Bad; end if;
            Used (Byte) := True;
         end loop;
         Pointer := Natural (Data (Pointer + 1));
      end loop;
      if Pointer = 0 then Result.Valid := True; return Result; end if;
      return Bad;
   end Decode;
end Intel_GPU_PCI_Interrupts;
