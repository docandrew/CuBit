pragma Ada_2022;
package body ACPI_SCI_Exclusion with SPARK_Mode is
   package R renames ACPI_SCI_Registers;
   use type Interfaces.Unsigned_64;
   Port_Count : constant := R.Port_Space;
   subtype Port_Value is Natural range 0 .. Port_Count - 1;
   -- Status/enable for both PM1 and GPE blocks, PM1 control, SMI command,
   -- and the two EC ports. No authority is inferred from these spans.
   Maximum_Spans : constant := 2 * (R.PM1_Kind'Pos (R.PM1_Kind'Last) + 1)
      + 2 * (R.GPE_Kind'Pos (R.GPE_Kind'Last) + 1) + 2 + 2;
   subtype Span_Count is Natural range 0 .. Maximum_Spans;
   subtype Span_Index is Span_Count range 1 .. Maximum_Spans;
   type Span is record First, Last : Port_Value := 0; end record;
   type Span_Array is array (Span_Index) of Span;
   function Check (Layout : R.Layout; EC_Active : Boolean;
      EC_Control, EC_Data : Interfaces.Unsigned_64) return Outcome is
      Spans : Span_Array := [others => (0, 0)];
      Count : Span_Count := 0;
      Bad : Boolean := False;
      procedure Add (First : Interfaces.Unsigned_64; Bytes : Natural) is
      begin
         if Bad then return; end if;
         if Bytes = 0 or else Bytes > Port_Count or else First >= Port_Count
           or else Interfaces.Unsigned_64 (Bytes - 1) > Port_Count - 1 - First
           or else Count = Span_Count'Last
         then Bad := True; return; end if;
         Count := Count + 1;
         Spans (Count) :=
           (Port_Value (First),
            Port_Value (First + Interfaces.Unsigned_64 (Bytes - 1)));
      end Add;
   begin
      if not Layout.Events (R.PM1a).Present then return Missing_PM1a; end if;
      for Kind in R.PM1_Kind loop
         if Layout.Events (Kind).Present then
            Add (Interfaces.Unsigned_64 (Layout.Events (Kind).Status), R.PM1_Register_Bytes);
            Add (Interfaces.Unsigned_64 (Layout.Events (Kind).Enable), R.PM1_Register_Bytes);
         end if;
      end loop;
      for Kind in R.GPE_Kind loop
         if Layout.GPEs (Kind).Present then
            Add (Interfaces.Unsigned_64 (Layout.GPEs (Kind).Status), Layout.GPEs (Kind).Registers);
            Add (Interfaces.Unsigned_64 (Layout.GPEs (Kind).Enable), Layout.GPEs (Kind).Registers);
         end if;
      end loop;
      if Layout.Control_Present then Add (Interfaces.Unsigned_64 (Layout.Control), R.PM1_Register_Bytes); end if;
      if Layout.SMI_Command_Present then Add (Interfaces.Unsigned_64 (Layout.SMI_Command), R.SMI_Command_Bytes); end if;
      if EC_Active then Add (EC_Control, 1); Add (EC_Data, 1); end if;
      if Bad then return Malformed_Block; end if;
      for Left in 1 .. Count loop
         for Right in 1 .. Left - 1 loop
            if Spans (Left).First <= Spans (Right).Last
              and then Spans (Right).First <= Spans (Left).Last
            then return Overlapping_Ports; end if;
         end loop;
      end loop;
      return Disjoint;
   end Check;
end ACPI_SCI_Exclusion;
