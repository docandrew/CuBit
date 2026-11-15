pragma Ada_2022;
with System.Machine_Code;
with ACPI_Native_Log;
package body ACPI_Native_Field_Profile is
   use type Tick, AML_Field_Notes.Boundary;
   type Counters is record Count, Total, Worst : Tick := 0; end record;
   Rows : array (Span_Kind) of Counters;
   Began, Ended : Tick := 0;
   Active, Started, Reported, Saturated, Partial : Boolean := False;
   -- Diagnostic storage ceiling only: overflow degrades this observation,
   -- without changing the interpreter's own nesting/work limits.
   Maximum_Profile_Nesting : constant Positive := 64;
   subtype Depth_Count is Natural range 0 .. Maximum_Profile_Nesting;
   Depth : Depth_Count := 0;
   Resolution : array (Positive range 1 .. Maximum_Profile_Nesting) of Tick := [others => No_Tick];
   function Counter return Tick is
      Low, High : Interfaces.Unsigned_32;
      use System.Machine_Code;
   begin
      Asm ("lfence; rdtsc; lfence",
         Outputs => [Interfaces.Unsigned_32'Asm_Output ("=a", Low),
                     Interfaces.Unsigned_32'Asm_Output ("=d", High)],
         Clobber => "memory", Volatile => True);
      return Interfaces.Shift_Left (Tick (High), 32) or Tick (Low);
   end Counter;
   procedure Add (Value : in out Tick; Amount : Tick) is
   begin
      if Amount > Tick'Last - Value then Value := Tick'Last; Saturated := True;
      else Value := Value + Amount; end if;
   end Add;
   procedure Start is
   begin
      if Started then return; end if;
      Started := True; Began := Counter; Active := True;
   end Start;
   procedure Stop is
   begin
      if not Active then return; end if;
      Ended := Counter; Active := False;
      if Ended < Began or else Depth /= 0 then Partial := True; end if;
   end Stop;
   function Stamp return Tick is (if Active then Counter else No_Tick);
   procedure Finish (Kind : Span_Kind; Since : Tick) is
      Now, Elapsed : Tick;
   begin
      if not Active or else Since = No_Tick then return; end if;
      Now := Counter;
      if Now < Since then Partial := True; return; end if;
      Elapsed := Now - Since;
      Add (Rows (Kind).Count, 1); Add (Rows (Kind).Total, Elapsed);
      if Elapsed > Rows (Kind).Worst then Rows (Kind).Worst := Elapsed; end if;
   end Finish;
   procedure Note (Item : AML_Field_Notes.Event) is
   begin
      if not Active or else Partial then return; end if;
      if Item.At_Point = AML_Field_Notes.Resolving then
         if Depth = Depth_Count'Last then Partial := True; return; end if;
         Depth := Depth + 1; Resolution (Depth) := Stamp;
      elsif Item.At_Point = AML_Field_Notes.Resolution_Finished then
         if Depth = 0 then Partial := True; return; end if;
         Finish (Field_Resolution, Resolution (Depth)); Depth := Depth - 1;
      end if;
   end Note;
   procedure Report is
   begin
      if not Started or else Active or else Reported then return; end if;
      Reported := True;
      ACPI_Native_Log.Copied ("acpi: stage module-field-timing ok basis=TSC_WALL ticks=" &
         Tick'Image (if Ended >= Began then Ended - Began else 0) &
         " partial=" & Partial'Image & " saturated=" & Saturated'Image & " spans=INCLUSIVE", False, True);
      for Kind in Span_Kind loop
         ACPI_Native_Log.Copied ("acpi: stage module-field-span ok kind=" & Kind'Image &
            " n=" & Rows (Kind).Count'Image & " ticks=" & Rows (Kind).Total'Image &
            " max_ticks=" & Rows (Kind).Worst'Image, False, True);
      end loop;
   end Report;
end ACPI_Native_Field_Profile;
