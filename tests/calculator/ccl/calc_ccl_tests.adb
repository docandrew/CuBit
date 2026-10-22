------------------------------------------------------------------------------
--  calc.* in a CCL session (hosted): the CCL face of the calculator core
--  gives the same answer as the core itself (docs/calculator.md).
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Sessions;
with CCL_Calc_Bindings;

procedure Calc_CCL_Tests is
   type No_Context is null record;
   Host : No_Context;
   procedure Invoke
     (Context : in out No_Context; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Calc_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);

   FUEL : constant := 1_000_000;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Installed : Boolean;
   Passed, Failed : Natural := 0;

   function Run (Source : String) return String is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Submit (Session, Source, FUEL, Grants, Host, Outcome);
      return CCL.Sessions.Result_Image (Outcome);
   end Run;

   procedure Expect (Source, Shown : String) is
      Got : constant String := Run (Source);
   begin
      if Got = Shown then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL: " & Source & ASCII.LF & "   got " & Got & ASCII.LF & "  want " & Shown);
      end if;
   end Expect;

   procedure Show (Source : String) is
   begin
      Put_Line (Source & "  =>  " & Run (Source));
   end Show;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL_Calc_Bindings.Install (Catalog, Grants, Installed);
   if not Installed then
      Put_Line ("FAIL: calc.* is not published and granted");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   CCL.Sessions.Initialize (Session, Catalog);
   Show ("(calc.evaluate ""2/3 + 1/6"")");
   Expect ("(field (calc.evaluate ""2/3 + 1/6"") numerator)", "String: 5");
   Expect ("(field (calc.evaluate ""2/3 + 1/6"") denominator)", "String: 6");
   Expect ("(field (calc.evaluate ""2/3 + 1/6"") decimal)", "String: 0.8(3)");
   Expect ("(field (calc.evaluate ""2/3 + 1/6"") exact)", "Boolean: true");
   Expect ("(field (calc.evaluate ""sqrt(2)"") exact)", "Boolean: false");
   --  An approximation: its guaranteed digits and their stated bound.
   Expect ("(field (calc.evaluate ""sqrt(2)"") decimal)", "String: 1.4142135623730950488");
   Expect ("(field (calc.evaluate ""sqrt(2)"") bound)", "String: 2e-21");
   Expect ("(field (calc.evaluate ""1/3"") bound)", "String");
   Expect ("(field (calc.evaluate ""2^64"") numerator)", "String: 18446744073709551616");
   Expect ("(field (calc.evaluate ""-7/2"") numerator)", "String: -7");
   Expect ("(field (calc.evaluate ""1e-30"") decimal)", "String: 1e-30");
   Expect ("(parse-int (field (calc.evaluate ""6 * 7"") numerator))", "Integer: 42");
   Show ("(field (calc.evaluate ""1/7"") form)");
   Show ("(calc.evaluate ""1/0"")");
   Show ("(calc.evaluate ""2 +"")");
   Show ("(calc.evaluate ""2^(10^9)"")");
   Show ("(calc.evaluate ""ans"")");
   --  A long result works inside a program; only a result shown whole
   --  is limited (CCL.VM.MAX_RESULT_TEXT).
   Expect ("(length (field (calc.evaluate ""1000!"") numerator))", "Integer: 2568");
   Expect ("(length (field (calc.evaluate ""2^(2^13)"") numerator))", "Integer: 2467");
   --  Beyond one CCL value's text: a typed refusal, not a cut number.
   Expect ("(calc.evaluate ""2^(2^15)"")",
           "calc.evaluate ran out of room: the result's text is 19731 bytes; one CCL value holds 8192. " &
           "Instead: the Calculator app shows results of any size");
   --  The command form: (calc ..) is calc.evaluate by its bare name.
   Show ("(calc ""2/3+1/6"")");
   Expect ("(field (calc ""2/3+1/6"") numerator)", "String: 5");
   Expect ("(field (calc ""2/3+1/6"") decimal)", "String: 0.8(3)");
   Expect ("(= (field (calc ""sqrt(2)"") decimal) (field (calc.evaluate ""sqrt(2)"") decimal))", "Boolean: true");
   Show ("(calc ""1/0"")");
   --  Modes: calc.evaluate-with a Calc_Request (the app's options).
   Show ("(calc.evaluate-with (Calc_Request ""sin(30)"" (Calc_Options angle => Calc_Angle.Degrees)))");
   Expect ("(field (calc.evaluate-with (Calc_Request ""sin(30)"" (Calc_Options angle => Calc_Angle.Degrees))) " &
           "numerator)", "String: 1");
   Expect ("(field (calc.evaluate-with (Calc_Request ""asin(1/2)"" (Calc_Options angle => Calc_Angle.Gradians)))" &
           " decimal)", "String: 33.(3)");
   Show ("(calc.evaluate-with (Calc_Request ""0xFF + 1"" (Calc_Options mode => Calc_Mode.Programmer " &
         "word => Calc_Word.Bits_8 signed => false)))");
   Expect ("(field (calc.evaluate-with (Calc_Request ""0xFF + 1"" (Calc_Options mode => Calc_Mode.Programmer " &
           "word => Calc_Word.Bits_8 signed => false))) wrapped)", "Boolean: true");
   Expect ("(field (calc.evaluate-with (Calc_Request ""-1"" (Calc_Options mode => Calc_Mode.Programmer " &
           "word => Calc_Word.Bits_16))) hexadecimal)", "String: FFFF");
   Expect ("(field (calc.evaluate-with (Calc_Request ""rotl(0b1001, 2)"" (Calc_Options mode => " &
           "Calc_Mode.Programmer word => Calc_Word.Bits_8 signed => false))) binary)", "String: 00100100");
   Expect ("(calc ""rotl(1, 1)"")", "calc rejected its argument: rotl and rotr need a word size (8, 16, 32 or " &
           "64 bits) (column 1). Instead: use calc.evaluate-with and Calc_Options with mode Programmer and a word size");
   Show ("(calc.evaluate-with (Calc_Request ""7/2"" (Calc_Options mode => Calc_Mode.Programmer)))");
   Put_Line ("calc.* in CCL:" & Passed'Image & " checks passed," & Failed'Image & " failed");
   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Calc_CCL_Tests;
