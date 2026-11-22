--  Golden wire vectors for the Observatory: real evaluations (with the
--  image interface installed, as ccl-control installs it) encoded by
--  Control_Presentation, written as JSON. run.sh compares the output with
--  userspace/ccl/tools/ccl-observatory/wire-vectors.json, which
--  wire.test.mjs decodes with the browser's decoder, so the Ada encoder
--  and the JS decoder cannot drift apart.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CBOR;
with CCL.Catalog;
with CCL.Completions;
with CCL.Control;
with CCL.Host_Values;
with CCL.Image_Store;
with CCL.Language;
with CCL.Presentations;
with CCL.Sessions;
with CCL_Image_Bindings;
with Control_Presentation;
with Control_Wire;

procedure Wire_Vectors is
   use type CCL.Control.Operation;
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Installed : Boolean;
   type No_Context is null record;
   Host : No_Context;
   procedure Invoke
     (Context : in out No_Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
   begin
      CCL_Image_Bindings.Invoke (Binding, Argument, Reply);
   end Invoke;
   procedure Submit is new CCL.Sessions.Submit_With_Values (No_Context, Invoke);
   Session : CCL.Sessions.Session;
   First : Boolean := True;
   Last_Image : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;

   function Hex (Data : Control_Wire.Response) return String is
      Digits_Of : constant String := "0123456789abcdef";
      Result : String (1 .. 2 * Data.Length);
   begin
      for I in 1 .. Data.Length loop
         Result (2 * I - 1) := Digits_Of (Natural (Data.Data (CBOR.SE_Offset (I))) / 16 + 1);
         Result (2 * I) := Digits_Of (Natural (Data.Data (CBOR.SE_Offset (I))) mod 16 + 1);
      end loop;
      return Result;
   end Hex;
   function Escaped (Text : String) return String is
      Result : String (1 .. 2 * Text'Length);
      Last : Natural := 0;
   begin
      for C of Text loop
         if C in '"' | '\' then Last := Last + 1; Result (Last) := '\'; end if;
         Last := Last + 1; Result (Last) := C;
      end loop;
      return Result (1 .. Last);
   end Escaped;
   procedure Emit (Name : String; Query : Control_Wire.Request; Value : CCL.Control.Response) is
      Data : Control_Wire.Response;
   begin
      Control_Presentation.Encode (Query, Value, Data);
      if Data.Length = 0 then raise Program_Error with "not encoded: " & Name; end if;
      Put_Line ((if First then "  " else ", ") & "{""name"": """ & Escaped (Name) & """, ""op"": " &
                (case Query.Op is
                    when CCL.Control.Present_Expression => """present""",
                    when CCL.Control.Present_Monitor => """presentMonitor""",
                    when CCL.Control.Complete_Expression => """complete""",
                    when others => """imageRows""") &
                ", ""id"": " & Unsigned_64'Image (Query.Id) & ", ""hex"": """ & Hex (Data) & """}");
      First := False;
   end Emit;
   procedure Present (Name, Source : String; Id : Unsigned_64) is
      Query : Control_Wire.Request := (Id => Id, Op => CCL.Control.Present_Expression, others => <>);
      Value : CCL.Control.Response;
      Shown : CCL.Presentations.Presentation;
      use type CCL.Presentations.Form;
   begin
      Query.Length := Source'Length;
      Query.Source (1 .. Source'Length) := Source;
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Host, Value.Outcome);
      CCL.Presentations.Describe (Value.Outcome, Shown);
      if Shown.Kind = CCL.Presentations.Picture then Last_Image := Shown.Image; end if;
      Emit (Name, Query, Value);
   end Present;
   procedure Rows (Name : String; Image : CCL.Image_Store.Image_Id; First_Row : Unsigned_64; Id : Unsigned_64) is
      Value : CCL.Control.Response;
   begin
      Emit (Name, (Id => Id, Op => CCL.Control.Read_Image_Rows,
                   Target => Unsigned_64 (Image), Row => First_Row, others => <>), Value);
   end Rows;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Catalog.Initialize (Grants);
   CCL_Image_Bindings.Install (Catalog, Grants, Installed);
   if not Installed then raise Program_Error with "image interface"; end if;
   CCL.Sessions.Initialize (Session, Catalog);
   Put_Line ("[");
   Present ("integer", "(+ 20 22)", 1);
   Present ("failure", "(+ 1 ""two"")", 2);
   Present ("string", "(concat ""a"" ""b"")", 3);
   Present ("type", "(type Event (record (time Integer) (level String)))", 4);
   Present ("table", "(list (Event 1200 ""info"") (Event 1385 ""warn""))", 5);
   Present ("record", "(Event 7 ""debug"")", 6);
   Present ("picture", "(image.gradient (Size 4 3))", 7);
   Rows ("rows", Last_Image, 0, 8);
   Rows ("rows-from", Last_Image, 2, 9);
   Rows ("expired", 12345, 0, 10);
   Present ("plot", "(image.plot (list 1 2))", 11);
   declare
      Idle : CCL.Control.Response;
   begin
      --  The periodic slot before anything runs: its (empty) last result.
      Emit ("monitor-idle", (Id => 15, Op => CCL.Control.Present_Monitor, others => <>), Idle);
   end;
   declare
      procedure Complete (Name, Before : String; Id : Unsigned_64) is
         Value : CCL.Control.Response;
         Query : Control_Wire.Request := (Id => Id, Op => CCL.Control.Complete_Expression, others => <>);
      begin
         Query.Length := Before'Length;
         Query.Source (1 .. Before'Length) := Before;
         CCL.Completions.Complete (Catalog, Before, ' ', Value.Completion);
         Emit (Name, Query, Value);
      end Complete;
   begin
      Complete ("complete-names", "(image.p", 16);
      Complete ("complete-signature", "(image.plot ", 17);
      Complete ("complete-words", "(so", 18);
   end;
   Present ("gallery", "(list (image.gradient (Size 4 3)) (image.plot (list 1 2)))", 14);
   --  A digest of all 19 digits must survive the presentation.
   Present ("large-id", "(image.plot (list 3 1 4 1 5 9 2 6))", 13);
   Rows ("plot-band", Last_Image, 0, 12);
   Put_Line ("]");
end Wire_Vectors;
