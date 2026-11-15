--  Hosted tests of Start_Child's pure parts (SUP-001):
--    * CuBit.Child_Specs: the spec round-trips; every truncation, every
--      broken field and every rule of Valid is refused with its status;
--    * Child_Parameters: the class block's policy plus the supervisor's
--      facts, checked by CCL's type checker against hid-i2c's own
--      HID_Parameters (read from its manifest) as procmgr checks it;
--      neither side may set the other's fields;
--    * CCL.Configurations: (driver-class ...) blocks, and the laptop
--      plans' PNP0C50 block (both twins) checks with a descriptor fact;
--    * Supervision_Edges: one supervisor per process, ranks, removal,
--      reaping marks, and the restart decision. Contracts are checked
--      (-gnata).
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with CCL.Configurations;
with CCL.Typed_Values;
with CuBit.Child_Specs;
with Child_Parameters;
with Class_Launch_Checks;
with Supervision_Edges;

procedure Supervision_Tests is
   package CS renames CuBit.Child_Specs;
   package CP renames Child_Parameters;
   package SE renames Supervision_Edges;
   use type CS.Decode_Status, CS.Restart_Kind, CS.Start_Mode, CS.Release_Decision, CS.Release_Refusal, CS.Parameter_Source, CS.Spec, CS.Refusal;
   use type CP.Merge_Status, CP.Schema_Status, CP.Read_Status, SE.Restart_Decision;
   use type CCL.Configurations.Diagnostic_Code;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Read_File (Path : String) return String is
      File : File_Type;
      Text : String (1 .. 65_536);
      Length : Natural := 0;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Text (Length + 1 .. Length + Line'Length) := Line;
            Length := Length + Line'Length + 1;
            Text (Length) := ASCII.LF;
         end;
      end loop;
      Close (File);
      return Text (1 .. Length);
   end Read_File;

   --  hid-i2c's parameter types: its manifest's (type ...) forms, before
   --  the Executable_Manifest (comment lines dropped).
   function HID_Schema return String is
      Manifest : constant String := Read_File ("../../userspace/services/hid-i2c/manifest.ccl");
      Stop : constant Natural := Ada.Strings.Fixed.Index (Manifest, "(Executable_Manifest");
      Result : String (1 .. Stop);
      Length : Natural := 0;
      Line_Start : Positive := Manifest'First;
   begin
      for K in Manifest'First .. Stop - 1 loop
         if Manifest (K) = ASCII.LF then
            if Manifest (Line_Start) /= '#' then
               Result (Length + 1 .. Length + K - Line_Start + 1) := Manifest (Line_Start .. K);
               Length := Length + K - Line_Start + 1;
            end if;
            Line_Start := K + 1;
         end if;
      end loop;
      return Result (1 .. Length);
   end HID_Schema;
   Schema : constant String := HID_Schema;

   function Field (Text : String) return CS.Field_Text is
      Result : CS.Field_Text;
   begin
      Result.Length := Text'Length;
      Result.Data (1 .. Text'Length) := Text;
      return Result;
   end Field;

   ---------------------------------------------------------------------------
   --  Specs.
   ---------------------------------------------------------------------------
   function Base_Spec return CS.Spec is
      S : CS.Spec;
   begin
      S.Restart := CS.Permanent;
      S.Intensity := 3;
      S.Window := 5_000;
      S.Priority := 5;
      S.Stop_Grace := 2_000;
      S.Name_Bytes := 11;
      S.Name (1 .. 11) := "hid-i2c.drv";
      S.Source := CS.Device_Class;
      S.Value_Bytes := 7;
      S.Value (1 .. 7) := "PNP0C50";
      S.Facts := 2;
      S.Fact_List (1) := (Field => Field ("descriptor_register"), Value => 32);
      S.Fact_List (2) := (Field => Field ("offset"), Value => -9_223_372_036_854_775_807 - 1);
      S.Bindings := 2;
      S.Binding_List (1) := (Label => Field ("i2c"), Badge => 16#4932_1015_0006_1A80#, Provider => 16#2_0000_0031#);
      S.Binding_List (2) := (Label => Field ("gpio"), Badge => 16#4750_1000_0001_004C#, Provider => CS.No_Provider);
      S.Start := CS.Held;
      S.Sink := CS.Write_Only_Sink;
      S.Sink_Badge := 16#4750_1000_0001_00D1#;
      S.Dependencies := 1;
      S.Dependency_List (1) := 16#1_0000_0042#;
      return S;
   end Base_Spec;

   Bytes : CS.Byte_Array (1 .. CS.Maximum_Spec_Bytes);
   Length : CS.Spec_Length;

   function Decode (Data : CS.Byte_Array) return CS.Decode_Status is
      S : CS.Spec;
      Status : CS.Decode_Status;
   begin
      CS.Decode (Data, S, Status);
      return Status;
   end Decode;

   procedure Spec_Tests is
      S : constant CS.Spec := Base_Spec;
      Back : CS.Spec;
      Status : CS.Decode_Status;

      --  The encoded base spec with byte At set to Value.
      function Changed (At_Byte : Positive; Value : Unsigned_8) return CS.Byte_Array is
         Copy : CS.Byte_Array := Bytes (1 .. Length);
      begin
         Copy (At_Byte) := Value;
         return Copy;
      end Changed;

      function Encoded (Item : CS.Spec) return CS.Byte_Array is
         B : CS.Byte_Array (1 .. CS.Maximum_Spec_Bytes);
         L : CS.Spec_Length;
      begin
         CS.Encode (Item, B, L);
         return B (1 .. L);
      end Encoded;
   begin
      Check (CS.Valid (S), "the base spec is valid");
      CS.Encode (S, Bytes, Length);
      Check (Length = CS.Encoded_Length (S), "Encoded_Length is the encoding's length");
      CS.Decode (Bytes (1 .. Length), Back, Status);
      Check (Status = CS.Decoded and then Back = S, "a spec round-trips");
      Check (Back.Fact_List (2).Value = Integer_64'First, "Integer_64'First survives");
      for Cut in 0 .. Length - 1 loop
         Check (Decode (Bytes (1 .. Cut)) /= CS.Decoded, "truncated at" & Cut'Image);
      end loop;
      declare
         Longer : CS.Byte_Array := Bytes (1 .. Length + 1);
      begin
         Longer (Length + 1) := 0;
         Check (Decode (Longer) = CS.Bad_Length, "a trailing byte is BAD_LENGTH");
      end;
      Check (Decode (Changed (1, 0)) = CS.Bad_Magic, "magic");
      Check (Decode (Changed (5, 1)) = CS.Bad_Version, "version 1 (the retired layout)");
      Check (Decode (Changed (7, 4)) = CS.Bad_Enumeration, "restart kind 4");
      Check (Decode (Changed (8, 0)) = CS.Bad_Enumeration, "child kind 0");
      Check (Decode (Changed (9, 2)) = CS.Bad_Enumeration, "supervisor choice 2");
      Check (Decode (Changed (10, 0)) = CS.Bad_Number, "priority 0");
      Check (Decode (Changed (10, 11)) = CS.Bad_Number, "priority 11");
      Check (Decode (Changed (11, 3)) = CS.Bad_Enumeration, "parameter source 3");
      Check (Decode (Changed (12, CS.Maximum_Facts + 1)) = CS.Bad_Count, "fact count past its ceiling");
      Check (Decode (Changed (15, 3)) = CS.Bad_Enumeration, "start mode 3");
      Check (Decode (Changed (15, 1)) = CS.Decoded, "start mode 1 (at once)");
      Check (Decode (Changed (16, 2)) = CS.Bad_Enumeration, "sink kind 2");
      Check (Decode (Changed (16, 0)) = CS.Bad_Text, "a sink badge without a sink");
      declare
         No_Badge : CS.Byte_Array := Bytes (1 .. Length);
      begin
         No_Badge (33 .. 40) := [others => 0];
         Check (Decode (No_Badge) = CS.Bad_Text, "a sink without a badge");
      end;
      --  Byte 22: the root stop's stage (PWR-003); 0 (Services) when unnamed.
      Check (Decode (Changed (23, 3)) = CS.Bad_Enumeration, "an unknown stop stage");
      Check (Decode (Changed (23, 1)) = CS.Decoded, "stop stage 1 (apps)");
      Check (Decode (Changed (24, 1)) = CS.Bad_Reserved, "reserved byte 23");
      declare
         Tail : CS.Spec := S;
         Again : CS.Spec;
         Tail_Status : CS.Decode_Status;
         use type CS.Stop_Stage, CS.Sink_Kind;
      begin
         Check (S.Stage = CS.Services, "a spec that names no stage is a service");
         Check (Bytes (23) = 0, "Services encodes as 0 at byte 22");
         Tail.Stage := CS.Power_Tail;
         CS.Decode (Encoded (Tail), Again, Tail_Status);
         Check (Tail_Status = CS.Decoded and then Again.Stage = CS.Power_Tail
                and then Again.Start = S.Start and then Again.Sink = S.Sink,
                "the Power_Tail stage round-trips beside the start mode and sink");
      end;
      Check (Decode (Changed (17, 0) (1 .. Length)) = CS.Decoded, "grace 2000 with its low byte 0 is 1792");
      declare
         Zero_Grace : CS.Byte_Array := Bytes (1 .. Length);
      begin
         Zero_Grace (17 .. 20) := [others => 0];
         Check (Decode (Zero_Grace) = CS.Bad_Number, "a zero stop grace");
      end;
      Check (Decode (Changed (41, 0)) = CS.Bad_Text, "a NUL in the program name");
      Check (Decode (Changed (41 + 11, Character'Pos ('p'))) = CS.Bad_Text, "a lower-case class");
      Check (Back.Binding_List (1).Provider = 16#2_0000_0031# and then Back.Sink_Badge = 16#4750_1000_0001_00D1#
             and then Back.Start = CS.Held, "providers, the sink and the start mode round-trip");

      --  The rules of Valid, each broken once.
      declare
         T : CS.Spec := S;
      begin
         T.Restart := CS.Temporary;
         Check (not CS.Valid (T), "temporary with an intensity");
         T.Intensity := 0;
         T.Window := 0;
         Check (CS.Valid (T), "temporary without one");
         CS.Decode (Encoded (T), Back, Status);
         Check (Status = CS.Decoded and then Back = T, "a temporary spec round-trips");
         T := S;
         T.Window := 0;
         Check (not CS.Valid (T), "permanent without a window");
         T := S;
         T.Fact_List (2).Field := Field ("descriptor_register");
         Check (not CS.Valid (T), "a fact given twice");
         T := S;
         T.Fact_List (1).Field := Field ("9lives");
         Check (not CS.Valid (T), "a fact that is no field name");
         T := S;
         T.Binding_List (2).Badge := 0;
         Check (not CS.Valid (T), "a zero badge");
         T := S;
         T.Binding_List (2).Label := Field ("i2c");
         Check (not CS.Valid (T), "a label bound twice");
         T := S;
         T.Dependency_List (1) := 0;
         Check (not CS.Valid (T), "a zero dependency");
         T := S;
         T.Source := CS.Given;
         Check (not CS.Valid (T), "facts with a given value");
         T.Facts := 0;
         Check (CS.Valid (T), "a given value");
         T.Value (3) := ASCII.NUL;
         Check (not CS.Valid (T), "a NUL in a given value");
         T := S;
         T.Source := CS.No_Parameters;
         T.Facts := 0;
         Check (not CS.Valid (T), "no parameters but a value");
         T.Value_Bytes := 0;
         Check (CS.Valid (T), "no parameters");
      end;
      Check (CS.Is_Refusal (13) and then CS.Refusal_Of (13) = CS.Sink_Not_Granted
             and then not CS.Is_Refusal (0) and then not CS.Is_Refusal (14), "refusal words");
      Check (CS.Is_Decision (1) and then CS.Decision_Of (2) = CS.Withdraw and then not CS.Is_Decision (3)
             and then CS.Is_Release_Refusal (4) and then CS.Release_Refusal_Of (3) = CS.Not_Held
             and then not CS.Is_Release_Refusal (5), "release words");
   end Spec_Tests;

   ---------------------------------------------------------------------------
   --  The fact/policy merge, checked by the type checker.
   ---------------------------------------------------------------------------
   --  Policy as a laptop plan writes it (every field but the fact).
   Policy : constant String :=
     "(HID_Parameters" &
     " x => (HID_Axis numerator => 1 denominator => 1 direction => HID_Direction.As_Reported)" &
     " y => (HID_Axis numerator => 1 denominator => 1 direction => HID_Direction.As_Reported)" &
     " confidence => HID_Confidence.Require_Confident" &
     " buttons => (HID_Buttons button_1 => HID_Button.Primary_Button" &
     " button_2 => HID_Button.Secondary_Button button_3 => HID_Button.Middle_Button)" &
     " contact_limit => 5 binding_budget => 65536" &
     " timing => (HID_Timing startup => 10000 control => 1000 read => 250 cleanup => 1000 idle_backoff => 4))";

   Declared_One : constant CP.Declared_Array := [1 => Field ("descriptor_register"), others => (others => <>)];

   function Facts_Of (Name : String; Value : Integer_64) return CS.Fact_Array is
     [1 => (Field => Field (Name), Value => Value), others => (others => <>)];

   procedure Merge
     (Text : String; Declared : CP.Declared_Array; Declared_Total : Natural;
      Facts : CS.Fact_Array; Fact_Total : Natural;
      Merged : out String; Merged_Length : out Natural; Status : out CP.Merge_Status) is
   begin
      CP.Merge (Text, Declared, Declared_Total, Facts, Fact_Total, Merged, Merged_Length, Status);
   end Merge;

   --  The checker's canonical source of Source, "" when refused (Message
   --  says why).
   Last_Message : String (1 .. 160);
   Last_Message_Length : Natural := 0;
   function Canonical (Source : String) return String is
      Result : CCL.Typed_Values.Check_Result;
   begin
      CCL.Typed_Values.Check (Schema, "HID_Parameters", Source, Result);
      Last_Message_Length := Result.Message_Length;
      Last_Message (1 .. Result.Message_Length) := Result.Message (1 .. Result.Message_Length);
      return (if Result.Success then Result.Canonical (1 .. Result.Length) else "");
   end Canonical;

   procedure Merge_Tests is
      Merged : String (1 .. CP.Maximum_Merged_Bytes);
      Merged_Length : Natural;
      Status : CP.Merge_Status;
   begin
      Merge (Policy, Declared_One, 1, Facts_Of ("descriptor_register", 32), 1, Merged, Merged_Length, Status);
      Check (Status = CP.Merged, "policy and the descriptor fact merge");
      Check (Merged (1 .. Merged_Length) = Policy (Policy'First .. Policy'Last - 1) & " descriptor_register => 32)",
             "the fact is appended as a named association");
      declare
         Text : constant String := Canonical (Merged (1 .. Merged_Length));
      begin
         Check (Ada.Strings.Fixed.Index (Text, "(HID_Parameters descriptor_register => 32 x => ") = 1,
                "the type checker accepts it; the fact is the first field in canonical order: " & Text);
      end;

      --  The policy names the fact field too: a repeated field.
      declare
         Policy_Sets_Fact : constant String :=
           Policy (Policy'First .. Policy'Last - 1) & " descriptor_register => 1)";
      begin
         Merge (Policy_Sets_Fact, Declared_One, 1, Facts_Of ("descriptor_register", 32), 1, Merged, Merged_Length, Status);
         Check (Status = CP.Merged, "merge itself leaves a repeated field to the checker");
         Check (Canonical (Merged (1 .. Merged_Length)) = "", "the plan may not set a fact field");
         Check (Ada.Strings.Fixed.Index (Last_Message (1 .. Last_Message_Length), "descriptor_register") > 0,
                "the refusal names the field: " & Last_Message (1 .. Last_Message_Length));
      end;
      --  A policy field the plan left out: the supervisor cannot fill it.
      declare
         Missing : constant String := Ada.Strings.Fixed.Replace_Slice
           (Policy, Ada.Strings.Fixed.Index (Policy, " contact_limit => 5"),
            Ada.Strings.Fixed.Index (Policy, " contact_limit => 5") + 18, "");
      begin
         Merge (Missing, Declared_One, 1, Facts_Of ("descriptor_register", 32), 1, Merged, Merged_Length, Status);
         Check (Status = CP.Merged and then Canonical (Merged (1 .. Merged_Length)) = "",
                "a policy field nobody wrote is refused");
         Merge (Missing, [1 => Field ("descriptor_register"), 2 => Field ("contact_limit"), others => (others => <>)], 2,
                [1 => (Field ("descriptor_register"), 32), 2 => (Field ("contact_limit"), 5), others => (others => <>)], 2,
                Merged, Merged_Length, Status);
         Check (Status = CP.Merged and then Canonical (Merged (1 .. Merged_Length)) /= "",
                "a field the plan declares a fact is the supervisor's to give");
      end;
      Merge (Policy, Declared_One, 1, Facts_Of ("descriptor_register", 70_000), 1, Merged, Merged_Length, Status);
      Check (Status = CP.Merged and then Canonical (Merged (1 .. Merged_Length)) = "",
             "a fact outside its field's range (HID_Register 0 .. 65535)");
      Merge (Policy, Declared_One, 1, Facts_Of ("descriptor_register", -1), 1, Merged, Merged_Length, Status);
      Check (Status = CP.Merged and then Ada.Strings.Fixed.Index (Merged (1 .. Merged_Length), "=> -1)") > 0
             and then Canonical (Merged (1 .. Merged_Length)) = "", "a negative fact");
      Merge (Policy, Declared_One, 1, Facts_Of ("contact_limit", 3), 1, Merged, Merged_Length, Status);
      Check (Status = CP.Fact_Not_Declared and then Merged_Length = 0, "the supervisor may not give a policy field");
      Merge (Policy, Declared_One, 1, Facts_Of ("descriptor_register", 32), 0, Merged, Merged_Length, Status);
      Check (Status = CP.Fact_Missing, "a declared fact not given");
      Merge ("HID_Parameters x => 1", Declared_One, 1, Facts_Of ("descriptor_register", 32), 1,
             Merged, Merged_Length, Status);
      Check (Status = CP.Policy_Not_Record, "a policy that is not one form");
      Merge (Policy, [1 => Field ("descriptor_register"), 2 => Field ("descriptor_register"), others => (others => <>)], 2,
             Facts_Of ("descriptor_register", 32), 1, Merged, Merged_Length, Status);
      Check (Status = CP.Declaration_Invalid, "a fact declared twice");
      declare
         Long : constant String := "(R" & [1 .. CP.Maximum_Merged_Bytes - 10 => ' '] & ")";
      begin
         Merge (Long, Declared_One, 1, Facts_Of ("descriptor_register", 32), 1, Merged, Merged_Length, Status);
         Check (Status = CP.Too_Long and then Merged_Length = 0, "a merge past one value's length");
      end;
   end Merge_Tests;

   ---------------------------------------------------------------------------
   --  Class launches: the typed check, and startup plans.
   ---------------------------------------------------------------------------
   type Path_Access is access constant String;
   P1 : aliased constant String := "../../tests/hardware/init-usb-live-build.ccl";
   P2 : aliased constant String := "../../tests/device-builds/init-usb-live-build-test.ccl";
   P3 : aliased constant String := "../../tests/hardware/init-installed-build.ccl";
   P4 : aliased constant String := "../../tests/device-builds/init-installed-build-test.ccl";
   P5 : aliased constant String := "../../tests/hardware/init-laptop-install.ccl";
   P6 : aliased constant String := "../../tests/hardware/init-laptop-live.ccl";
   P7 : aliased constant String := "../../tests/headless/init-gpio-launch.ccl";
   Plan_Paths : constant array (1 .. 7) of Path_Access :=
     [P1'Access, P2'Access, P3'Access, P4'Access, P5'Access, P6'Access, P7'Access];

   function Compiled (Source : String) return CCL.Configurations.Compilation_Result is
      Result : CCL.Configurations.Compilation_Result;
   begin
      CCL.Configurations.Compile (Source, Result);
      return Result;
   end Compiled;

   function Launch_Of (Facts_Text, Policy_Text : String) return String is
     ("(Device_Class_Launch facts => [" & Facts_Text & "] policy => " & Policy_Text & ")");

   --  The typed check of a class launch, as procmgr makes it, against
   --  hid-i2c's own declarations (manifest text: whitespace and comments
   --  as written, so Policy_Schema is tolerant of both).
   function Class_Check (Launch : String; Facts : CS.Fact_Array; Fact_Total : Natural)
     return Class_Launch_Checks.Result
   is
      Item : Class_Launch_Checks.Result;
   begin
      Class_Launch_Checks.Check (Schema, "HID_Parameters", Launch, Facts, Fact_Total, Item);
      return Item;
   end Class_Check;

   procedure Class_Tests is
      use type Class_Launch_Checks.Outcome;
      Descriptor : constant CS.Fact_Array := Facts_Of ("descriptor_register", 32);
      Good : constant Class_Launch_Checks.Result :=
        Class_Check (Launch_Of ("""descriptor_register""", Policy), Descriptor, 1);
      Schema_Text : String (1 .. CP.Maximum_Schema_Bytes);
      Schema_Length : Natural;
      Built : CP.Schema_Status;
      First, Last : Natural;
      Read : CP.Read_Status;
      function Message (Item : Class_Launch_Checks.Result) return String is
        (Item.Message (1 .. Item.Message_Length));
   begin
      Check (Good.Status = Class_Launch_Checks.Merged, "a typed class launch merges: " & Message (Good));
      Check (Canonical (Good.Merged_Value (1 .. Good.Length)) /= "",
             "the merged record is a HID_Parameters: " & Last_Message (1 .. Last_Message_Length));
      Check (Ada.Strings.Fixed.Index (Good.Merged_Value (1 .. Good.Length), "descriptor_register => 32)") > 0,
             "the fact is the supervisor's value");

      --  The schema: the root record without the fact field.
      CP.Policy_Schema (Schema, "HID_Parameters", Descriptor, 1, Schema_Text, Schema_Length, Built);
      Check (Built = CP.Built and then Ada.Strings.Fixed.Index (Schema_Text (1 .. Schema_Length), "descriptor_register") = 0
             and then Ada.Strings.Fixed.Index (Schema_Text (1 .. Schema_Length), "(contact_limit HID_Contact_Limit)") > 0
             and then Ada.Strings.Fixed.Index (Schema_Text (1 .. Schema_Length),
                        "(type Device_Class_Launch (record (facts (List String)) (policy HID_Parameters)))") > 0,
             "the policy schema drops exactly the fact field");
      CP.Policy_Schema (Schema, "HID_Parameters", Facts_Of ("no_such_field", 1), 1, Schema_Text, Schema_Length, Built);
      Check (Built = CP.Fact_Not_Field and then Schema_Length = 0, "a fact that is no field of the record");
      CP.Policy_Schema (Schema, "HID_Timing", Descriptor, 1, Schema_Text, Schema_Length, Built);
      Check (Built = CP.Fact_Not_Field, "a fact of another record");
      CP.Policy_Schema (Schema, "HID_Register", Descriptor, 1, Schema_Text, Schema_Length, Built);
      Check (Built = CP.Root_Not_Record, "a root that is no record");
      CP.Policy_Schema ("(type R (record (s String ""a ) b"") (n N)))", "R", Facts_Of ("n", 1), 1,
                        Schema_Text, Schema_Length, Built);
      Check (Built = CP.Built and then Schema_Text (1 .. Ada.Strings.Fixed.Index (Schema_Text (1 .. Schema_Length), " (type D") - 1)
               = "(type R (record (s String ""a ) b"")))",
             "a default string with a parenthesis is one field: " & Schema_Text (1 .. Schema_Length));

      --  The fact/policy split, by the type checker.
      declare
         Sets_Fact : constant Class_Launch_Checks.Result := Class_Check
           (Launch_Of ("""descriptor_register""", Policy (Policy'First .. Policy'Last - 1) & " descriptor_register => 1)"),
            Descriptor, 1);
         Missing : constant Class_Launch_Checks.Result := Class_Check
           (Launch_Of ("""descriptor_register""", Ada.Strings.Fixed.Replace_Slice
              (Policy, Ada.Strings.Fixed.Index (Policy, " contact_limit => 5"),
               Ada.Strings.Fixed.Index (Policy, " contact_limit => 5") + 18, "")), Descriptor, 1);
      begin
         Check (Sets_Fact.Status = Class_Launch_Checks.Not_Typed
                and then Ada.Strings.Fixed.Index (Message (Sets_Fact), "descriptor_register") > 0,
                "the plan may not set a fact field: " & Message (Sets_Fact));
         Check (Missing.Status = Class_Launch_Checks.Not_Typed, "a policy field nobody wrote: " & Message (Missing));
      end;
      --  (An empty list may not type at all: refused either way.)
      Check (Class_Check (Launch_Of ("", Policy), Descriptor, 1).Status /= Class_Launch_Checks.Merged,
             "the plan declares no fact the supervisor gives");
      Check (Class_Check (Launch_Of ("""descriptor_register"" ""contact_limit""", Policy), Descriptor, 1).Status
               = Class_Launch_Checks.Facts_Differ, "the plan declares a fact the supervisor does not give");
      Check (Class_Check (Launch_Of ("""descriptor_register""", Policy), Facts_Of ("contact_limit", 3), 1).Status
               /= Class_Launch_Checks.Merged, "the supervisor may not give a policy field");
      declare
         Wide : constant Class_Launch_Checks.Result :=
           Class_Check (Launch_Of ("""descriptor_register""", Policy), Facts_Of ("descriptor_register", 70_000), 1);
      begin
         Check (Wide.Status = Class_Launch_Checks.Merged and then Canonical (Wide.Merged_Value (1 .. Wide.Length)) = "",
                "a fact outside its field's range is refused by the full check");
      end;
      Check (Class_Check ("(HID_Parameters)", Descriptor, 1).Status = Class_Launch_Checks.Not_Typed,
             "a value that is no Device_Class_Launch");
      Check (Class_Check (Launch_Of ("""descriptor_register""", "(HID_Timing startup => 1 control => 1 read => 1 " &
                                     "cleanup => 1 idle_backoff => 1)"), Descriptor, 1).Status
               = Class_Launch_Checks.Not_Typed, "a policy of another record type");

      --  Read_Launch on canonical text.
      declare
         Text : constant String := "(Device_Class_Launch facts => [""descriptor_register""] policy => (R a => 1))";
      begin
         CP.Read_Launch (Text, Descriptor, 1, First, Last, Read);
         Check (Read = CP.Read and then Text (First .. Last) = "(R a => 1)", "Read_Launch finds the policy");
      end;
      CP.Read_Launch ("(Device_Class_Launch facts => [""descriptor_register"" ""descriptor_register""] policy => (R))",
                      Descriptor, 1, First, Last, Read);
      Check (Read = CP.Facts_Differ, "a fact listed twice");
      CP.Read_Launch ("(Device_Class_Launch facts => [] policy => R)", Descriptor, 0, First, Last, Read);
      Check (Read = CP.Not_Canonical, "a policy that is not a form");
   end Class_Tests;

   procedure Plan_Tests is
      Start : constant String := "(startup v1 (start ""logstore.svc"" (priority 5)) ";
      Value : constant String := Launch_Of ("""descriptor_register""", Policy);
      Good : constant CCL.Configurations.Compilation_Result :=
        Compiled (Start & "(driver-class ""PNP0C50"" " & Value & "))");
   begin
      Check (Good.Success and then Good.Plan.Class_Total = 1, "a plan with a class launch compiles");
      if Good.Success then
         declare
            Block : CCL.Configurations.Class_Block renames Good.Plan.Classes (1);
         begin
            Check (Block.Class.Data (1 .. Block.Class.Length) = "PNP0C50"
                   and then Block.Launch.Data (1 .. Block.Launch.Length) = Value,
                   "the entry keeps its class and its value as written");
         end;
      end if;
      Check (Compiled (Start & "(driver-class ""pnp0c50"" (R)))").Diagnostic =
               CCL.Configurations.Invalid_Driver_Class, "a lower-case class");
      Check (Compiled (Start & "(driver-class ""PNP0C50XX"" (R)))").Diagnostic =
               CCL.Configurations.Invalid_Driver_Class, "a class past eight characters");
      Check (Compiled (Start & "(driver-class ""PNP0C50"" (R)) (driver-class ""PNP0C50"" (R)))").Diagnostic =
               CCL.Configurations.Invalid_Driver_Class, "a class given twice");
      Check (Compiled (Start & "(driver-class ""PNP0C50""))").Diagnostic =
               CCL.Configurations.Invalid_Driver_Class, "an entry without its value");
      Check (Compiled (Start & "(driver-class ""PNP0C50"" (R) (R)))").Diagnostic =
               CCL.Configurations.Invalid_Driver_Class, "two values");
      --  The old keyword form is one value followed by another: refused.
      Check (not Compiled (Start & "(driver-class ""PNP0C50"" (facts ""a"") (parameters (R))))").Success,
             "the retired (facts ...) (parameters ...) form");

      --  Every laptop plan that carries a PNP0C50 launch (twins alike):
      --  it is a Device_Class_Launch whose policy is a HID_Parameters
      --  without the descriptor register, which devmgr gives.
      for Path of Plan_Paths loop
         declare
            Plan : constant CCL.Configurations.Compilation_Result := Compiled (Read_File (Path.all));
            Found : Natural := 0;
         begin
            Check (Plan.Success, Path.all & " compiles: " &
                   CCL.Configurations.Diagnostic_Name (Plan.Diagnostic) & " at" & Plan.Position'Image);
            for K in 1 .. Plan.Plan.Class_Total loop
               if Plan.Plan.Classes (K).Class.Data (1 .. Plan.Plan.Classes (K).Class.Length) = "PNP0C50" then
                  Found := K;
               end if;
            end loop;
            Check (Found /= 0, Path.all & " has a PNP0C50 class launch");
            if Found /= 0 then
               declare
                  Block : CCL.Configurations.Class_Block renames Plan.Plan.Classes (Found);
                  Item : constant Class_Launch_Checks.Result :=
                    Class_Check (Block.Launch.Data (1 .. Block.Launch.Length), Facts_Of ("descriptor_register", 32), 1);
                  use type Class_Launch_Checks.Outcome;
               begin
                  Check (Item.Status = Class_Launch_Checks.Merged and then
                         Canonical (Item.Merged_Value (1 .. Item.Length)) /= "",
                         Path.all & ": a HID_Parameters with devmgr's descriptor fact: " &
                         Item.Message (1 .. Item.Message_Length) & Last_Message (1 .. Last_Message_Length));
                  if Item.Status = Class_Launch_Checks.Merged then
                     Put_Line (Path.all & ": " & Item.Merged_Value (1 .. Item.Length));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Plan_Tests;

   ---------------------------------------------------------------------------
   --  Supervision edges.
   ---------------------------------------------------------------------------
   procedure Edge_Tests is
      Edges : SE.Edge_Array (1 .. 8);
      Length : Natural := 0;
      Next : Unsigned_64 := 1;
      Root : constant SE.Process_Word := 7;
      A : constant SE.Process_Word := 100;
      B : constant SE.Process_Word := 200;
      C : constant SE.Process_Word := 300;
      History : SE.Restart_History;
      Decision : SE.Restart_Decision;
   begin
      Check (SE.Well_Formed (Edges, Length, Next), "no edges are well formed");
      SE.Add (Edges, Length, Next, A, Root, CS.Temporary);
      SE.Add (Edges, Length, Next, B, A, CS.Permanent);
      SE.Add (Edges, Length, Next, C, A, CS.Transient);
      Check (Length = 3 and then SE.Well_Formed (Edges, Length, Next), "a chain root -> a -> {b, c}");
      Check (SE.Supervisor_Of (Edges, Length, B) = A and then SE.Supervisor_Of (Edges, Length, A) = Root
             and then SE.Supervisor_Of (Edges, Length, Root) = SE.No_Process, "each child's one supervisor");
      Check (not SE.Fresh (Edges, Length, A) and then not SE.Fresh (Edges, Length, Root)
             and then SE.Fresh (Edges, Length, 400), "freshness");
      Check (Edges (SE.Find_Child (Edges, Length, B)).Rank > Edges (SE.Find_Child (Edges, Length, A)).Rank,
             "a supervisor is ranked before its children");
      --  a ends: its own edge goes, its children are marked for reaping.
      SE.Remove (Edges, Length, Next, SE.Find_Child (Edges, Length, A));
      for K in 1 .. Length loop
         if Edges (K).Supervisor = A then
            SE.Mark_Reaping (Edges, Length, Next, K);
         end if;
      end loop;
      Check (Length = 2 and then Edges (1).Reaping and then Edges (2).Reaping
             and then SE.Find_Child (Edges, Length, A) = 0, "a's children are reaped");
      SE.Remove (Edges, Length, Next, 1);
      SE.Remove (Edges, Length, Next, 1);
      Check (Length = 0 and then SE.Well_Formed (Edges, Length, Next), "every edge removed");
      --  An identity is fresh again only once no edge names it.
      SE.Add (Edges, Length, Next, A, Root, CS.Temporary);
      Check (Length = 1 and then Edges (1).Rank = 4, "ranks never repeat");

      --  Restart decisions.
      SE.Decide (CS.Temporary, 0, 0, True, False, 1_000, History, Decision);
      Check (Decision = SE.Stay_Ended, "temporary: never restarted");
      SE.Decide (CS.Transient, 2, 1_000, False, False, 1_000, History, Decision);
      Check (Decision = SE.Stay_Ended, "transient after a normal end");
      SE.Decide (CS.Permanent, 2, 1_000, False, True, 1_000, History, Decision);
      Check (Decision = SE.Stay_Ended, "a reaped child is never restarted");
      History := (others => <>);
      SE.Decide (CS.Transient, 2, 1_000, True, False, 1_000, History, Decision);
      Check (Decision = SE.Restart, "transient after a failure");
      SE.Decide (CS.Permanent, 2, 1_000, False, False, 1_500, History, Decision);
      Check (Decision = SE.Restart, "the second restart in the window");
      SE.Decide (CS.Permanent, 2, 1_000, False, False, 1_900, History, Decision);
      Check (Decision = SE.Escalate, "the third within the window escalates");
      SE.Decide (CS.Permanent, 2, 1_000, False, False, 2_000, History, Decision);
      Check (Decision = SE.Restart and then History.Restarts = 1, "a new window");
   end Edge_Tests;
begin
   Spec_Tests;
   Merge_Tests;
   Class_Tests;
   Plan_Tests;
   Edge_Tests;
   Put_Line ("supervision:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error with "supervision tests failed";
   end if;
end Supervision_Tests;
