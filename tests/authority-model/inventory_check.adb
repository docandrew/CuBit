--  Checks the typed CCL side of the authority model (CAP-012) on the CCL
--  VM (CCL.Evaluation: analyse, compile, verify, run):
--    * rules.ccl's classify equals the SPARK model's Authority_Model.
--      Classify on every one of the 4**5 combinations of facts;
--    * every inventory path's recorded verdict equals classify of its
--      facts, path ids are unique, and a wrong verdict is caught;
--    * every authority kind has exactly one root issuer;
--    * every recorded synchronous call goes down the layering;
--    * an unknown kind is refused by the type checker.
--  Argument: the directory holding rules.ccl and inventory.ccl.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Authority_Model;
with CCL.Evaluation;
with CCL.Language; use CCL.Language;
with CCL.Sessions;

procedure Inventory_Check is
   package AM renames Authority_Model;

   Fuel : constant := 200_000_000;
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Read (Name : String) return String is
      F : File_Type;
      Text : Unbounded_String;
   begin
      Open (F, In_File, Name);
      while not End_Of_File (F) loop
         Append (Text, Get_Line (F));
         Append (Text, ASCII.LF);
      end loop;
      Close (F);
      return To_String (Text);
   end Read;

   Dir : constant String :=
     (if Ada.Command_Line.Argument_Count >= 1 then Ada.Command_Line.Argument (1) else ".");
   Rules : constant String := Read (Dir & "/rules.ccl");
   Inventory : constant String := Read (Dir & "/inventory.ccl");

   --  Evaluate Source; its Integer result, or -1 (with the reason printed).
   function Integer_Of (Source : String; What : String) return Integer is
      R : Interpretation_Result;
      Prefix : constant String := "Integer: ";
   begin
      CCL.Evaluation.Evaluate (Source, Fuel, R);
      if R.Status /= Succeeded then
         Put_Line (What & ": " & R.Status'Image & " " & CCL.Sessions.Result_Image (R));
         return -1;
      end if;
      declare
         Image : constant String := Ada.Strings.Fixed.Trim (CCL.Sessions.Result_Image (R), Ada.Strings.Both);
      begin
         if Image'Length > Prefix'Length
           and then Image (Image'First .. Image'First + Prefix'Length - 1) = Prefix
         then
            return Integer'Value (Image (Image'First + Prefix'Length .. Image'Last));
         end if;
         Put_Line (What & ": not an Integer: " & Image);
         return -1;
      end;
   end Integer_Of;

   function Fact_Name (F : AM.Fact) return String is
     ("Fact." & (case F is
                   when AM.Enforced => "Enforced", when AM.Observed => "Observed",
                   when AM.Fails => "Fails", when AM.Unknown => "Unknown"));

   function Verdict_Number (V : AM.Verdict) return Integer is (AM.Verdict'Pos (V));

   Verdict_Query : constant String :=
     "(if (= v Verdict.Fits) 0 (if (= v Verdict.Violates) 1 2))";

   --  The 29 kinds of rules.ccl, each of which must have one root.
   --  Every kind of rules.ccl, as Family:field:Type.Member, each of which
   --  must have exactly one root issuer.
   Kinds : constant String :=
     "Spawn:spawn:SpawnK.Process_Control Hardware:hardware:HwK.Device_Memory " &
     "Hardware:hardware:HwK.Firmware_Memory Hardware:hardware:HwK.IO_Ports " &
     "Hardware:hardware:HwK.Interrupt_Line Hardware:hardware:HwK.Device_Vectors " &
     "Hardware:hardware:HwK.DMA Hardware:hardware:HwK.Platform_Pool " &
     "Hardware:hardware:HwK.Firmware_Data Hardware:hardware:HwK.Scheduling " &
     "Hardware:hardware:HwK.Whole_Disk Endpoint:endpoint:EpK.Service_Endpoint " &
     "Endpoint:endpoint:EpK.Notification_Role Data:data:DataK.Data_Grant " &
     "Data:data:DataK.Filesystem_Scope Data:data:DataK.Network_Scope " &
     "Data:data:DataK.Config_Scope Data:data:DataK.Secret_Use " &
     "Observation:observation:ObsK.Log_Observation Observation:observation:ObsK.Metric_Observation " &
     "Observation:observation:ObsK.Process_Observation Observation:observation:ObsK.Device_Inventory " &
     "Observation:observation:ObsK.Trace Observation:observation:ObsK.Test_Console " &
     "Observation:observation:ObsK.Firmware_Snapshot Administration:administration:AdmK.Platform_Power " &
     "Administration:administration:AdmK.Startup_Plan Administration:administration:AdmK.Trust_List " &
     "Administration:administration:AdmK.Settings ";

   Mismatch_Query : constant String :=
     "(count (fn ((p Path)) (not (= (field p verdict) (classify (field p facts))))) (paths))";

begin
   --  1. classify (CCL) = Classify (SPARK) on every combination of facts.
   declare
      Agree : Natural := 0;
   begin
      for P in AM.Fact loop
         for A in AM.Fact loop
            for B in AM.Fact loop
               for R in AM.Fact loop
                  for C in AM.Fact loop
                     declare
                        F : constant AM.Facts :=
                          [AM.Provenance => P, AM.No_Amplification => A, AM.Broker_Inert => B,
                           AM.Revocation => R, AM.Call_Order => C];
                        Source : constant String :=
                          Rules & "(let ((v (classify (Invariant_Facts " & Fact_Name (P) & " " &
                          Fact_Name (A) & " " & Fact_Name (B) & " " & Fact_Name (R) & " " &
                          Fact_Name (C) & ")))) " & Verdict_Query & ")";
                        Got : constant Integer := Integer_Of (Source, "classify");
                     begin
                        if Got = Verdict_Number (AM.Classify (F)) then
                           Agree := Agree + 1;
                        else
                           Check (False, "classify differs from the SPARK model on " & Source);
                        end if;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Check (Agree = 4 ** 5, "classify agrees on all 1024 combinations");
      Put_Line ("classify: CCL and SPARK agree on" & Agree'Image & " fact combinations");
   end;

   --  2. The inventory.
   declare
      Both : constant String := Rules & Inventory;
      Total : constant Integer := Integer_Of (Both & "(length (paths))", "paths");
      function Count_Of (V : String) return Integer is
        (Integer_Of (Both & "(count (fn ((p Path)) (= (field p verdict) Verdict." & V & ")) (paths))", V));
      Fits : constant Integer := Count_Of ("Fits");
      Violates : constant Integer := Count_Of ("Violates");
      Unclear : constant Integer := Count_Of ("Unclear");
   begin
      Check (Total > 0, "the inventory evaluates");
      Put_Line ("inventory:" & Total'Image & " paths:" & Fits'Image & " fit," & Violates'Image &
                " violate," & Unclear'Image & " unclear");
      Check (Fits + Violates + Unclear = Total, "every path has a verdict");
      Check (Integer_Of (Both & Mismatch_Query, "verdicts") = 0,
             "every recorded verdict equals classify of its facts");
      Check (Integer_Of (Both & "(count (fn ((p Path)) (not (well_formed (field p kind)))) (paths))", "kinds") = 0
             and then Integer_Of (Both & "(count (fn ((r Root)) (not (well_formed (field r kind)))) (roots))",
                                  "root kinds") = 0,
             "every kind names exactly its family's field");
      --  Path ids are unique (read from the source text: an all-pairs
      --  query exceeds the VM's object storage), and the VM saw them all.
      declare
         Marker : constant String := "(Path """;
         Ids : Unbounded_String := To_Unbounded_String (" ");
         Seen_Count : Natural := 0;
         Duplicates : Natural := 0;
         From : Natural := Inventory'First;
      begin
         loop
            From := Ada.Strings.Fixed.Index (Inventory, Marker, From);
            exit when From = 0;
            declare
               Start : constant Positive := From + Marker'Length;
               Stop : constant Natural := Ada.Strings.Fixed.Index (Inventory, """", Start);
               Id : constant String := " " & Inventory (Start .. Stop - 1) & " ";
            begin
               if Ada.Strings.Fixed.Index (To_String (Ids), Id) /= 0 then
                  Duplicates := Duplicates + 1;
                  Put_Line ("duplicate path id" & Id);
               end if;
               Append (Ids, Id (Id'First + 1 .. Id'Last));
               Seen_Count := Seen_Count + 1;
               From := Stop;
            end;
         end loop;
         Check (Duplicates = 0 and then Seen_Count = Total, "path ids are unique and all evaluated");
      end;
      --  Discrimination: one wrong verdict is caught.
      declare
         At_Fits : constant Natural := Ada.Strings.Fixed.Index (Inventory, "Verdict.Fits)");
         Wrong : constant String :=
           Rules & Inventory (Inventory'First .. At_Fits - 1) & "Verdict.Violates)" &
           Inventory (At_Fits + String'("Verdict.Fits)")'Length .. Inventory'Last);
      begin
         Check (At_Fits > 0 and then Integer_Of (Wrong & Mismatch_Query, "wrong verdict") = 1,
                "a wrong recorded verdict is caught");
      end;
      --  Discrimination: an undeclared kind does not type-check.
      declare
         Name : constant String := "SpawnK.Process_Control) Operation.Spawn";
         At_Kind : constant Natural := Ada.Strings.Fixed.Index (Inventory, Name);
         Bad : constant String :=
           Rules & Inventory (Inventory'First .. At_Kind - 1) & "SpawnK.Ambient_Root) Operation.Spawn" &
           Inventory (At_Kind + Name'Length .. Inventory'Last);
         R : Interpretation_Result;
      begin
         CCL.Evaluation.Evaluate (Bad & "(length (paths))", Fuel, R);
         Check (At_Kind > 0 and then R.Status /= Succeeded, "an undeclared kind is refused");
      end;

      --  3. One root issuer per kind.
      declare
         First : Positive := Kinds'First;
         Kind_Count : Natural := 0;
      begin
         for I in Kinds'Range loop
            if Kinds (I) = ' ' then
               declare
                  K : constant String := Kinds (First .. I - 1);
                  C1 : constant Natural := Ada.Strings.Fixed.Index (K, ":");
                  C2 : constant Natural := Ada.Strings.Fixed.Index (K (C1 + 1 .. K'Last), ":");
                  Family : constant String := K (K'First .. C1 - 1);
                  Field_Name : constant String := K (C1 + 1 .. C2 - 1);
                  Member : constant String := K (C2 + 1 .. K'Last);
               begin
                  Kind_Count := Kind_Count + 1;
                  Check (Integer_Of (Both & "(count (fn ((r Root)) (and (= (field (field r kind) family) Family." &
                                     Family & ") (= (field (field r kind) " & Field_Name & ") " & Member &
                                     "))) (roots))", Member) = 1,
                         "exactly one root issuer for " & Member);
               end;
               First := I + 1;
            end if;
         end loop;
         Put_Line ("roots: one issuer for each of" & Kind_Count'Image & " kinds checked");
      end;

      --  4. Calls go down the layering.
      declare
         Calls : constant Integer := Integer_Of (Both & "(length (calls))", "calls");
         Up : constant Integer :=
           Integer_Of (Both & "(count (fn ((c Call)) (not (goes_down c))) (calls))", "calls up");
         Forever : constant Integer :=
           Integer_Of (Both & "(count (fn ((c Call)) (field c forever)) (calls))", "forever");
      begin
         Check (Calls > 0 and then Up = 0, "every recorded synchronous call goes down the layering");
         Put_Line ("calls:" & Calls'Image & " recorded," & Up'Image & " up the layering," &
                   Forever'Image & " with no deadline (Wait_Forever)");
      end;
   end;

   Put_Line ("inventory check:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Inventory_Check;
