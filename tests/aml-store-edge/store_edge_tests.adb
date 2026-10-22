pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with AML_Interface_Policy;
with Firmware_Tables;
with Store_Edge_Data;
procedure Store_Edge_Tests is
   subtype Bytes is Firmware_Tables.Bytes;
   use type Bytes;
   function Configured return AML_Interface_Policy.Policy is
      use type AML_Interface_Policy.Add_Status;
      Item : AML_Interface_Policy.Policy := AML_Interface_Policy.Empty;
      Status : AML_Interface_Policy.Add_Status;
      Text : constant String := "CuBitProbeSupported";
      Encoded : Bytes (1 .. Text'Length);
   begin
      for I in Text'Range loop Encoded (I) := Character'Pos (Text (I)); end loop;
      AML_Interface_Policy.Add (Item, Encoded, Status);
      if Status /= AML_Interface_Policy.Added then raise Program_Error; end if;
      return Item;
   end Configured;
   package S is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Supported_Interfaces => Configured);
   use type S.Stage_Status;
   use type S.Freeze_Status;
   use type S.Execution_Phase_Result;
   use type S.Values.Access_Status;
   use type S.Values.Module_Outcome;
   use type S.Values.Preparation_Outcome;
   use type S.Values.Value_Kind;
   use type AML_Execute.Execution_Status;
   Checks, Evaluations : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   type Observation_Kind is (Scalar, Buffer_Value, Buffer_Index_Failure);
   type Expected_Result is record
      Name : String (1 .. 4);
      Kind : Observation_Kind;
      Number : Unsigned_64;
      Count : Natural range 0 .. 2;
      Data : Bytes (1 .. 2);
   end record;
   type Expectations is array (Positive range <>) of Expected_Result;
   procedure Run (Label : String; Revision : Positive; First, Second : Bytes; Expected : Expectations) is
      Service : aliased S.State (2, 8192, 4096);
      Stage : S.Stage_Status;
      Frozen : S.Freeze_Status;
      Report : S.Execution_Report;
      Status : S.Values.Access_Status;
      Initialized : S.Namespace.Initialization_Report;
      Node : S.Namespace.Node_ID;
      Result : S.Values.Result;
      Description : S.Values.Value_Description;
   begin
      Check (Natural (First (9)) = Revision and then Natural (Second (9)) = Revision, Label & " revision");
      S.Stage_Table (Service, 1, S.DSDT, First, Stage); Check (Stage = S.Staged, Label & " first stage");
      S.Stage_Table (Service, 2, S.SSDT, Second, Stage); Check (Stage = S.Staged, Label & " second stage");
      Check (S.Observe (Service).Objects = 0, Label & " staging empty");
      S.Freeze_Catalog (Service, Frozen); Check (Frozen = S.Catalog_Frozen_OK, Label & " freeze");
      S.Execute_Frozen (Service, 10000, Report, Status);
      Check (Status = S.Values.Available and then Report.Phase = S.Module_Execution_Reported
        and then Report.Preparation = S.Values.Firmware_Prepared
        and then Report.Modules.Outcome = S.Values.Modules_Completed
        and then Report.Modules.Completed = 2 and then Report.Modules.Charged <= 10000, Label & " execute");
      S.Initialize_Members (Service, Initialized, Status); Check (Status = S.Values.Available, Label & " seal");
      for Item of Expected loop
         S.Find_Child (Service, S.Namespace.Root, Item.Name, Node, Status);
         Check (Status = S.Values.Available and then Node /= S.Namespace.Root, Label & " find " & Item.Name);
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available and then Result.Charged <= 10000, Label & " invoke " & Item.Name);
         case Item.Kind is
            when Scalar =>
               Check (Result.Status = AML_Execute.Returned,
                 Label & " " & Item.Name & " scalar status " & Result.Status'Image);
               Check (Result.Number = Item.Number,
                 Label & " " & Item.Name & " expected" & Item.Number'Image & " actual" & Result.Number'Image);
            when Buffer_Index_Failure =>
               -- This exact out-of-range Index maps ACPICA AE_AML_BUFFER_LIMIT
               -- to CuBit's coarser Unsupported_Value. No other error accepted.
               Check (Item.Name = "TEST" and then Result.Status = AML_Execute.Unsupported_Value,
                 Label & " expected Index failure, actual " & Result.Status'Image);
            when Buffer_Value =>
               Check (Result.Status = AML_Execute.Object_Returned, Label & " buffer status");
               Check (S.Retained_Results (Service) = 1, Label & " buffer retained");
               S.Describe_Result (Service, Result.Handle, Description, Status);
               Check (Status = S.Values.Available and then Description.Kind = S.Values.Buffer_Description
                 and then Description.Length = Item.Count, Label & " buffer extent");
               declare
                  Data : Bytes (1 .. Item.Count);
                  Copied : Natural;
               begin
                  S.Read_Result_Bytes (Service, Result.Handle, 0, Data, Copied, Status);
                  Check (Status = S.Values.Available and then Copied = Item.Count
                    and then Data = Item.Data (1 .. Item.Count), Label & " exact buffer bytes");
               end;
               S.Release_Result (Service, Result.Handle, Status);
               Check (Status = S.Values.Available, Label & " release buffer");
         end case;
         Check (S.Retained_Results (Service) = 0, Label & " retained leak");
         Evaluations := Evaluations + 1;
      end loop;
   end Run;
begin
   Run ("r1-local-self-read", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_1,
     [("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r1-local-self-expression", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_2,
     [("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r1-arg-self", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_3,
     [("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#00000000FFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r1-failed-target-effects", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_4,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Buffer_Index_Failure, 0, 0, [16#00#, 16#00#]),
      ("PRE0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("AFT0", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 1, [16#00#, 16#00#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Buffer_Index_Failure, 0, 0, [16#00#, 16#00#]),
      ("PRE0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("AFT0", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 1, [16#00#, 16#00#])]);
   Run ("r1-alias-rebind-store", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_5,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#22#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#22#])]);
   Run ("r1-alias-rebind-copyobject", 1, Store_Edge_Data.Base_1, Store_Edge_Data.Case_6,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000011#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#11#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000011#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#11#])]);
   Run ("r2-local-self-read", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_7,
     [("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r2-local-self-expression", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_8,
     [("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r2-arg-self", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_9,
     [("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#FFFFFFFFFFFFFFFF#, 0, [16#00#, 16#00#])]);
   Run ("r2-failed-target-effects", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_10,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Buffer_Index_Failure, 0, 0, [16#00#, 16#00#]),
      ("PRE0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("AFT0", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 1, [16#00#, 16#00#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Buffer_Index_Failure, 0, 0, [16#00#, 16#00#]),
      ("PRE0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("AFT0", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 1, [16#00#, 16#00#])]);
   Run ("r2-alias-rebind-store", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_11,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#22#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#22#])]);
   Run ("r2-alias-rebind-copyobject", 2, Store_Edge_Data.Base_2, Store_Edge_Data.Case_12,
     [("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000011#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#11#]),
      ("INIT", Scalar, 16#0000000000000000#, 0, [16#00#, 16#00#]),
      ("TEST", Scalar, 16#0000000000000011#, 0, [16#00#, 16#00#]),
      ("POST", Scalar, 16#0000000000000022#, 0, [16#00#, 16#00#]),
      ("FLG0", Scalar, 16#0000000000000001#, 0, [16#00#, 16#00#]),
      ("BVAL", Buffer_Value, 0, 2, [16#00#, 16#11#])]);
   Check (Evaluations = 76, "all exact oracle observations visited");
   Ada.Text_IO.Put_Line ("Store edge checks" & Checks'Image & " observations" & Evaluations'Image);
end Store_Edge_Tests;
