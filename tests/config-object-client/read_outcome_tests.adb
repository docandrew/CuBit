with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Host_Values;
with Config_Read_Outcomes;
with Config_Object_Client.Host;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Read_Outcome_Tests is
   package R renames Config_Read_Outcomes;
   package W renames Config_Object_Messages;
   package C renames Config_Object_Client;
   package H renames Config_Object_Client.Host;
   package IPC renames CuBit.Messages;
   use type R.Alternative;
   use type H.Outcome_State;
   use type C.Phase;
   use type C.Submission;
   use type C.Completion_Result;
   use type W.Status;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Catalog.Publication_Result;
   Types : Registry;
   Value_Type, Other_Type, Unbound : Binding;
   Definition, Empty_Definition : R.Description;
   Root, Choice_Type, Ref : Type_Reference;
   Status : Definition_Result;
   Desc : CCL.Types.Description;
   Input, Output, Bad : CCL.Objects.Image;
   Built : Build_Result;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "Config read outcome check" & Checks'Image; end if;
   end Check;
   procedure Prepare (Contract : Binding; Result : out R.Description) is
   begin
      R.Define (Contract, Named ("ConfigSnapshot"), Named ("ConfigRead"), [5, 6, 7, 8], Result, Good);
      Check (Good and R.Is_Defined (Result) and R.Matches (Result, Contract));
   end Prepare;
   procedure Is_Choice (Expected : R.Alternative) is
   begin
      Check (Good and Validate (Output, R.Schema (Definition)));
      Check (Output.Cells (1).First = R.Alternative'Enum_Rep (Expected));
      if Expected not in R.Found | R.Stale then
         Check (Output.Used_Cells = 2 and Output.Used_Bytes = 0);
         Check (Output.Cells (2) = Unit_Cell);
      end if;
   end Is_Choice;
   function Answer (Token : Unsigned_64; Code : W.Status; Word : Unsigned_64 := 0)
     return IPC.CompletionEntry is
     (requestId => 100, token => Token, msg => W.Reply (Code, Word),
      from => 42, status => IPC.COMPLETION_OK, valid => True);
   type Revisions is array (Positive range <>) of Unsigned_64;
begin
   -- A nested product containing text, a sum with text payload, and Boolean.
   Desc := (Identifier => Named ("Reading"), Form => Sum, Count => 2, others => <>);
   Desc.Parts (1) := (Named ("Text"), String_Type);
   Desc.Parts (2) := (Named ("Absent"), Unit_Type);
   Define (Types, Desc, Choice_Type, Status); Check (Status = Defined);
   Desc := (Identifier => Named ("Settings"), Form => Product, Count => 3, others => <>);
   Desc.Parts (1) := (Named ("title"), String_Type);
   Desc.Parts (2) := (Named ("reading"), Choice_Type);
   Desc.Parts (3) := (Named ("enabled"), Boolean_Type);
   Define (Types, Desc, Root, Status); Check (Status = Defined);
   Bind (Types, Root, [1, 2, 3, 4], Value_Type, Good); Check (Good);
   Input := Empty (Value_Type);
   Append (Input, Product_Cell (3), Built); Check (Built = Added);
   Append_Text (Input, "hello", Built); Check (Built = Added);
   Append (Input, Variant_Cell (1), Built); Check (Built = Added);
   Append_Text (Input, "world", Built); Check (Built = Added);
   Append (Input, Boolean_Cell (True), Built); Check (Built = Added);
   Check (Validate (Input, Value_Type));
   Prepare (Value_Type, Definition);
   declare
      Shifted : Registry;
      Moved : Type_Reference;
      Imported : Import_Result;
      Shifted_Binding : Binding;
      Catalog : CCL.Objects.Catalog.Schema_Catalog;
      Published : CCL.Objects.Catalog.Publication_Result;
   begin
      Define (Shifted, (Identifier => Named ("Unrelated"), Form => Product, others => <>), Ref, Status);
      Check (Status = Defined);
      Import_Definition (Types, Root, Shifted, Moved, Imported); Check (Imported = CCL.Types.Imported);
      Bind (Shifted, Moved, Identity (Value_Type), Shifted_Binding, Good); Check (Good);
      Check (Moved /= Root and R.Matches (Definition, Shifted_Binding));
      CCL.Objects.Catalog.Publish (Catalog, Value_Type, Published);
      Check (Published = CCL.Objects.Catalog.Published);
      CCL.Objects.Catalog.Publish (Catalog, R.Schema (Definition), Published);
      Check (Published = CCL.Objects.Catalog.Published);
      Check (CCL.Objects.Catalog.Root_Of (Catalog, Identity (R.Schema (Definition))) /= Invalid_Type);
   end;
   for Valid in Boolean loop
      for Code in W.Status loop
         for Revision of Revisions'[0, 1, W.Maximum_Revision, Unsigned_64'Last] loop
            declare
               Expected : constant R.Alternative :=
                 (if not Valid then R.Invalid_Completion
                  elsif Code in W.Success | W.Stale and Revision in 1 .. W.Maximum_Revision then
                    (if Code = W.Success then R.Found else R.Stale)
                  elsif Revision /= 0 then R.Invalid_Completion
                  else (case Code is when W.Missing => R.Missing, when W.Denied => R.Denied,
                    when W.Busy => R.Busy, when W.Unavailable => R.Unavailable,
                    when W.Schema_Mismatch => R.Schema_Mismatch,
                    when W.Invalid_Request => R.Invalid_Request, when others => R.Invalid_Completion));
            begin
               R.Build (Definition, Valid, Code, Revision, Input, Output, Good);
               Is_Choice (Expected);
               if Expected in R.Found | R.Stale then
                  Check (Output.Used_Cells = Input.Used_Cells + 3 and Output.Used_Bytes = 10);
                  Check (Integer_Of (Output.Cells (3)) = Integer_64 (Revision));
                  Check (Output.Cells (4 .. 8) = Input.Cells (1 .. 5) and Output.Text = Input.Text);
               end if;
            end;
         end loop;
      end loop;
   end loop;
   for Fault in 1 .. 5 loop
      Bad := Input;
      case Fault is
         when 1 => Bad.Schema (0) := 99;
         when 2 => Bad.Cells (4).First := 1; -- overlapping nested text offset
         when 3 => Bad.Reserved := 1;
         when 4 => Bad.Padding (1) := 1;
         when others => Bad.Used_Cells := Unsigned_32'Last;
      end case;
      R.Build (Definition, True, W.Success, 1, Bad, Output, Good);
      Is_Choice (R.Invalid_Completion);
   end loop;
   -- Full text budget, empty strings, and primitive types are not special
   -- protocol encodings. Native text offsets survive the envelope unchanged.
   Bind (Types, String_Type, [1, 2, 3, 4], Other_Type, Good); Check (Good);
   Check (not R.Matches (Definition, Other_Type));
   Prepare (Other_Type, Definition);
   for Full in Boolean loop
      Input := Empty (Other_Type);
      Append_Text (Input, String'(1 .. (if Full then Maximum_Text_Bytes else 0) => 'x'), Built);
      Check (Built = Added);
      R.Build (Definition, True, W.Success, 1, Input, Output, Good);
      Is_Choice (R.Found);
      Check (Output.Used_Bytes = Input.Used_Bytes and Output.Text = Input.Text and
        Output.Cells (4) = Input.Cells (1));
   end loop;
   -- Distinct schema keys and valid, collision-free names are required.
   R.Define (Unbound, Named ("Snapshot"), Named ("Read"), [5, 6, 7, 8], Definition, Good);
   Check (not Good and not R.Is_Defined (Definition));
   R.Define (Other_Type, Named ("Snapshot"), Named ("Read"), No_Schema, Definition, Good);
   Check (not Good and not R.Is_Defined (Definition));
   R.Define (Other_Type, Named ("Snapshot"), Named ("Read"), Identity (Other_Type), Definition, Good);
   Check (not Good and not R.Is_Defined (Definition));
   R.Define (Other_Type, Named ("same"), Named ("same"), [5, 6, 7, 8], Definition, Good);
   Check (not Good and not R.Is_Defined (Definition));
   R.Define (Value_Type, Named ("Settings"), Named ("Read"), [5, 6, 7, 8], Definition, Good);
   Check (not Good and not R.Is_Defined (Definition));
   R.Build (Empty_Definition, True, W.Success, 1, Input, Output, Good); Check (not Good);

   -- The 3-cell envelope is admitted by Types.Define, not silently truncated.
   for Tail_Count in 13 .. 14 loop
      declare
         Dense : Registry;
         Block, Tail, Whole : Type_Reference;
         D : CCL.Types.Description :=
           (Identifier => Named ("Block"), Form => Product, Count => 16, others => <>);
         Contract : Binding;
      begin
         for I in 1 .. 16 loop
            D.Parts (I) := (Named ("f" & Character'Val (Character'Pos ('a') + I - 1)), Integer_Type);
         end loop;
         Define (Dense, D, Block, Status); Check (Status = Defined);
         D.Identifier := Named ("Tail"); D.Count := Tail_Count;
         Define (Dense, D, Tail, Status); Check (Status = Defined);
         D := (Identifier => Named ("Whole"), Form => Product, Count => 15, others => <>);
         for I in 1 .. 15 loop
            D.Parts (I) := (Named ("f" & Character'Val (Character'Pos ('a') + I - 1)),
              (if I = 15 then Tail else Block));
         end loop;
         Define (Dense, D, Whole, Status); Check (Status = Defined);
         Bind (Dense, Whole, [1, 2, 3, 4], Contract, Good); Check (Good);
         R.Define (Contract, Named ("Snapshot"), Named ("Read"), [5, 6, 7, 8], Definition, Good);
         Check (Good = (Tail_Count = 13));
         if Good then
            Input := Empty (Contract);
            Append (Input, Product_Cell (15), Built); Check (Built = Added);
            for I in 1 .. 15 loop
               Append (Input, Product_Cell (if I = 15 then Tail_Count else 16), Built); Check (Built = Added);
               for J in 1 .. (if I = 15 then Tail_Count else 16) loop
                  Append (Input, Integer_Cell (Integer_64 (J)), Built); Check (Built = Added);
               end loop;
            end loop;
            R.Build (Definition, True, W.Success, 1, Input, Output, Good);
            Is_Choice (R.Found); Check (Output.Used_Cells = Maximum_Cells);
         end if;
      end;
   end loop;
   -- Shared host adapter consumes errors as data without replay or loss of
   -- lifecycle state; wrong expected value type leaves completion untouched.
   declare
      Object : C.Client;
      Sent : C.Submission;
      Done : C.Completion_Result;
      Native : C.Response;
      Reply : CCL.Host_Values.Call_Result;
      State : H.Outcome_State;
      Taken : Boolean;
      Token : Unsigned_64 := 2;
   begin
      CuBit.Memory_Grants.Expected_Pages := W.Creation_Bytes / 4096;
      Prepare (Other_Type, Definition);
      H.Take_Read_Outcome (Object, Definition, Reply, State); Check (State = H.No_Outcome);
      C.Initialize (Object, 5, Good); Check (Good);
      C.Open (Object, "org.cubit.settings", Other_Type, W.Read_Only, 0, 1, Sent); Check (Sent = C.Submitted);
      C.Complete (Object, Answer (1, W.Success, 55), Done); Check (Done = C.Completed);
      H.Take_Read_Outcome (Object, Definition, Reply, State); Check (State = H.Other_Operation);
      C.Take_Result (Object, Native, Taken); Check (Taken);
      declare
         Loan : W.Frame with Import, Address => CuBit.Memory_Grants.Mapping;
      begin
         for Code in W.Status loop
            if W.Valid_Reply (W.Reply (Code, 1), W.Get_Object) then
               C.Get (Object, Token, Sent); Check (Sent = C.Submitted);
               Loan.Value := Empty (Other_Type);
               Append_Text (Loan.Value, "native value", Built); Check (Built = Added);
               C.Complete (Object, Answer (Token, Code, 1), Done); Check (Done = C.Completed);
               H.Take_Read_Outcome (Object, Empty_Definition, Reply, State);
               Check (State = H.Type_Mismatch and C.Status (Object) = C.Result_Ready and not Reply.Success);
               H.Take_Read_Outcome (Object, Definition, Reply, State);
               Check (State = H.Outcome_Ready and Reply.Success and C.Status (Object) = C.Ready);
               Check (Reply.Value.Kind = CCL.Host_Values.Object_Value and then
                 Validate (Reply.Value.Object, R.Schema (Definition)));
               Token := Token + 1;
            end if;
         end loop;
         C.Get (Object, Token, Sent); Check (Sent = C.Submitted);
         Loan.Value := Empty (Other_Type); -- invalid successful object
         C.Complete (Object, Answer (Token, W.Success, 1), Done); Check (Done = C.Completed);
         H.Take_Read_Outcome (Object, Definition, Reply, State);
         Check (State = H.Outcome_Ready and Reply.Success and C.Status (Object) = C.Failed);
         Check (Reply.Value.Object.Cells (1).First = R.Alternative'Enum_Rep (R.Invalid_Completion));
      end;
      C.Retire (Object, Good); Check (Good and IPC.Waits = 0);
   end;
   Ada.Text_IO.Put_Line ("Typed native Config read outcomes: PASS" & Checks'Image & " checks");
end Read_Outcome_Tests;
