with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Copy_Store_Collecting_Tests is
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type Bytes;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   type Scenario is (Copy_Local, Store_Local, Copy_Named, Store_Package);
   type Source_Mode is (Inline_Buffer, Retained_Buffer);
   Checks : Natural := 0;
   Input : aliased AML_Table_Backing.State (1, 1);
   Buffer_Value : constant Bytes := [16#11#,7,16#0A#,4,16#55#,16#AA#,16#00#,16#FF#];
   Package_Value : constant Bytes := [16#12#,3,1,0];
   function Enc (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin
      for I in B'Range loop B (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return B;
   end Enc;
   function Method (Text : String; Arity : Byte; Body_Code : Bytes) return Bytes is
     ([16#14#, Byte (Body_Code'Length + 6)] & Enc (Text) & [Arity] & Body_Code);
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
begin
   for Width in Integer_Width loop
      for Mode in Scenario loop
         for Source in Source_Mode loop
            declare
               A : C.Arena;
               Accessed : C.Access_Status;
               Loaded : N.Load_Status;
               Members : N.Initialization_Report;
               Outcome : C.Result;
               Original, Pack, Garbage, Result_Handle : C.Value_Handle := C.No_Value;
               Before : C.Collection_Statistics;
               Operand : constant Bytes :=
                 (if Source = Inline_Buffer then Buffer_Value else [16#68#]);
               Target : constant Bytes :=
                 (case Mode is
                    when Copy_Local | Store_Local => [16#60#],
                    when Copy_Named => Enc ("DEST"),
                    when Store_Package => [16#88#,16#69#,0,0]);
               Returned : constant Bytes :=
                 (case Mode is
                    when Copy_Local | Store_Local => [16#60#],
                    when Copy_Named => Enc ("DEST"),
                    when Store_Package => [16#83#,16#88#,16#69#,0,0]);
               Code : constant Bytes := [16#08#] & Enc ("DEST") & Buffer_Value &
                 Method ("MAKE", 0, [16#A4#] & Buffer_Value) &
                 Method ("PACK", 0, [16#A4#] & Package_Value) &
                 Method ("TEST", 2,
                   [Byte'(if Mode in Copy_Local | Copy_Named then 16#9D# else 16#70#)] &
                   Operand & Target & [16#A4#] & Returned);
               procedure Invoke (Name : String; Count : C.Argument_Count := 0) is
                  Node : N.Node_ID;
                  Args : C.Arguments := [others => (C.Immediate_Argument, 0)];
               begin
                  C.Find_Child (A, N.Root, Name, Node, Accessed);
                  Check (Accessed = C.Available and then Node /= N.Root, "method exists");
                  if Count /= 0 then
                     Args (0) := (C.Retained_Argument, Original);
                     Args (1) := (C.Retained_Argument, Pack);
                  end if;
                  C.Invoke (A, Input, Node, Args, Count, 1000, Outcome, Accessed);
                  Check (Accessed = C.Available and then Outcome.Status = AML_Execute.Object_Returned,
                    Mode'Image & Source'Image & Outcome.Status'Image);
               end Invoke;
               procedure Release (Handle : in out C.Value_Handle) is
               begin C.Release (A, Handle, Accessed); Check (Accessed = C.Available, "release"); end Release;
               procedure Full_Bytes (Handle : C.Value_Handle) is
                  Description : C.Value_Description;
                  Data : Bytes (1 .. 4);
                  Copied : Natural;
               begin
                  C.Describe (A, Handle, Description, Accessed);
                  Check (Accessed = C.Available and then Description.Kind = C.Buffer_Description
                    and then Description.Length = 4, "full buffer description");
                  C.Read_Bytes (A, Handle, 0, Data, Copied, Accessed);
                  Check (Accessed = C.Available and then Copied = 4
                    and then Data = [16#55#,16#AA#,16#00#,16#FF#], "all buffer bytes survive safepoint");
               end Full_Bytes;
            begin
               C.Initialize (A, Accessed); Check (Accessed = C.Available, "initialize");
               C.Load (A, Code, Width, Loaded, Accessed);
               Check (Accessed = C.Available and then Loaded = N.Loaded, "load");
               C.Seal (A, Members, Accessed); Check (Accessed = C.Available, "seal");
               Invoke ("MAKE"); Original := Outcome.Handle;
               Invoke ("PACK"); Pack := Outcome.Handle;
               Invoke ("MAKE"); Garbage := Outcome.Handle; Release (Garbage);
               Before := C.Reclamation_Metrics (A);
               Invoke ("TEST", 2); Result_Handle := Outcome.Handle;
               -- No allocation besides the inline literal (when selected) and
               -- the target adapter occurs in TEST. The retained-input case
               -- isolates exactly the Clone/Copy/Store collection safepoint.
               Check (C.Reclamation_Metrics (A).Attempted - Before.Attempted =
                 (if Source = Inline_Buffer then 2 else 1), "exact allocation hook count");
               Check (C.Reclamation_Metrics (A).Completed - Before.Completed =
                 (if Source = Inline_Buffer then 2 else 1), "exact successful collection count");
               Check (C.Reclamation_Metrics (A).Rejected = Before.Rejected,
                 "no rejected owner census");
               Check (C.Reclamation_Metrics (A).Freed_Bytes > Before.Freed_Bytes,
                 "real reclamation, not hook invocation alone");
               Full_Bytes (Result_Handle); Full_Bytes (Original);
               Release (Result_Handle); Release (Original); Release (Pack);
               Check (C.Retained_Count (A) = 0, "all public roots released");
               pragma Assert (C.Module_Transient_Free (A));
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("COPY STORE COLLECTING" & Checks'Image);
end Copy_Store_Collecting_Tests;
