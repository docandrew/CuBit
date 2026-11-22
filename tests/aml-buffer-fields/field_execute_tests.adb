with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Integers;
with AML_Namespace;
with AML_Table_Backing;
procedure Field_Execute_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (32, Max_Retained_Roots => 8,
     Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type N.Load_Status;
   use type Integer_Value;
   use type C.Access_Status;
   use type C.Collection_Count;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant C.Arguments := [others => (C.Immediate_Argument, 0)];
   Outcome : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Checks : Natural := 0;
   Raw : constant Bytes := [16#41#,16#42#,16#43#,16#44#,16#45#,16#46#,16#47#,16#48#];
   Buffer_Code : constant Bytes := Bytes'(16#11#,11,16#0A#,8) & Raw;
   Field_Name : constant Bytes := [70,76,68,48];
   Return_Field : constant Bytes := Bytes'(1 => 16#A4#) & Field_Name;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop; return Data; end Name;
   function Method (Text : String; Code : Bytes; Flags : Byte := 0) return Bytes is
   begin
      if Code'Length > 57 then raise Program_Error with "method requires multibyte PkgLength"; end if;
      return Bytes'(16#14#, Byte (Code'Length + 6)) & Name (Text) & Bytes'(1 => Flags) & Code;
   end Method;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Outcome.Status'Image; end if; end Check;
   procedure Load (Code : Bytes; Width : Integer_Width; Extra : Bytes := []) is
   begin
      C.Reset (A, Status); Check (Status = C.Available);
      C.Load (A, Method ("TEST", Code) & Extra, Width, Loaded, Status);
      Check (Status = C.Available and then Loaded = N.Loaded);
      C.Seal (A, Report, Status); Check (Status = C.Available);
   end Load;
   procedure Invoke (Budget : Natural := 100) is
   begin
      C.Invoke (A, Input, 1, Args, 0, Budget, Outcome, Status);
      Check (Status = C.Available and then Outcome.Charged <= Budget);
   end Invoke;
   procedure Verify_Bytes (Expected : Bytes) is
      Data : Bytes (1 .. 16); Copied : Natural;
      H : C.Value_Handle := Outcome.Handle;
   begin
      C.Read_Bytes (A, H, 0, Data, Copied, Status);
      Check (Status = C.Available and then Copied = Expected'Length);
      Check (Data (1 .. Copied) = Expected);
      C.Release (A, H, Status); Check (Status = C.Available);
   end Verify_Bytes;
   Fixed_Ops : constant Bytes := [Create_Bit_Field_Op, Create_Byte_Field_Op,
     Create_Word_Field_Op, Create_DWord_Field_Op, Create_QWord_Field_Op];
   Fixed_Values : constant array (1 .. 5) of Integer_Value :=
     [1,16#41#,16#4241#,16#44434241#,16#4847464544434241#];
begin
   for Width in Integer_Width loop
      for I in Fixed_Ops'Range loop
         declare Decl : constant Bytes := Bytes'(1 => Fixed_Ops (I)) & Buffer_Code & Bytes'(1 => 0) & Field_Name; begin
            Load (Decl & Return_Field, Width); Invoke;
            if I = 5 and then Width = Bits_32 then
               Check (Outcome.Status = Object_Returned); Verify_Bytes (Raw);
            else
               Check (Outcome.Status = Returned and then Outcome.Number = Fixed_Values (I));
            end if;
            Check (Outcome.Charged = 5);
            for Budget in 0 .. 4 loop
               Load (Decl & Return_Field, Width); Invoke (Budget);
               Check (Outcome.Status = Budget_Exceeded and then Outcome.Charged = Budget);
            end loop;
            for Length in 1 .. Decl'Length - 1 loop
               Load (Decl (Decl'First .. Decl'First + (Length - 1)), Width); Invoke;
               Check (Outcome.Status not in Returned | Object_Returned | Reference_Returned | No_Return);
            end loop;
         end;
      end loop;
      declare Decl : constant Bytes := Bytes'(16#5B#,Create_Field_Extension) & Buffer_Code & Bytes'(0,1) & Field_Name; begin
         Load (Decl & Return_Field, Width); Invoke;
         Check (Outcome.Status = Object_Returned and then Outcome.Charged = 6); Verify_Bytes ([1]);
         Load (Decl & Bytes'(16#A4#,16#8E#) & Field_Name, Width); Invoke;
         Check (Outcome.Status = Returned and then Outcome.Number = 14);
         for Budget in 0 .. 5 loop
            Load (Decl & Return_Field, Width); Invoke (Budget);
            Check (Outcome.Status = Budget_Exceeded and then Outcome.Charged = Budget);
         end loop;
         for Length in 1 .. Decl'Length - 1 loop
            Load (Decl (Decl'First .. Decl'First + (Length - 1)), Width); Invoke;
            Check (Outcome.Status not in Returned | Object_Returned | Reference_Returned | No_Return);
         end loop;
      end;
      Load (Bytes'(16#5B#,Create_Field_Extension) & Buffer_Code & Bytes'(0,0) & Field_Name & Return_Field, Width);
      Invoke; Check (Outcome.Status = Invalid_Field_Length);
      Load (Bytes'(1 => Create_Byte_Field_Op) & Buffer_Code & Bytes'(16#0A#,8) & Field_Name & Return_Field, Width);
      Invoke; Check (Outcome.Status = Buffer_Field_Limit);
      -- Later index method allocates while the anonymous source must stay live.
      Load (Bytes'(1 => Create_Byte_Field_Op) & Buffer_Code & Name ("NEXT") & Field_Name & Return_Field,
        Width, Method ("NEXT", [16#70#,16#0D#,88,0,16#60#,16#A4#,0]));
      declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
         Invoke; Check (Outcome.Status = Returned and then Outcome.Number = 16#41#);
         Check (C.Reclamation_Metrics (A).Completed > Before);
      end;
      -- Eager named field reads allocate distinct Buffers; the saved left
      -- expression must survive collection for the right field read.
      declare
         First : constant Bytes := Bytes'(16#5B#,Create_Field_Extension) & Buffer_Code & Bytes'(0,16#0A#,8) & Name ("FLD1");
         Second : constant Bytes := Bytes'(16#5B#,Create_Field_Extension) & Buffer_Code & Bytes'(0,16#0A#,8) & Name ("FLD2");
      begin
         Load (First & Second & Bytes'(16#A4#,16#93#) & Name ("FLD1") & Name ("FLD2"), Width);
         declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
            Invoke; Check (Outcome.Status = Returned and then Outcome.Number = AML_Integers.Normalize (Integer_Value'Last, Width));
            Check (C.Reclamation_Metrics (A).Completed >= Before + 4);
         end;
      end;
      -- CopyObject source is a fresh field Buffer. Argument validation reads
      -- another field and collects while the source must remain in expression roots.
      declare
         One_Buffer : constant Bytes := [16#11#, 3, 1, 16#41#];
         Target_Buffer : constant Bytes := [16#11#, 3, 1, 16#42#];
         Source_Name : constant Bytes := Bytes'(1 => 16#5C#) & Name ("FLD1");
         Target_Name : constant Bytes := Bytes'(1 => 16#5C#) & Name ("FLD2");
         Source_Field : constant Bytes := Bytes'(16#5B#, Create_Field_Extension)
           & One_Buffer & Bytes'(0, 16#0A#, 8) & Source_Name;
         Target_Field : constant Bytes := Bytes'(16#5B#, Create_Field_Extension)
           & Target_Buffer & Bytes'(0, 16#0A#, 8) & Target_Name;
         Main_Code : constant Bytes := Source_Field & Target_Field
           & Bytes'(1 => 16#A4#) & Name ("HELP")
           & Bytes'(1 => 16#71#) & Target_Name;
         Help_Code : constant Bytes := Bytes'(1 => 16#9D#) & Source_Name
           & Bytes'(16#68#, 16#A4#) & Target_Name;
      begin
         Load (Main_Code, Width, Method ("HELP", Help_Code, Flags => 1));
         declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
            Invoke;
            Check (Outcome.Status = Object_Returned);
            Verify_Bytes ([16#41#]);
            Check (C.Reclamation_Metrics (A).Completed > Before);
         end;
      end;
      -- An ordinary Arg cell receives the copied Index reference itself.
      for Shape in 0 .. 1 loop
         declare
            Source_Code : constant Bytes :=
              (if Shape = 0 then Bytes'(16#11#,3,1,65)
               else Bytes'(16#12#,4,1,16#0A#,65));
            Help_Code : constant Bytes := Bytes'(16#9D#,16#88#) & Source_Code
              & Bytes'(0,0,16#68#,16#A4#,16#83#,16#68#);
         begin
            Load (Bytes'(1 => 16#A4#) & Name ("HELP") & Bytes'(1 => 0),
              Width, Method ("HELP", Help_Code, Flags => 1));
            Invoke;
            Check (Outcome.Status = Returned and then Outcome.Number = 65);
         end;
      end loop;
      -- The named copied wrapper is the sole owner of an anonymous Index
      -- backing object after the copy statement's expression roots are cleared.
      for Shape in 0 .. 1 loop
         declare
            Source_Code : constant Bytes :=
              (if Shape = 0 then Bytes'(16#11#,3,1,65)
               else Bytes'(16#12#,4,1,16#0A#,65));
            Main_Code : constant Bytes := Bytes'(16#9D#,16#88#) & Source_Code
              & Bytes'(0,0) & Name ("DST0") & Bytes'(16#A4#,0);
            Read_Code : constant Bytes :=
              Bytes'(16#70#,16#11#,3,1,66,16#60#,16#A4#,16#83#) & Name ("DST0");
         begin
            Load (Main_Code, Width, Bytes'(1 => 16#08#) & Name ("DST0")
              & Bytes'(1 => 0) & Method ("READ", Read_Code));
            Invoke;
            Check (Outcome.Status = Returned and then Outcome.Number = 0);
            -- The original invocation and every one of its expression roots
            -- have ended before this distinct invocation forces collection.
            declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
               C.Invoke (A, Input, 3, Args, 0, 100, Outcome, Status);
               Check (Status = C.Available and then Outcome.Status = Returned and then Outcome.Number = 65);
               Check (C.Reclamation_Metrics (A).Completed > Before);
            end;
         end;
      end loop;
      -- Explicit indexed CopyObject sources are invalid for field targets.
      -- Inspect the persistent backing after failure to detect accidental writes.
      for Generic_Field in Boolean loop
         for Shape in 0 .. 2 loop
            declare
               Source_Code : constant Bytes :=
                 (if Shape = 0 then Bytes'(16#11#,3,1,66)
                  elsif Shape = 1 then Bytes'(16#12#,4,1,16#0A#,66)
                  else Bytes'(16#12#,2,1));
               Field_Code : constant Bytes :=
                 (if Generic_Field then Bytes'(16#5B#,Create_Field_Extension)
                    & Name ("BUFF") & Bytes'(0,16#0A#,8) & Name ("FLD0")
                  else Bytes'(1 => Create_Byte_Field_Op)
                    & Name ("BUFF") & Bytes'(1 => 0) & Name ("FLD0"));
               Main_Code : constant Bytes := Field_Code & Bytes'(16#9D#,16#88#)
                 & Source_Code & Bytes'(0,0) & Name ("FLD0");
               Extra : constant Bytes := Bytes'(1 => 16#08#) & Name ("BUFF")
                 & Bytes'(16#11#,3,1,42)
                 & Method ("READ", Bytes'(1 => 16#A4#) & Name ("BUFF"));
            begin
               Load (Main_Code, Width, Extra); Invoke;
               Check (Outcome.Status = Unsupported_Value);
               C.Invoke (A, Input, 3, Args, 0, 100, Outcome, Status);
               Check (Status = C.Available and then Outcome.Status = Object_Returned);
               Verify_Bytes ([42]);
            end;
         end loop;
      end loop;
      Check (C.Retained_Count (A) = 0 and then C.Reclamation_Metrics (A).Rejected = 0);
   end loop;
   Ada.Text_IO.Put_Line ("Field evaluator checks" & Checks'Image);
end Field_Execute_Tests;
