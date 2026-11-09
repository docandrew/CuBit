with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with Build_Encoding;
with Build_Platform;
with Build_Timing;
with Storehouse.Paths;

package body Build_Steps is
   use type Interfaces.Unsigned_64;

   Step_Tag : constant String := "cubit.step";
   Step_Version : constant := 1;
   Action_Tag : constant String := "cubit.action";
   Action_Version : constant := 1;
   --  Items of each encoded array: a step is its tag, version, program,
   --  arguments, inputs, outputs, working directory and captured output.
   Step_Fields : constant := 8;
   Action_Fields : constant := 4;
   Input_Fields : constant := 3;
   Output_Fields : constant := 2;
   Piece_Fields : constant := 6;
   Program_Fields : constant := 3;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   function Add_Input (S : in out Step; Path : String; Kind : Entry_Kind; Identity : Digest)
     return Input_Number is
   begin
      S.Inputs.Append (Input'(To_Unbounded_String (Path), Kind, Identity));
      return S.Inputs.Last_Index;
   end Add_Input;

   function Add_Output (S : in out Step; Path : String; Executable : Boolean := False)
     return Output_Number is
   begin
      S.Outputs.Append (Output'(To_Unbounded_String (Path), Executable));
      return S.Outputs.Last_Index;
   end Add_Output;

   procedure Declare_Input (S : in out Step; Path : String; Kind : Entry_Kind; Identity : Digest) is
   begin
      S.Inputs.Append (Input'(To_Unbounded_String (Path), Kind, Identity));
   end Declare_Input;

   procedure Declare_Output (S : in out Step; Path : String; Executable : Boolean := False) is
   begin
      S.Outputs.Append (Output'(To_Unbounded_String (Path), Executable));
   end Declare_Output;

   function Inside (Path, Directory : String) return Boolean is
     (Path'Length > Directory'Length + 1
      and then Path (Path'First .. Path'First + Directory'Length - 1) = Directory
      and then Path (Path'First + Directory'Length) = '/');

   procedure Check (S : Step) is
      Label : constant String := To_String (S.Label);
      procedure Fail (Why : String) is
      begin
         raise Step_Error with Label & ": " & Why;
      end Fail;
      procedure Relative (Path : String; What : String) is
      begin
         if not Storehouse.Paths.Is_Relative (Path) then
            Fail (What & " """ & Path & """ is not a relative path without . or ..");
         end if;
      end Relative;
      Directory : constant String := To_String (S.Directory);
   begin
      Relative (Directory, "working directory");
      for I of S.Inputs loop
         Relative (To_String (I.Path), "input");
      end loop;
      for O of S.Outputs loop
         Relative (To_String (O.Path), "output");
      end loop;
      for A of S.Arguments loop
         if not Storehouse.Paths.Is_Literal (To_String (A.Prefix))
           or else not Storehouse.Paths.Is_Literal (To_String (A.Suffix))
         then
            Fail ("argument text """ & To_String (A.Prefix) & To_String (A.Suffix) &
                  """ names a file; pass files as inputs or outputs");
         end if;
         case A.Kind is
            when Literal | Step_Root => null;
            when Step_Path | Toolchain_Path => Relative (To_String (A.Path), "directory argument");
            when Input_Path | Local_Input =>
               if A.Index not in 1 .. S.Inputs.Last_Index then Fail ("no such input"); end if;
               if A.Kind = Local_Input and then not Inside (To_String (S.Inputs (A.Index).Path), Directory) then
                  Fail ("input " & To_String (S.Inputs (A.Index).Path) & " is not in " & Directory);
               end if;
            when Output_Path | Local_Output =>
               if A.Index not in 1 .. S.Outputs.Last_Index then Fail ("no such output"); end if;
               if A.Kind = Local_Output and then not Inside (To_String (S.Outputs (A.Index).Path), Directory) then
                  Fail ("output " & To_String (S.Outputs (A.Index).Path) & " is not in " & Directory);
               end if;
         end case;
      end loop;
      case S.Tool.Kind is
         when Toolchain_Program => Relative (To_String (S.Tool.Path), "program");
         when Input_Program =>
            if S.Tool.Index not in 1 .. S.Inputs.Last_Index
              or else S.Inputs (S.Tool.Index).Kind /= Executable_File
            then
               Fail ("the program is not an executable input");
            end if;
         when Installed_Program =>
            declare
               Path : constant String := To_String (S.Tool.Path);
            begin
               if Path'Length < 2 or else Path (Path'First) /= '/'
                 or else not Storehouse.Paths.Is_Relative (Path (Path'First + 1 .. Path'Last))
               then
                  Fail ("installed program """ & Path & """ is not an absolute path without . or ..");
               end if;
            end;
      end case;
      if S.Capture /= 0 and then S.Capture not in 1 .. S.Outputs.Last_Index then
         Fail ("unix.stdout is captured to no output");
      end if;
   end Check;

   --  The canonical encoding, with the inputs' content identities (the
   --  step's identity) or without them (its shape).
   function Encoding (S : Step; With_Contents : Boolean) return Digest is
      W : Build_Encoding.Writer;
   begin
      Check (S);
      W.Add_Array (Step_Fields);
      W.Add_Text (Step_Tag);
      W.Add_Unsigned (Step_Version);
      W.Add_Array (Program_Fields);
      W.Add_Unsigned (Program_Kind'Pos (S.Tool.Kind));
      case S.Tool.Kind is
         when Toolchain_Program =>
            W.Add_Digest (S.Tool.Toolchain);
            W.Add_Text (To_String (S.Tool.Path));
         when Input_Program =>
            W.Add_Unsigned (Interfaces.Unsigned_64 (S.Tool.Index));
            W.Add_Text ("");
         when Installed_Program =>
            W.Add_Digest (S.Tool.Contents);
            W.Add_Text (To_String (S.Tool.Path));
      end case;
      W.Add_Array (Natural (S.Arguments.Length));
      for A of S.Arguments loop
         W.Add_Array (Piece_Fields);
         W.Add_Unsigned (Piece_Kind'Pos (A.Kind));
         W.Add_Text (To_String (A.Prefix));
         W.Add_Text (To_String (A.Suffix));
         W.Add_Unsigned (Interfaces.Unsigned_64 (A.Index));
         W.Add_Text (To_String (A.Path));
         W.Add_Digest (A.Tree);
      end loop;
      W.Add_Array (Natural (S.Inputs.Length));
      for I of S.Inputs loop
         W.Add_Array (Input_Fields);
         W.Add_Text (To_String (I.Path));
         W.Add_Unsigned (Entry_Kind'Pos (I.Kind));
         if With_Contents then
            W.Add_Digest (I.Identity);
         else
            W.Add_Bytes ([]);
         end if;
      end loop;
      W.Add_Array (Natural (S.Outputs.Length));
      for O of S.Outputs loop
         W.Add_Array (Output_Fields);
         W.Add_Text (To_String (O.Path));
         W.Add_Boolean (O.Executable);
      end loop;
      W.Add_Text (To_String (S.Directory));
      W.Add_Unsigned (Interfaces.Unsigned_64 (S.Capture));
      return W.Identity;
   end Encoding;

   function Identity (S : Step) return Digest is (Encoding (S, With_Contents => True));
   function Shape (S : Step) return Digest is (Encoding (S, With_Contents => False));

   function Output_Kind (O : Output) return Entry_Kind is
     (if O.Executable then Executable_File else Regular_File);

   function Action_Record (Step_Identity : Digest; S : Step; Outputs : Digest_Vectors.Vector)
     return Byte_Array
   is
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (Action_Fields);
      W.Add_Text (Action_Tag);
      W.Add_Unsigned (Action_Version);
      W.Add_Digest (Step_Identity);
      W.Add_Array (Natural (S.Outputs.Length));
      for N in S.Outputs.First_Index .. S.Outputs.Last_Index loop
         W.Add_Array (Input_Fields);
         W.Add_Text (To_String (S.Outputs (N).Path));
         W.Add_Unsigned (Entry_Kind'Pos (Output_Kind (S.Outputs (N))));
         W.Add_Digest (Outputs (N));
      end loop;
      return W.Bytes;
   end Action_Record;

   procedure Decode_Action (Data : Byte_Array; Step_Identity : out Digest; Outputs : out Recorded_Vectors.Vector) is
      R : Build_Encoding.Reader := (Length => Data'Length, Data => Data, Position => 1);
      Count : Natural;
   begin
      Outputs.Clear;
      if Build_Encoding.Read_Array (R) /= Action_Fields
        or else Build_Encoding.Read_Text (R) /= Action_Tag
        or else Build_Encoding.Read_Unsigned (R) /= Action_Version
      then
         raise Build_Encoding.Decode_Error with "not an action record";
      end if;
      Step_Identity := Build_Encoding.Read_Digest (R);
      Count := Build_Encoding.Read_Array (R);
      for N in 1 .. Count loop
         if Build_Encoding.Read_Array (R) /= Input_Fields then
            raise Build_Encoding.Decode_Error with "not an output";
         end if;
         declare
            Path : constant String := Build_Encoding.Read_Text (R);
            Kind : constant Interfaces.Unsigned_64 := Build_Encoding.Read_Unsigned (R);
         begin
            if Kind > Entry_Kind'Pos (Executable_File) then
               raise Build_Encoding.Decode_Error with "an output is not a file";
            end if;
            Outputs.Append (Recorded_Output'(To_Unbounded_String (Path), Entry_Kind'Val (Kind), Build_Encoding.Read_Digest (R)));
         end;
      end loop;
      if not Build_Encoding.At_End (R) then
         raise Build_Encoding.Decode_Error with "bytes after the action record";
      end if;
   end Decode_Action;

   --  The outputs an action record names, if it is this step's record and
   --  every output entry is present; otherwise Found is False.
   procedure Recall
     (Store : Build_Store.Store; Step_Identity : Digest; S : Step;
      Outputs : out Digest_Vectors.Vector; Found : out Boolean)
   is
      Timed : Build_Timing.Scope (Build_Timing.Recall);
      pragma Unreferenced (Timed);
   begin
      Outputs.Clear;
      Found := False;
      if not Store.Has_Action (Step_Identity) then
         return;
      end if;
      declare
         Data : constant Byte_Array := Store.Get_Action (Step_Identity);
         R : Build_Encoding.Reader := (Length => Data'Length, Data => Data, Position => 1);
      begin
         if Build_Encoding.Read_Array (R) /= Action_Fields
           or else Build_Encoding.Read_Text (R) /= Action_Tag
           or else Build_Encoding.Read_Unsigned (R) /= Action_Version
           or else Build_Encoding.Read_Digest (R) /= Step_Identity
           or else Build_Encoding.Read_Array (R) /= Natural (S.Outputs.Length)
         then
            raise Build_Encoding.Decode_Error with "not this step's action record";
         end if;
         for O of S.Outputs loop
            if Build_Encoding.Read_Array (R) /= Input_Fields
              or else Build_Encoding.Read_Text (R) /= To_String (O.Path)
              or else Build_Encoding.Read_Unsigned (R) /= Entry_Kind'Pos (Output_Kind (O))
            then
               raise Build_Encoding.Decode_Error with "action record outputs differ";
            end if;
            declare
               Output_Identity : constant Digest := Build_Encoding.Read_Digest (R);
            begin
               if not Store.Contains (Output_Kind (O), Output_Identity) then
                  return;   --  collected: run again
               end if;
               Outputs.Append (Output_Identity);
            end;
         end loop;
         if not Build_Encoding.At_End (R) then
            raise Build_Encoding.Decode_Error with "bytes after the action record";
         end if;
         Found := True;
      exception
         when E : Build_Encoding.Decode_Error =>
            --  Never trusted: moved aside, so the step runs and records anew.
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
              "ccl-build: " & To_String (S.Label) & ": damaged action record (" &
              Ada.Exceptions.Exception_Message (E) & ")");
            Outputs.Clear;
            Store.Quarantine_Action (Step_Identity);
      end;
   end Recall;

   procedure Make_Parent (Path : String) is
   begin
      Ada.Directories.Create_Path (Ada.Directories.Containing_Directory (Path));
   end Make_Parent;

   --  Recreates a store tree at Target: directories made, files linked.
   procedure Place_Tree (Source, Target : String) is
      use Ada.Directories;
      Search : Search_Type;
      Item : Directory_Entry_Type;
   begin
      Create_Path (Target);
      Build_Timing.Count (Build_Timing.Directories_Made);
      Start_Search (Search, Source, "");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Item);
         declare
            Name : constant String := Simple_Name (Item);
            From : constant String := Source / Name;
            To : constant String := Target / Name;
         begin
            if Name = "." or else Name = ".." then
               null;
            elsif Build_Platform.Is_Symbolic_Link (From) then
               Build_Platform.Symbolic_Link (Build_Platform.Link_Target (From), To);
            elsif GNAT.OS_Lib.Is_Directory (From) then
               Place_Tree (From, To);
            else
               Build_Platform.Link (From, To);
            end if;
         end;
      end loop;
      End_Search (Search);
   end Place_Tree;

   function Render (Store : Build_Store.Store; S : Step; Root : String; A : Piece) return String is
      Directory : constant String := To_String (S.Directory);
      function Local (Path : String) return String is
        (Path (Path'First + Directory'Length + 1 .. Path'Last));
      Middle : constant String :=
        (case A.Kind is
           when Literal => "",
           when Input_Path => Root / To_String (S.Inputs (A.Index).Path),
           when Output_Path => Root / To_String (S.Outputs (A.Index).Path),
           when Local_Input => Local (To_String (S.Inputs (A.Index).Path)),
           when Local_Output => Local (To_String (S.Outputs (A.Index).Path)),
           when Step_Path => Root / To_String (A.Path),
           when Toolchain_Path => Store.Path_Of (Tree, A.Tree) / To_String (A.Path),
           when Step_Root => Root);
   begin
      return To_String (A.Prefix) & Middle & To_String (A.Suffix);
   end Render;

   function Program_Path (Store : Build_Store.Store; S : Step; Root : String) return String is
     (case S.Tool.Kind is
        when Toolchain_Program => Store.Path_Of (Tree, S.Tool.Toolchain) / To_String (S.Tool.Path),
        when Input_Program => Root / To_String (S.Inputs (S.Tool.Index).Path),
        when Installed_Program => To_String (S.Tool.Path));

   function Command_Image (Store : Build_Store.Store; S : Step; Root : String) return String is
      Result : Unbounded_String := To_Unbounded_String (Program_Path (Store, S, Root));
   begin
      for A of S.Arguments loop
         Append (Result, " " & Render (Store, S, Root, A));
      end loop;
      if S.Capture /= 0 then
         Append (Result, " > " & To_String (S.Outputs (S.Capture).Path));
      end if;
      return To_String (Result);
   end Command_Image;

   --  Runs S in its claimed directory Root and stores its outputs. On a
   --  failure the directory is kept for inspection (the claim is released
   --  by the caller).
   procedure Execute
     (Store : Build_Store.Store; S : Step; Root : String; Outputs : out Digest_Vectors.Vector; Failed : out Boolean)
   is
      use Interfaces.C.Strings;
      Arguments : Build_Platform.Argument_List (1 .. Natural (S.Arguments.Length) + 1);
      Status : Integer;
   begin
      Outputs.Clear;
      Failed := True;
      declare
         Timed : Build_Timing.Scope (Build_Timing.Place_Inputs);
         pragma Unreferenced (Timed);
      begin
         for I of S.Inputs loop
            declare
               Target : constant String := Root / To_String (I.Path);
            begin
               case I.Kind is
                  when Tree => Place_Tree (Store.Path_Of (Tree, I.Identity), Target);
                  when others =>
                     Make_Parent (Target);
                     Build_Platform.Link (Store.Path_Of (I.Kind, I.Identity), Target);
               end case;
            end;
         end loop;
         Ada.Directories.Create_Path (Root / To_String (S.Directory));
         for O of S.Outputs loop
            Make_Parent (Root / To_String (O.Path));
         end loop;
      end;
      Arguments (1) := New_String (Program_Path (Store, S, Root));
      for N in S.Arguments.First_Index .. S.Arguments.Last_Index loop
         Arguments (N + 1) := New_String (Render (Store, S, Root, S.Arguments (N)));
      end loop;
      Build_Platform.Run
        (Program => Program_Path (Store, S, Root), Arguments => Arguments,
         Directory => Root / To_String (S.Directory),
         Standard_Output => (if S.Capture = 0 then "" else Root / To_String (S.Outputs (S.Capture).Path)),
         Exit_Status => Status);
      for A of Arguments loop
         Free (A);
      end loop;
      if Status /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "failed: " & Command_Image (Store, S, Root));
         raise Step_Error with To_String (S.Label) & ": exit status" & Status'Image &
           " (step directory kept until the next build: " & Root & ")";
      end if;
      for O of S.Outputs loop
         declare
            Produced : constant String := Root / To_String (O.Path);
            Stored : Digest;
         begin
            if Build_Platform.Is_Symbolic_Link (Produced) or else not GNAT.OS_Lib.Is_Regular_File (Produced) then
               raise Step_Error with To_String (S.Label) & ": did not produce " & To_String (O.Path);
            end if;
            declare
               Timed : Build_Timing.Scope (Build_Timing.Output_Hash);
               pragma Unreferenced (Timed);
            begin
               Store.Put_File (Produced, Output_Kind (O), Stored);
            end;
            Outputs.Append (Stored);
         end;
      end loop;
      Failed := False;
   end Execute;

   procedure Run (Store : Build_Store.Store; S : Step; Result : out Outcome) is
      Found : Boolean;
      Place : constant Digest := Shape (S);
      Failed : Boolean := True;
      Released : Boolean := False;
   begin
      Result := (Step_Identity => Identity (S), others => <>);
      Recall (Store, Result.Step_Identity, S, Result.Outputs, Found);
      if Found then
         return;
      end if;
      declare
         Root : constant String := Store.Claim_Step (Place, To_String (S.Label));
      begin
         --  Another build may have run the step while this one waited.
         Recall (Store, Result.Step_Identity, S, Result.Outputs, Found);
         if Found then
            Released := True;
            Store.Release_Step (Place);
            return;
         end if;
         Execute (Store, S, Root, Result.Outputs, Failed);
         Result.Ran := True;
         --  Recorded before the claim is released, so a build waiting for
         --  the step finds the record rather than running it again.
         Store.Put_Action
           (Result.Step_Identity, Action_Record (Result.Step_Identity, S, Result.Outputs),
            Result.Not_Reproducible);
         Released := True;
         Store.Release_Step (Place);
      exception
         when others =>
            if not Released then
               Store.Release_Step (Place, Keep => Failed);
            end if;
            raise;
      end;
   end Run;

end Build_Steps;
