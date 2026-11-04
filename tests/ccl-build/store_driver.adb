with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Build_Generations;
with Build_Steps;
with Build_Store;
with Storehouse;
with Storehouse.Hex;
with Storehouse.Profiles;

--  A small real process over the Storehouse, for tests that need several
--  processes or kill one at a chosen system call (tests/ccl-build/
--  crash.sh, race.sh):
--
--    store_driver step STORE SOURCES DELAY
--       runs (or answers) a step with three outputs: SOURCES/three.sh with
--       SOURCES/in.txt, sleeping DELAY seconds first; prints "ran" or "hit"
--       and the three output identities.
--    store_driver generation STORE SOURCES PROFILE DELAY
--       the same step, then its outputs as the profile's next generation.
--    store_driver root STORE SOURCES CHECKOUT
--       the same step, then its outputs as checkout CHECKOUT's last build.
--    store_driver check STORE
--       every action record decodes, names its own step, and names only
--       present entries whose content matches their identity; every entry
--       matches its identity. Prints "consistent" or each problem.
procedure Store_Driver is
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Build_Steps;
   use Storehouse;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);
   function Argument (N : Positive) return String renames Ada.Command_Line.Argument;

   --  The three-output step on SOURCES/in.txt, run or answered; prints
   --  "ran" or "hit" and the outputs. Input: the input's identity.
   procedure Three (Store : Build_Store.Store; Sources, Delay_Text : String; Result : out Outcome;
                    S : out Step; Input : out Digest) is
      Script : Digest;
   begin
      S := (Label => To_Unbounded_String ("three outputs"), Directory => To_Unbounded_String ("work"),
            others => <>);
      Store.Put_File (Sources / "three.sh", Executable_File, Script);
      Store.Put_File (Sources / "in.txt", Regular_File, Input);
      declare
         Tool : constant Input_Number := Add_Input (S, "tools/three", Executable_File, Script);
         Ignored : constant Input_Number := Add_Input (S, "work/in.txt", Regular_File, Input);
      begin
         S.Tool := (Kind => Input_Program, Index => Tool, others => <>);
      end;
      S.Arguments.Append (Text (Delay_Text));
      Declare_Output (S, "work/a.txt");
      Declare_Output (S, "work/b.txt");
      Declare_Output (S, "work/c.txt");
      if Ada.Environment_Variables.Exists ("STORE_DRIVER_SHAPE") then
         --  The step directory's name, for tests that leave stale claims.
         Put_Line ("shape " & Hex.Image (Shape (S)));
         if Ada.Environment_Variables.Value ("STORE_DRIVER_SHAPE") = "only" then
            GNAT.OS_Lib.OS_Exit (0);
         end if;
      end if;
      Run (Store, S, Result);
      Put ((if Result.Ran then "ran" else "hit"));
      for O of Result.Outputs loop
         Put (" " & Hex.Image (O));
      end loop;
      New_Line;
   end Three;

   function Outputs_Of (S : Step; Result : Outcome) return Build_Generations.Item_Vectors.Vector is
      Items : Build_Generations.Item_Vectors.Vector;
   begin
      for N in S.Outputs.First_Index .. S.Outputs.Last_Index loop
         Items.Append (Build_Generations.Item'(S.Outputs (N).Path, Regular_File, Result.Outputs (N)));
      end loop;
      return Items;
   end Outputs_Of;

   procedure Step_Command is
      Store : constant Build_Store.Store := Build_Store.Open (Argument (2), Build_Store.Shared_Use);
      S : Step;
      Input : Digest;
      Result : Outcome;
   begin
      Three (Store, Argument (3), Argument (4), Result, S, Input);
   end Step_Command;

   --  store_driver generation STORE SOURCES PROFILE DELAY: the step's
   --  outputs as the profile's next generation (component "three", the
   --  input as its parameter).
   procedure Generation_Command is
      Store : constant Build_Store.Store := Build_Store.Open (Argument (2), Build_Store.Shared_Use);
      S : Step;
      Input : Digest;
      Result : Outcome;
      Value : Build_Generations.Content;
      Number : Storehouse.Profiles.Generation_Number;
      Added : Boolean;
   begin
      Three (Store, Argument (3), Argument (5), Result, S, Input);
      Value.Parameters.Append (Build_Generations.Item'(To_Unbounded_String ("in.txt"), Regular_File, Input));
      Value.Components.Append (Build_Generations.Component'(To_Unbounded_String ("three"), Outputs_Of (S, Result)));
      Build_Generations.Add (Store, Argument (4), Value, Number, Added);
      Put_Line ((if Added then "generation " else "unchanged ") & Storehouse.Profiles.Image (Number));
   end Generation_Command;

   --  store_driver root STORE SOURCES CHECKOUT: the step's outputs as the
   --  last build of CHECKOUT (an absolute directory).
   procedure Root_Command is
      Store : constant Build_Store.Store := Build_Store.Open (Argument (2), Build_Store.Shared_Use);
      S : Step;
      Input : Digest;
      Result : Outcome;
   begin
      Three (Store, Argument (3), "0", Result, S, Input);
      Store.Put_Root (Argument (4), "three", Build_Generations.Encode_Root
        (Outputs_Of (S, Result), Build_Generations.Item_Vectors.Empty_Vector));
   end Root_Command;

   procedure Check_Command is
      Store : constant Build_Store.Store := Build_Store.Open (Argument (2), Build_Store.Shared_Use);
      Problems : Natural := 0;
      Records : Natural := 0;
      Complete : constant Boolean :=
        Ada.Command_Line.Argument_Count >= 3 and then Argument (3) = "complete";
      Checked : Natural;
      procedure Problem (What : String) is
      begin
         Problems := Problems + 1;
         Put_Line ("inconsistent: " & What);
      end Problem;
      procedure Report (Kind : Entry_Kind; Name : String) is
      begin
         Problem ("entry " & Kind'Image & " " & Name & " does not match its name");
      end Report;
      procedure Verify_All is new Build_Store.Verify (Report);
   begin
      Verify_All (Store, Checked);
      for Name of Store.Names ("actions") loop
         Records := Records + 1;
         declare
            Identity : Digest;
            Outputs : Recorded_Vectors.Vector;
         begin
            Decode_Action (Store.Read_Record ("actions" / Name), Identity, Outputs);
            if Hex.Image (Identity) /= Name then
               Problem ("action " & Name & " names another step");
            end if;
            for O of Outputs loop
               declare
                  Path : constant String := Store.Path_Of (O.Kind, O.Identity);
               begin
                  if not Ada.Directories.Exists (Path) then
                     --  After garbage collection an action record may name
                     --  collected outputs (the step runs again); before any
                     --  collection ("complete") it never may.
                     if Complete then
                        Problem ("action " & Name & " names a missing output " & To_String (O.Path));
                     end if;
                  elsif Build_Store.Hash_File (Path) /= O.Identity then
                     Problem ("action " & Name & " names a damaged output " & To_String (O.Path));
                  end if;
               end;
            end loop;
         exception
            when E : others =>
               Problem ("action " & Name & ": " & Ada.Exceptions.Exception_Message (E));
         end;
      end loop;
      --  Roots: every generation kept and every live root names only
      --  present entries; each profile's pointer names a generation.
      declare
         Generations, Roots : Natural := 0;
         procedure Need (Items : Build_Generations.Item_Vectors.Vector; Where : String) is
         begin
            for I of Items loop
               if not Store.Contains (I.Kind, I.Identity) then
                  Problem (Where & " names a missing entry " & To_String (I.Name));
               end if;
            end loop;
         end Need;
         procedure Root (Name : String; Data : Byte_Array; Live : Boolean) is
            Products, Pins : Build_Generations.Item_Vectors.Vector;
         begin
            if Live then
               Roots := Roots + 1;
               Build_Generations.Decode_Root (Data, Products, Pins);
               Need (Products, "root " & Name);
               Need (Pins, "root " & Name);
            end if;
         end Root;
         procedure Each_Root is new Build_Store.For_Each_Root (Root);
      begin
         for Profile of Build_Generations.Profiles (Store) loop
            declare
               Current : constant Storehouse.Profiles.Generation_Count := Build_Generations.Current (Store, Profile);
               Numbers : constant Build_Generations.Number_Vectors.Vector := Build_Generations.Numbers (Store, Profile);
            begin
               if Current /= 0 and then not Numbers.Contains (Current) then
                  Problem ("profile " & Profile & " points to a missing generation");
               end if;
               for N of Numbers loop
                  Generations := Generations + 1;
                  declare
                     G : constant Build_Generations.Generation := Build_Generations.Read (Store, Profile, N);
                     Where : constant String := "generation " & Profile & "/" & Storehouse.Profiles.Image (N);
                  begin
                     Need (G.Value.Parameters, Where);
                     for C of G.Value.Components loop
                        Need (C.Outputs, Where);
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         Each_Root (Store);
         Put_Line ("generations:" & Generations'Image & ", live roots:" & Roots'Image);
      exception
         when E : others =>
            Problem ("roots: " & Ada.Exceptions.Exception_Message (E));
      end;
      if Problems = 0 then
         Put_Line ("consistent:" & Checked'Image & " entries," & Records'Image & " action records");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Check_Command;

begin
   if Argument (1) = "step" then
      Step_Command;
   elsif Argument (1) = "products" then
      --  store_driver products ROOT-RECORD: what a root record keeps, one
      --  "path kind identity" line per product, then its pins.
      declare
         File : constant String := Argument (2);
         Size : constant Natural := Natural (Ada.Directories.Size (File));
         Data : Byte_Array (1 .. Size);
         Input : Ada.Streams.Stream_IO.File_Type;
         Products, Pins : Build_Generations.Item_Vectors.Vector;
      begin
         Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, File);
         Byte_Array'Read (Ada.Streams.Stream_IO.Stream (Input), Data);
         Ada.Streams.Stream_IO.Close (Input);
         Build_Generations.Decode_Root (Data, Products, Pins);
         for P of Products loop
            Put_Line (To_String (P.Name) & " " & P.Kind'Image & " " & Hex.Image (P.Identity));
         end loop;
         for P of Pins loop
            Put_Line ("pin " & To_String (P.Name) & " " & P.Kind'Image & " " & Hex.Image (P.Identity));
         end loop;
      end;
   elsif Argument (1) = "generation" then
      Generation_Command;
   elsif Argument (1) = "root" then
      Root_Command;
   elsif Argument (1) = "check" then
      Check_Command;
   else
      raise Program_Error with "unknown command " & Argument (1);
   end if;
end Store_Driver;
