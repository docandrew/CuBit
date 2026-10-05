--  binutils on CuBit through typed parameters (docs/ccl-launch-parameters.md):
--  asks procmgr what as.app and ld.app take, fills in typed values, renders
--  them into argv and delegated places (CuBit.Program_Descriptions), launches
--  the tools, and has binutils-compare check their outputs against Linux's.
--  The tools hold no file scopes of their own: each touches exactly the
--  files a call names. Results go to the kernel console.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Program_Descriptions;

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type LA.Launch_Failure;
   use type PP.Check_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("binutils-check: " & Text & (if Pass then " PASS" else " FAIL") &
                  ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  A tool's signature, from procmgr.
   procedure Describe (Program : String; S : out PP.Signature; Ok : out Boolean) is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
   begin
      CuBit.Launching.Describe (Program, Descriptor, Length, Result, Failure);
      Ok := Result = CuBit.Launching.Launched and then Length > 0;
      if Ok then
         PP.Decode (Descriptor (1 .. Length), S, Ok);
      else
         PP.Decode (Descriptor (1 .. 0), S, Ok);
         Ok := False;
      end if;
   end Describe;

   --  Add a value by parameter name.
   procedure Set (S : PP.Signature; V : in out PP.Values; Name, Value : String) is
      Index : PP.Parameter_Index;
      Found, Added : Boolean;
   begin
      PP.Find (S, Name, Index, Found);
      if Found then
         PP.Add (V, Index, Value, Added);
      end if;
      if not Found or else not Added then
         Say ("set " & Name, False);
      end if;
   end Set;

   --  Render V for Program, launch it and wait: True when it exits with 0.
   procedure Run (Program : String; S : PP.Signature; V : PP.Values;
                  Checked : out PP.Check_Result; Launched : out CuBit.Launching.Launch_Result;
                  Failure : out LA.Launch_Failure; Passed : out Boolean) is
      Block : LA.Builder;
      Grants : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Child : CuBit.Launching.Child;
      Ended : CuBit.Child_Exits.Report;
   begin
      Passed := False;
      Launched := CuBit.Launching.Refused;
      Failure := LA.Malformed_Request;
      PP.Render (S, V, Program, Block, Grants, Checked);
      if Checked /= PP.Matches then
         return;
      end if;
      LG.Finish (Grants, Region, Region_Length);
      CuBit.Launching.Launch
        (Program, Block.Data (1 .. Block.Used), Region (1 .. Region_Length),
         Child, Launched, Failure);
      if Launched = CuBit.Launching.Launched then
         CuBit.Launching.Wait (Child, Ended);
         Passed := Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0;
      end if;
   end Run;

   --  binutils-compare A B: exits 0 when the files are identical.
   function Same (A, B : String) return Boolean is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Accepted : Boolean;
      Child : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Ended : CuBit.Child_Exits.Report;
      None : constant LG.Bytes (1 .. 0) := [others => 0];
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, "binutils-compare.app", Accepted);
      LA.Add_Argument (Block, A, Accepted);
      LA.Add_Argument (Block, B, Accepted);
      LA.Finish (Block, Length, Accepted);
      CuBit.Launching.Launch
        ("binutils-compare.app", Block.Data (1 .. Length), None, Child, Result, Failure);
      if Result /= CuBit.Launching.Launched then
         return False;
      end if;
      CuBit.Launching.Wait (Child, Ended);
      return Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0;
   end Same;

   As_Signature, Ld_Signature : PP.Signature;
   V : PP.Values;
   Ok, Passed : Boolean;
   Checked : PP.Check_Result;
   Launched : CuBit.Launching.Launch_Result;
   Failure : LA.Launch_Failure;
begin
   Describe ("as.app", As_Signature, Ok);
   Say ("procmgr describes as.app", Ok and then As_Signature.Parameter_Total = 2);
   Describe ("ld.app", Ld_Signature, Ok);
   Say ("procmgr describes ld.app", Ok and then Ld_Signature.Parameter_Total = 5);

   PP.Clear (V);
   Set (As_Signature, V, "output", "@nvme:0/work/hello.o");
   Set (As_Signature, V, "source", "@nvme:0/work/hello.s");
   Run ("as.app", As_Signature, V, Checked, Launched, Failure, Passed);
   Say ("as assembles hello.s", Passed);
   Say ("as output matches Linux", Same ("@nvme:0/work/hello.o", "@nvme:0/work/expected-hello.o"));

   PP.Clear (V);
   Set (Ld_Signature, V, "output", "@nvme:0/work/hello.elf");
   Set (Ld_Signature, V, "inputs", "@nvme:0/work/hello.o");
   Set (Ld_Signature, V, "static", "");
   Run ("ld.app", Ld_Signature, V, Checked, Launched, Failure, Passed);
   Say ("ld links hello.o", Passed);
   Say ("ld output matches Linux", Same ("@nvme:0/work/hello.elf", "@nvme:0/work/expected-hello.elf"));

   --  The typing refuses a call before anything starts.
   PP.Clear (V);
   Set (Ld_Signature, V, "inputs", "@nvme:0/work/hello.o");
   Run ("ld.app", Ld_Signature, V, Checked, Launched, Failure, Passed);
   Say ("ld without an output is refused (Missing_Parameter)",
        Checked = PP.Missing_Parameter);
   --  A file the launcher does not hold cannot be handed over.
   PP.Clear (V);
   Set (As_Signature, V, "output", "@nvme:0/elsewhere/hello.o");
   Set (As_Signature, V, "source", "@nvme:0/work/hello.s");
   Run ("as.app", As_Signature, V, Checked, Launched, Failure, Passed);
   Say ("an output outside the launcher's places is refused (Not_Granted)",
        Launched = CuBit.Launching.Refused and then Failure = LA.Not_Granted);
   --  Without its typed parameters, as touches nothing: argv is data.
   declare
      Block : LA.Builder;
      Length : LA.Present_Length;
      Accepted : Boolean;
      Child : CuBit.Launching.Child;
      Ended : CuBit.Child_Exits.Report;
      None : constant LG.Bytes (1 .. 0) := [others => 0];
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, "as.app", Accepted);
      LA.Add_Argument (Block, "-o", Accepted);
      LA.Add_Argument (Block, "@nvme:0/work/stray.o", Accepted);
      LA.Add_Argument (Block, "@nvme:0/work/hello.s", Accepted);
      LA.Finish (Block, Length, Accepted);
      CuBit.Launching.Launch ("as.app", Block.Data (1 .. Length), None, Child, Launched, Failure);
      Passed := False;
      if Launched = CuBit.Launching.Launched then
         CuBit.Launching.Wait (Child, Ended);
         Passed := not (Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0);
      end if;
      Say ("as with raw argv and no places fails", Passed);
   end;

   debugPrint ((if Failed then "BINUTILS-CHECK: FAIL" else "BINUTILS-CHECK: PASS") &
               ASCII.LF);
end Main;
