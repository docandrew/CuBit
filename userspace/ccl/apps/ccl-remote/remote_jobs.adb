with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Memory_Grants;
with CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Streams;
with Startup;

package body Remote_Jobs is
   use Interfaces;
   use type W.Job_State;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Launching.Control_Result;
   use type CuBit.Child_Exits.Termination_Kind;
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;

   Program : constant String := "ccl-build.app";
   --  Where ccl-build's places are (its manifest, Build_Platform on CuBit).
   System_Volume : constant String := "@system";
   Toolchain_Import : constant String := "gnat-15.3.0-cubit";
   Stderr_Prefix : constant String := "[stderr] ";
   LF : constant Character := ASCII.LF;

   function Launcher return CuBit.Launching.Launcher is
     (CuBit.Launching.Using (Startup.Process_Manager));

   type Output_Store is array (0 .. Output_Bytes - 1) of Unsigned_8;
   type Job_Record is record
      Id : Unsigned_32 := 0;                 --  0: the slot is free
      State : W.Job_State := W.Unknown_Job;
      Started : CuBit.Launching.Child;
      Code : Unsigned_32 := 0;
      Total : Unsigned_64 := 0;              --  bytes ever written
      Finished_At : Unsigned_64 := 0;
      Output : Output_Store := [others => 0];
   end record;
   type Job_Index is range 1 .. Maximum_Jobs;
   Jobs : array (Job_Index) of Job_Record;
   Next_Id : Unsigned_32 := 1;
   Current : Job_Index'Base := 0;            --  the running job's slot

   --  The work root, its volume ("@system") and its POSIX spelling
   --  ("/work": absolute names are the system volume's). ccl-build is
   --  given POSIX names (its core joins paths like any POSIX tool), so
   --  builds need the work root on the system volume, where the
   --  Storehouse and the toolchain are.
   Root_Bytes : constant := 64;
   Work_Root : String (1 .. Root_Bytes) := [others => ' '];
   Work_Length : Natural range 0 .. Root_Bytes := 0;
   Volume_Length : Natural range 0 .. Root_Bytes := 0;

   function Work return String is (Work_Root (1 .. Work_Length));
   function Volume return String is (Work_Root (1 .. Volume_Length));
   function Posix_Work return String is (Work_Root (Volume_Length + 1 .. Work_Length));

   --  ccl-build's outlets, lent for one run at a time.
   type Outlet_Index is range 1 .. 2;
   Outlet_Names : constant array (Outlet_Index) of access constant String :=
     [new String'("unix.stdout"), new String'("unix.stderr")];
   Ring_Pages : constant := 4;
   Ring_Bases : array (Outlet_Index) of Unsigned_64 := [others => 0];
   Ring_References : array (Outlet_Index) of CuBit.Memory_Grants.Grant_Reference;
   Lent : array (Outlet_Index) of Boolean := [others => False];
   Rings : CuBit.Outlet_Rings.Table;

   function Now return Unsigned_64 is
     (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME));

   procedure Initialize (Work : String) is
   begin
      Work_Length := Natural'Min (Work'Length, Root_Bytes);
      Work_Root (1 .. Work_Length) := Work (Work'First .. Work'First + Work_Length - 1);
      Volume_Length := 0;
      for I in 1 .. Work_Length loop
         if Work_Root (I) = '/' then
            Volume_Length := I - 1;
            exit;
         end if;
      end loop;
   end Initialize;

   procedure Record_Output (J : in out Job_Record; Text : String) is
   begin
      for C of Text loop
         J.Output (Natural (J.Total mod Output_Bytes)) := Character'Pos (C);
         J.Total := J.Total + 1;
      end loop;
   end Record_Output;

   Line_Bytes : constant := 512;

   procedure Drain is
      Buffer : String (1 .. Line_Bytes);
      Read : Natural;
   begin
      if Current = 0 then
         return;
      end if;
      for O in Outlet_Index loop
         if Lent (O) then
            loop
               Read := CuBit.Stream_Regions.Read_Owned
                 (Ring_Bases (O), Ring_Pages, Buffer'Address, Buffer'Length);
               exit when Read = 0;
               if O = Outlet_Index'Last then
                  Record_Output (Jobs (Current), Stderr_Prefix);
               end if;
               Record_Output (Jobs (Current), Buffer (1 .. Read));
               if Buffer (Read) /= LF then
                  Record_Output (Jobs (Current), [1 => LF]);
               end if;
            end loop;
         end if;
      end loop;
   end Drain;

   procedure Revoke_Outlets is
      Revoked : Boolean;
   begin
      Drain;
      for O in Outlet_Index loop
         if Lent (O) then
            CuBit.Memory_Grants.Revoke (Ring_References (O), Revoked);
            Lent (O) := False;
         end if;
      end loop;
      Rings.Count := 0;
   end Revoke_Outlets;

   procedure Lend_Outlets is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PP.Signature;
      Index : PP.Connector_Index;
      Ok, Found : Boolean;
      Grant : Unsigned_64;
   begin
      Rings.Count := 0;
      CuBit.Launching.Describe (Launcher, Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return;
      end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return;
      end if;
      for O in Outlet_Index loop
         PP.Find_Connector (S, Outlet_Names (O).all, Index, Found);
         if Found then
            CuBit.Launching.Lend_Ring
              (Launcher, Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Ring_Bases (O), Grant,
               Ring_References (O), Ok);
            if Ok then
               Rings.Count := Rings.Count + 1;
               Rings.Entries (Rings.Count) := (Outlet => Index, Grant => Grant);
               Lent (O) := True;
            end if;
         end if;
      end loop;
   end Lend_Outlets;

   --  A slot for a new job: a free one, else one whose job finished more
   --  than Orphan_Retention_Ms ago.
   function Free_Slot return Job_Index'Base is
   begin
      for J in Job_Index loop
         if Jobs (J).Id = 0 then
            return J;
         end if;
      end loop;
      for J in Job_Index loop
         if Jobs (J).State /= W.Running and then Now - Jobs (J).Finished_At > Orphan_Retention_Ms then
            return J;
         end if;
      end loop;
      return 0;
   end Free_Slot;

   procedure Finish (J : Job_Index; State : W.Job_State; Code : Unsigned_32) is
   begin
      Jobs (J).State := State;
      Jobs (J).Code := Code;
      Jobs (J).Finished_At := Now;
      if Current = J then
         Revoke_Outlets;
         Current := 0;
      end if;
   end Finish;

   procedure Start (Component : String; Job : out Unsigned_32; Result : out W.Status) is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean;
      Launched : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Slot : constant Job_Index'Base := Free_Slot;
      Workspace : constant String := Posix_Work & "/remote";

      procedure Argument (Text : String) is
      begin
         LA.Add_Argument (Block, Text, Added);
         Accepted := Accepted and Added;
      end Argument;
   begin
      Job := 0;
      if Current /= 0 then
         Result := W.Busy;
         return;
      elsif Slot = 0 then
         Result := W.Busy;
         return;
      elsif Volume_Length = 0 then
         Result := W.Unavailable;
         return;
      elsif Volume /= System_Volume then
         --  ccl-build's names would point at another volume than the
         --  places it is given: refuse rather than build somewhere else.
         Result := W.Unavailable;
         return;
      end if;
      Accepted := True;
      LA.Start (Block);
      Argument (Program);
      Argument ("build");
      Argument ("--store");
      Argument ("/storehouse");
      Argument ("--source-root");
      Argument (Workspace);
      Argument ("--import");
      Argument (Toolchain_Import & "=/toolchain");
      Argument ("--out");
      Argument (Workspace & "/out");
      Argument (Workspace & "/" & Component);
      LA.Finish (Block, Length, Added);
      Accepted := Accepted and Added;
      --  What ccl-build's steps need (decision D2): the Storehouse, the
      --  toolchain, and the workspace (sources in, products out).
      LG.Start (Places);
      LG.Add (Places, LG.All_Rights, System_Volume & "/storehouse", Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.Read_Right, System_Volume & "/toolchain", Added);
      Accepted := Accepted and Added;
      LG.Add (Places, LG.All_Rights, Work & "/remote", Added);
      Accepted := Accepted and Added;
      LG.Finish (Places, Region, Region_Length);
      if not Accepted then
         Result := W.Malformed;
         return;
      end if;
      Jobs (Slot) := (Id => Next_Id, State => W.Running, others => <>);
      Job := Next_Id;
      Next_Id := (if Next_Id = Unsigned_32'Last then 1 else Next_Id + 1);
      Current := Slot;
      Lend_Outlets;
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length),
         Jobs (Slot).Started, Launched, Failure, Rings);
      if Launched /= CuBit.Launching.Launched then
         Record_Output (Jobs (Slot), "ccl-remote: ccl-build.app could not be started: " &
           (if Launched = CuBit.Launching.No_Process_Manager then "no process manager"
            else "the process manager refused the launch") & LF);
         Finish (Slot, W.Failed_To_Start, 0);
      end if;
      Result := W.OK;
   end Start;

   function Find (Job : W.Job_Id) return Job_Index'Base is
   begin
      for J in Job_Index loop
         if Jobs (J).Id = Job then
            return J;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Poll
     (Job : W.Job_Id; Offset : W.File_Offset; State : out W.Job_State;
      Exit_Code : out Unsigned_32; Total, First_Kept : out Unsigned_64;
      Output : out W.Byte_Array; Length : out Natural)
   is
      J : Job_Index'Base;
      From : Unsigned_64;
   begin
      Pump;
      Output := [others => 0];
      Length := 0;
      Exit_Code := 0;
      Total := 0;
      First_Kept := 0;
      J := Find (Job);
      if J = 0 then
         State := W.Unknown_Job;
         return;
      end if;
      State := Jobs (J).State;
      Exit_Code := Jobs (J).Code;
      Total := Jobs (J).Total;
      First_Kept := (if Total > Output_Bytes then Total - Output_Bytes else 0);
      From := Unsigned_64'Max (Offset, First_Kept);
      while From < Total and then Length < Output'Length and then Length < W.Maximum_Chunk_Bytes loop
         Output (Output'First + Length) := Jobs (J).Output (Natural (From mod Output_Bytes));
         Length := Length + 1;
         From := From + 1;
      end loop;
      --  Polled to the end after it finished: the host has it all.
      if State /= W.Running and then From = Total then
         Jobs (J) := (others => <>);
      end if;
   end Poll;

   procedure Cancel (Job : W.Job_Id; State : out W.Job_State) is
      J : constant Job_Index'Base := Find (Job);
      Sent : CuBit.Launching.Control_Result;
   begin
      if J = 0 then
         State := W.Unknown_Job;
         return;
      end if;
      if Jobs (J).State = W.Running then
         CuBit.Launching.Send_Control (Jobs (J).Started, CuBit.Control_Events.Interrupt, Sent);
         if Sent = CuBit.Launching.Sent then
            Record_Output (Jobs (J), "ccl-remote: interrupt sent" & LF);
         end if;
      end if;
      State := Jobs (J).State;
   end Cancel;

   procedure Pump is
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean;
      Interrupted_Exit_Code : constant := 130;   --  128 + SIGINT, as the libc reports it
   begin
      if Current = 0 then
         return;
      end if;
      Drain;
      CuBit.Launching.Poll_Exit (Jobs (Current).Started, Has_Ended, Ended);
      if Has_Ended then
         --  Stopped: ended from outside (not by its own exit).
         Finish (Current,
                 (if Ended.Kind /= CuBit.Child_Exits.Exited or else Ended.Code = Interrupted_Exit_Code
                  then W.Cancelled else W.Exited),
                 (if Ended.Kind = CuBit.Child_Exits.Exited then Unsigned_32 (Ended.Code) else 0));
      end if;
   end Pump;

   function Running return Boolean is (Current /= 0);
end Remote_Jobs;
