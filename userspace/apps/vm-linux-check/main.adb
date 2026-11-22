--  VMM-001 slice 1: boot the pinned Linux guest to its shell prompt under
--  the CuBit hypervisor, from procmgr's place, first on one vCPU, then on
--  two (the guest starts its AP with INIT and SIPI). The guest's serial
--  console goes to the test console (lines prefixed "guest: ") and the time
--  to each stage is reported (docs/hypervisor.md 12.2). At the prompt the
--  check types into the guest's serial port (COM1's receive FIFO) as a
--  terminal would: a command a key at a time (each key's echo timed), then
--  Ctrl+C on a sleep, and a command sent faster than the FIFO drains.
pragma Ada_2022;
with Interfaces; use Interfaces;
with Embedded;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Test_Consoles;
with CuBit.Monotonic;
with Vmm_Machine;

procedure Main is
   Roots : constant CuBit.Handles.Boot_Roots.Launcher_Handles :=
     CuBit.Handles.Boot_Roots.Process_Manager;

   procedure Say (Text : String) is
      Answer : CuBit.Handles.Test_Consoles.Outcome;
      pragma Unreferenced (Answer);
   begin
      Answer := CuBit.Handles.Test_Consoles.Write
        (Roots.Test_Console_Object, Text & ASCII.LF);
   end Say;

   --  The line the guest's /init prints with its online CPU count.
   CPUs_Line : constant String := "CuBit guest: 2 CPUs online";
   Saw_Two_CPUs : Boolean := False;

   --  What the guest prints for the typed commands.
   Typed_Answer : constant String := "typed-42";
   Interrupted_Answer : constant String := "interrupted-5";
   Saw_Typed, Saw_Interrupted : Boolean := False;

   procedure Guest_Line (Text : String) is
   begin
      Say ("guest: " & Text);
      if Text = Typed_Answer then
         Saw_Typed := True;
      elsif Text = Interrupted_Answer then
         Saw_Interrupted := True;
      end if;
      if Text'Length >= CPUs_Line'Length
        and then (for some I in Text'First .. Text'Last - CPUs_Line'Length + 1 =>
                    Text (I .. I + CPUs_Line'Length - 1) = CPUs_Line)
      then
         Saw_Two_CPUs := True;
      end if;
   end Guest_Line;

   procedure Monitor_Line (Text : String) is
   begin
      Say ("vm-linux: " & Text);
   end Monitor_Line;

   function Ms (Us : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (Us / 1000);
   begin
      return S (S'First + 1 .. S'Last) & " ms";
   end Ms;

   --  The typing script, from the shell's prompt: each step has Step_Ms.
   CR : constant Character := Character'Val (16#0D#);
   ETX : constant Character := Character'Val (16#03#);   --  Ctrl+C
   Typed_Command : constant String := "echo typed-$((6*7))" & CR;
   Sleep_Command : constant String := "sleep 100" & CR;
   --  Longer than the 16-byte FIFO: sent as fast as the guest drains it.
   Burst_Command : constant String := "echo interrupted-$((2+3))" & CR;
   Prompt_End : constant String := "# ";
   Step_Ms : constant := 10_000;
   Sleep_Settle_Ms : constant := 1_000;   --  sleep is running before Ctrl+C
   Linger_Ms : constant := 500;
   type Script_Step is
     (Wait_Prompt, Type_Keys, Wait_Typed, Type_Sleep, Settle, Interrupt, Wait_Prompt_Again,
      Send_Burst, Wait_Interrupted, Linger, Finished, Failed);
   Step : Script_Step := Wait_Prompt;
   Step_Started : Unsigned_64 := 0;
   Next_Key : Positive := 1;
   Echoes_Before : Unsigned_64 := 0;
   Interrupted_At : Unsigned_64 := 0;
   Prompt_Back_Ms : Unsigned_64 := 0;
   Prompt_Back : Boolean := False;
   --  Key-to-echo times, in microseconds, one per typed key.
   Max_Samples : constant := Typed_Command'Length;
   subtype Sample_Count is Natural range 0 .. Max_Samples;
   type Sample_List is array (1 .. Max_Samples) of Unsigned_64;
   Samples : Sample_List := [others => 0];
   Sampled : Sample_Count := 0;

   function Now return Unsigned_64 is (CuBit.Monotonic.Milliseconds);

   procedure Go (S : Script_Step) is
   begin
      Step := S;
      Step_Started := Now;
   end Go;

   function At_Prompt return Boolean is
      P : constant String := Vmm_Machine.Partial_Line;
   begin
      return P'Length >= Prompt_End'Length and then P (P'Last - Prompt_End'Length + 1 .. P'Last) = Prompt_End;
   end At_Prompt;

   --  Send Text from Next_Key, as far as the FIFO has room; True once all is sent.
   function Send (Text : String) return Boolean is
   begin
      while Next_Key <= Text'Length and then Vmm_Machine.Input_Room > 0 loop
         Vmm_Machine.Send_Input (Character'Pos (Text (Text'First + Next_Key - 1)));
         Next_Key := Next_Key + 1;
      end loop;
      return Next_Key > Text'Length;
   end Send;

   procedure Pump (Stop : out Boolean) is
   begin
      if Step not in Wait_Prompt | Linger | Finished | Failed and then Now - Step_Started >= Step_Ms then
         Monitor_Line ("typing script: no progress in " & Script_Step'Image (Step));
         Go (Failed);
      end if;
      case Step is
         when Wait_Prompt =>
            if Vmm_Machine.Stage_Us (Vmm_Machine.Prompt) /= 0 and then At_Prompt then
               Next_Key := 1;
               Echoes_Before := Vmm_Machine.Echo_Count;
               Go (Type_Keys);
               --  The first key now; each next one after the last one's echo.
               Vmm_Machine.Send_Input (Character'Pos (Typed_Command (1)));
               Next_Key := 2;
            end if;
         when Type_Keys =>
            if Vmm_Machine.Echo_Count > Echoes_Before then
               Echoes_Before := Vmm_Machine.Echo_Count;
               Sampled := Sampled + 1;
               Samples (Sampled) := Vmm_Machine.Last_Echo_Us;
               if Next_Key > Typed_Command'Length then
                  Go (Wait_Typed);
               elsif Vmm_Machine.Input_Room > 0 then
                  Vmm_Machine.Send_Input (Character'Pos (Typed_Command (Next_Key)));
                  Next_Key := Next_Key + 1;
               end if;
            end if;
         when Wait_Typed =>
            if Saw_Typed and then At_Prompt then
               Next_Key := 1;
               Go (Type_Sleep);
            end if;
         when Type_Sleep =>
            if Send (Sleep_Command) then
               Go (Settle);
            end if;
         when Settle =>
            if Now - Step_Started >= Sleep_Settle_Ms then
               Go (Interrupt);
            end if;
         when Interrupt =>
            if Vmm_Machine.Input_Room > 0 then
               Vmm_Machine.Send_Input (Character'Pos (ETX));
               Interrupted_At := Now;
               Go (Wait_Prompt_Again);
            end if;
         when Wait_Prompt_Again =>
            if At_Prompt then
               Prompt_Back_Ms := Now - Interrupted_At;
               Prompt_Back := True;
               Next_Key := 1;
               Go (Send_Burst);
            end if;
         when Send_Burst =>
            if Send (Burst_Command) then
               Go (Wait_Interrupted);
            end if;
         when Wait_Interrupted =>
            if Saw_Interrupted then
               Go (Linger);
            end if;
         when Linger =>
            if Now - Step_Started >= Linger_Ms then
               Go (Finished);
            end if;
         when Finished | Failed =>
            null;
      end case;
      Stop := Step in Finished | Failed;
   end Pump;

   function Us (Value : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (Value);
   begin
      return S (S'First + 1 .. S'Last) & " us";
   end Us;

   --  The typed keys' echo times: least, median and greatest.
   procedure Report_Echoes (Tag : String) is
      Sorted : Sample_List := Samples;
      T : Unsigned_64;
   begin
      if Sampled = 0 then
         Monitor_Line (Tag & "no key was echoed");
         return;
      end if;
      for I in 2 .. Sampled loop
         for J in reverse 2 .. I loop
            exit when Sorted (J - 1) <= Sorted (J);
            T := Sorted (J); Sorted (J) := Sorted (J - 1); Sorted (J - 1) := T;
         end loop;
      end loop;
      Monitor_Line (Tag & "key to echo over" & Natural'Image (Sampled) & " keys: least " & Us (Sorted (1))
                    & ", median " & Us (Sorted ((Sampled + 1) / 2)) & ", greatest " & Us (Sorted (Sampled)));
   end Report_Echoes;

   function Boot (VCPUs : Vmm_Machine.VCPU_Count) return Boolean is
      Result : Vmm_Machine.Boot_Result;
      use type Vmm_Machine.Boot_Result;
      Tag : constant String := "[" & Integer'Image (VCPUs) (2 .. Integer'Image (VCPUs)'Last) & " vCPU] ";
      OK : Boolean;
   begin
      Step := Wait_Prompt;
      Sampled := 0;
      Saw_Typed := False;
      Saw_Interrupted := False;
      Prompt_Back := False;
      Monitor_Line (Tag & "start");
      Vmm_Machine.Boot_Linux
        (Authority    => Roots.VM_Create_Authority,
         Guest        => (Kernel => Embedded.Guest_Kernel'Address,
                          Kernel_Bytes => Embedded.Guest_Kernel_Bytes,
                          Initrd => Embedded.Guest_Initrd'Address,
                          Initrd_Bytes => Embedded.Guest_Initrd_Bytes),
         RAM_MiB      => 128,
         Command_Line => "rdinit=/init",
         Run_Ms       => 60_000,
         Console      => Guest_Line'Unrestricted_Access,
         Log          => Monitor_Line'Unrestricted_Access,
         Result       => Result,
         Pump         => Pump'Unrestricted_Access,
         VCPUs        => VCPUs);
      for S in Vmm_Machine.Stage loop
         Monitor_Line (Tag & "stage " & Vmm_Machine.Stage'Image (S) & " at "
                       & Ms (Vmm_Machine.Stage_Us (S)));
      end loop;
      Monitor_Line (Tag & "exits returned to the monitor:" & Unsigned_64'Image (Vmm_Machine.Exits_Returned));
      Monitor_Line (Tag & "result " & Vmm_Machine.Boot_Result'Image (Result));
      OK := Vmm_Machine.Stage_Us (Vmm_Machine.Prompt) /= 0 and then Result = Vmm_Machine.Booted_To_Deadline;
      Monitor_Line (Tag & "the guest reached its shell prompt " & (if OK then "PASS" else "FAIL"));
      Report_Echoes (Tag);
      Monitor_Line (Tag & "keys typed one at a time reach the shell and each is echoed: "
                    & Typed_Answer & " " & (if Saw_Typed and then Sampled = Typed_Command'Length
                                            then "PASS" else "FAIL"));
      Monitor_Line (Tag & "Ctrl+C ends a foreground sleep 100, the prompt back in"
                    & Unsigned_64'Image (Prompt_Back_Ms) & " ms "
                    & (if Prompt_Back then "PASS" else "FAIL"));
      Monitor_Line (Tag & "a command longer than the receive FIFO arrives whole: " & Interrupted_Answer & " "
                    & (if Saw_Interrupted then "PASS" else "FAIL"));
      OK := OK and then Saw_Typed and then Sampled = Typed_Command'Length and then Saw_Interrupted
        and then Prompt_Back and then Step = Finished;
      if VCPUs > 1 then
         Monitor_Line (Tag & "the guest started its AP with INIT and SIPI "
                       & (if Vmm_Machine.APs_Started = VCPUs - 1 then "PASS" else "FAIL"));
         Monitor_Line (Tag & "the guest reports 2 CPUs online " & (if Saw_Two_CPUs then "PASS" else "FAIL"));
         Monitor_Line (Tag & "INIT, SIPI and the AP's mode switch left the nested tables unchanged, "
                       & "its tag flushed " & (if Vmm_Machine.AP_Tables_Unchanged then "PASS" else "FAIL"));
         OK := OK and then Vmm_Machine.APs_Started = VCPUs - 1 and then Saw_Two_CPUs
           and then Vmm_Machine.AP_Tables_Unchanged;
      end if;
      return OK;
   end Boot;

   One, Two : Boolean;
begin
   One := Boot (1);
   Two := Boot (2);
   Monitor_Line (if One and then Two then "done PASS" else "done FAIL");
end Main;
