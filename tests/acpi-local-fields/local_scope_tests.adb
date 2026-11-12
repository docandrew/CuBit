pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with ACPI_Local_Field_Scope;
with ACPI_Global_Lock_Protocol;
with AML_Decode;
with AML_Field_Layout;
with AML_Register_Views;
with AML_Field_Plans;
procedure Local_Scope_Tests is
   package P renames ACPI_Global_Lock_Protocol;
   use type Interfaces.Unsigned_64, P.Lock_Outcome;
   Mode : constant String := Ada.Command_Line.Argument (1);
   type Lease is limited record Prepared : Boolean := False; Generation : Natural := 0; end record;
   Checks, Acquires, Releases, Windows, Access_Checks : Natural := 0;
   Physical_Held, Recover, Expired : Boolean := False;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Mode & Checks'Image; end if; end Check;
   function Admission return Boolean;
   function Due (Item : Lease) return Boolean is (Item.Prepared);
   procedure Window (Deadline : out Interfaces.Unsigned_64; Budget : out P.Attempt_Budget; Available : out Boolean);
   procedure Acquire (Item : in out Lease; Deadline : Interfaces.Unsigned_64;
      Budget : P.Attempt_Budget; Result : out P.Lock_Outcome);
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome);
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome);
   package S is new ACPI_Local_Field_Scope (Lease,Admission,Due,Window,Acquire,Validate,Release,4);
   use type S.Depth_Count;
   State : S.Context;
   procedure Reentry is
      OK : Boolean;
      Before : constant S.Depth_Count := S.Observe (State).Depth;
   begin
      if Mode /= "reentry" then return; end if;
      Check (S.Observe (State).Working);
      S.Begin_Field (State,True,True,OK); Check (not OK and S.Observe (State).Depth = Before);
      S.End_Field (State,OK); Check (not OK and S.Observe (State).Depth = Before);
      S.Check_Datum (State,OK); Check (not OK);
      S.Cleanup (State,OK); Check (not OK);
      S.Mark_Fault (State); Check (not S.Observe (State).Faulted);
   end Reentry;
   function Admission return Boolean is
   begin Reentry; return not Physical_Held and Mode /= "closed-admission"; end Admission;
   procedure Window (Deadline : out Interfaces.Unsigned_64; Budget : out P.Attempt_Budget; Available : out Boolean) is
   begin
      Windows := Windows + 1; Reentry;
      Deadline := (if Mode = "zero-window" then 0 elsif Mode = "forever-window" then Interfaces.Unsigned_64'Last else 100);
      Budget := 3; Available := Mode /= "unavailable-window";
   end Window;
   procedure Acquire (Item : in out Lease; Deadline : Interfaces.Unsigned_64;
      Budget : P.Attempt_Budget; Result : out P.Lock_Outcome) is
   begin
      Check (not Item.Prepared and not Physical_Held and Deadline = 100 and Budget = 3);
      Acquires := Acquires + 1; Item.Generation := Acquires; Item.Prepared := True; Physical_Held := True;
      Reentry;
      Result := (if Mode = "acquire-late" or Mode = "child-failure" then P.Acquired_After_Deadline
         elsif Mode = "acquire-unknown" then P.Atomic_Indeterminate else P.Acquired);
      if Mode = "acquire-refused" then
         Item.Prepared := False; Physical_Held := False; Result := P.Contended;
      end if;
      if Mode = "contradictory-acquire" then Item.Prepared := False; Physical_Held := False; end if;
   end Acquire;
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome) is
   begin
      Access_Checks := Access_Checks + 1; Check (Item.Prepared and Item.Generation = Acquires and Physical_Held);
      Reentry;
      Result := (if Expired then P.Expired else P.Configured);
   end Validate;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome) is
   begin
      Releases := Releases + 1; Check (Item.Prepared and Item.Generation = Acquires and Physical_Held);
      Reentry;
      if (Mode = "release-unknown" or Mode = "signal-deferred") and not Recover then
         Result := (if Mode = "signal-deferred" then P.Signal_Deferred else P.Atomic_Indeterminate);
      elsif Mode = "contradictory-release" then Result := P.Released;
      else Item.Prepared := False; Physical_Held := False; Result := P.Released;
      end if;
   end Release;
   OK : Boolean;
   procedure Policy_Matrix is
      package L renames AML_Field_Layout;
      use type AML_Decode.Byte;
      Field, Unlocked : L.Field_Geometry;
      Register : L.Register_Geometry;
      Old_Layout, Old_Plan : Boolean;
   begin
      for Space in L.Memory_Space .. L.Embedded_Control_Space loop
         Field := (Region => (Space => Space, Base => 0, Length => 16,
            PCI => (if Space = L.PCI_Config_Space then (Known => True, Device => <>) else (Known => False))),
            Bit_Length => 8, Effective_Access => 1, others => <>);
         Register := (Region => Field.Region, others => <>);
         for Flags in AML_Decode.Byte loop
            Field.Flags := Flags; Unlocked := Field; Unlocked.Flags := Flags and 16#EF#;
            Old_Layout := L.Admitted (Unlocked,Register);
            Old_Plan := AML_Field_Plans.Valid (AML_Field_Plans.Prepare (Unlocked));
            Check (L.Admitted (Field,Register,False,True) = Old_Layout);
            Check (AML_Register_Views.Admitted (Field,Register,False,True) = Old_Layout);
            Check (AML_Field_Plans.Valid (AML_Field_Plans.Prepare (Field,False,True)) = Old_Plan);
            Check (L.Admitted (Field,Register) = (Old_Layout and (Flags and 16#10#) = 0));
            Check (AML_Field_Plans.Valid (AML_Field_Plans.Prepare (Field)) =
               (Old_Plan and (Flags and 16#10#) = 0));
            Check (L.Admitted (Field,Register,True) =
               (Old_Layout and ((Flags and 16#10#) = 0 or Space = L.Memory_Space)));
            Check (AML_Field_Plans.Valid (AML_Field_Plans.Prepare (Field,True)) =
               (Old_Plan and ((Flags and 16#10#) = 0 or Space = L.Memory_Space)));
         end loop;
      end loop;
   end Policy_Matrix;
begin
   if Mode = "normal" then Policy_Matrix; end if;
   Check (S.Ready (State) and not S.Pending (State));
   S.Check_Datum (State,OK); Check (not OK and Acquires = 0 and Access_Checks = 0);
   if Mode = "unsolicited-end" then
      S.End_Field (State,OK); Check (not OK and S.Observe (State).Faulted and Releases = 0);
   elsif Mode = "capacity" then
      for I in 1 .. 4 loop S.Begin_Field (State,False,False,OK); Check (OK and S.Observe (State).Depth = S.Depth_Count (I)); end loop;
      S.Begin_Field (State,True,False,OK); Check (not OK and S.Observe (State).Depth = 4 and Acquires = 0);
      for I in reverse 1 .. 4 loop S.End_Field (State,OK); Check (not OK and S.Observe (State).Depth = S.Depth_Count (I-1)); end loop;
   elsif Mode = "child-failure" then
      S.Begin_Field (State,False,False,OK); Check (OK);
      S.Begin_Field (State,True,False,OK); Check (not OK and S.Observe (State).Depth = 1);
      S.Cleanup (State,OK); Check (not OK and Releases = 0);
      S.End_Field (State,OK); Check (not OK and Releases = 0 and S.Observe (State).Depth = 0);
      S.Cleanup (State,OK); Check (OK and Releases = 1 and not Physical_Held);
   elsif Mode = "normal" or Mode = "reentry" or Mode = "repeated" then
      for Cycle in 1 .. (if Mode = "repeated" then 100 else 1) loop
         S.Begin_Field (State,False,False,OK); Check (OK);
         S.Check_Datum (State,OK); Check (OK);
         S.Begin_Field (State,True,True,OK); Check (OK and S.Observe (State).Held);
         S.Begin_Field (State,True,False,OK); Check (OK and Acquires = Cycle and Windows = Cycle);
         S.Check_Datum (State,OK); Check (OK);
         S.End_Field (State,OK); Check (OK and Releases = Cycle-1 and Physical_Held);
         S.End_Field (State,OK); Check (OK and Releases = Cycle and not Physical_Held);
         S.Check_Datum (State,OK); Check (OK);
         S.End_Field (State,OK); Check (OK and S.Observe (State).Depth = 0 and not S.Pending (State));
      end loop;
   else
      S.Begin_Field (State,True,False,OK);
      if Mode = "zero-window" or Mode = "forever-window" or Mode = "unavailable-window" or Mode = "closed-admission" then
         Check (not OK and S.Observe (State).Depth = 0 and Acquires = 0);
      elsif Mode = "acquire-late" or Mode = "acquire-unknown" then
         Check (not OK and S.Observe (State).Depth = 0 and S.Observe (State).Cleanup_Required);
         S.Cleanup (State,OK); Check (OK and Releases = 1 and not Physical_Held);
      elsif Mode = "contradictory-acquire" then
         Check (not OK and S.Observe (State).Depth = 0 and S.Observe (State).Faulted);
      elsif Mode = "acquire-refused" then
         Check (not OK and not S.Observe (State).Cleanup_Required and not Physical_Held);
         S.Cleanup (State,OK); Check (OK and Releases = 0 and S.Observe (State).First_Failure = P.Contended);
      else
         Check (OK and Physical_Held);
         if Mode = "expire-read" then
            Expired := True; S.Check_Datum (State,OK); Check (not OK and S.Observe (State).First_Failure = P.Expired);
         elsif Mode = "expire-end" then Expired := True;
         elsif Mode = "data-failure" then S.Mark_Fault (State);
         elsif Mode = "cleanup-parent" then S.Cleanup (State,OK); Check (not OK and Releases = 0);
         end if;
         S.End_Field (State,OK); Check (not OK and S.Observe (State).Depth = 0);
         if Mode = "release-unknown" or Mode = "signal-deferred" then
            Check (S.Observe (State).Cleanup_Required and Physical_Held and Releases = 1);
            S.Begin_Field (State,False,False,OK); Check (not OK and Acquires = 1);
            Recover := True; S.Cleanup (State,OK); Check (OK and Releases = 2 and not Physical_Held);
         elsif Mode = "contradictory-release" then
            Check (S.Observe (State).Cleanup_Required and Physical_Held);
         else Check (not Physical_Held and Releases = 1);
         end if;
      end if;
      Check (not S.Ready (State));
   end if;
   Ada.Text_IO.Put_Line ("LOCAL SCOPE " & Mode & " checks" & Checks'Image);
end Local_Scope_Tests;
