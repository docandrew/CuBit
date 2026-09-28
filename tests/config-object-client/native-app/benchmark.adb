with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Benchmark_Clock;
with CCL.Types;
with CCL.Objects;
with Config_Object_Client;
with Config_Object_Messages;

-- Native public IPC benchmark, with the same narrow manifest as the regression
-- app. No filesystem/worker authority, SQL, CBOR or direct service calls.
procedure Benchmark is
   package Client renames Config_Object_Client;
   package Wire renames Config_Object_Messages;
   package Clock renames CuBit.Benchmark_Clock;
   use type Client.Submission;
   use type Client.Completion_Result;
   use type Wire.Status;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   Writer, Reader : Client.Client;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Sent : Client.Submission;
   Item, Read_Item, Write_Item : Client.Response;
   Token, Ignore, Rate, Started, Finished, Read_Start, Write_Start : Unsigned_64 := 0;
   Revision : Unsigned_64 := 1;
   Good : Boolean;
   Name : constant String := "org.cubit.publication";
   Samples : constant := 64;
   type Measurements is array (1 .. Samples) of Unsigned_64;
   Cached_Read, Commit, Overlap_Read, Overlap_Commit : Measurements;
   Read_First : Natural := 0;

   procedure Check (OK : Boolean; Step : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL config-objects-benchmark " & Step & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Check;

   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1;
      return Token;
   end Next_Token;

   function Value (N : Unsigned_64) return CCL.Objects.Image is
      Result : CCL.Objects.Image := CCL.Objects.Empty (Contract);
      Built : CCL.Objects.Build_Result;
   begin
      CCL.Objects.Append (Result, CCL.Objects.Integer_Cell (Integer_64 (N)), Built);
      Check (Built = CCL.Objects.Added, "value construction");
      return Result;
   end Value;

   procedure Receive (Completion : out CompletionEntry) is
      Entry_Buffer : aliased CompletionEntry := NULL_COMPLETION;
      Activity : Activity_Result;
   begin
      loop
         if Poll_Completion (Entry_Buffer'Address) = 1 then
            Completion := Entry_Buffer;
            return;
         end if;
         Activity := Wait_For_Activity_Until (Unsigned_64'Last);
         Check (Activity /= Unavailable, "wait");
      end loop;
   end Receive;

   procedure Take (Object : in out Client.Client; Result : out Client.Response) is
      Taken : Boolean;
   begin
      Client.Take_Result (Object, Result, Taken);
      Check (Taken and Result.Valid and Result.Code = Wire.Success, "reply");
   end Take;

   procedure Finish (Object : in out Client.Client; Result : out Client.Response) is
      Entry_Buffer : CompletionEntry;
      Done : Client.Completion_Result;
   begin
      Check (Sent = Client.Submitted, "submit");
      Receive (Entry_Buffer);
      Client.Complete (Object, Entry_Buffer, Done);
      Check (Done = Client.Completed, "completion identity");
      Take (Object, Result);
   end Finish;

   function Elapsed (Start, Stop : Unsigned_64) return Unsigned_64 is
   begin
      Check (Stop > Start, "monotonic counter");
      return Stop - Start;
   end Elapsed;

   procedure Close (Object : in out Client.Client) is
   begin
      Client.Close (Object, Next_Token, Sent);
      Finish (Object, Item);
      Client.Retire (Object, Good);
      Check (Good, "grant retirement");
   end Close;

   procedure Report (Phase : String; Data : Measurements) is
   begin
      for I in Data'Range loop
         debugPrint ("CONFIG-BENCH: sample phase=" & Phase &
           " index=" & I'Image & " ticks=" & Data (I)'Image & ASCII.LF);
      end loop;
   end Report;
begin
   Clock.Calibrate (Rate);
   Check (Rate > 0, "counter calibration");
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   Check (Good, "binding");
   Client.Initialize (Writer, CAP_SLOT_CONFIG, Good);
   Check (Good, "writer grant");
   Client.Initialize (Reader, CAP_SLOT_CONFIG, Good);
   Check (Good, "reader grant");
   Client.Create (Writer, Name, Contract, Wire.Read_Write, 0, Next_Token, Sent);
   Finish (Writer, Item);
   Client.Set (Writer, Value (Revision), 0, Next_Token, Sent);
   Finish (Writer, Item);
   Check (Item.Revision = Revision, "initial revision");
   Client.Open (Reader, Name, Contract, Wire.Read_Only, 0, Next_Token, Sent);
   Finish (Reader, Item);
   -- Untimed warm-up; no serial output occurs between timed operations below.
   for I in 1 .. 8 loop
      Client.Get (Reader, Next_Token, Sent);
      Finish (Reader, Item);
      Check (Item.Revision = Revision and Item.Value = Value (Revision), "warm-up");
   end loop;
   for I in Cached_Read'Range loop
      Started := Clock.Read_Counter;
      Client.Get (Reader, Next_Token, Sent);
      Finish (Reader, Item);
      Finished := Clock.Read_Counter;
      Cached_Read (I) := Elapsed (Started, Finished);
      Check (Item.Revision = Revision and Item.Value = Value (Revision), "cached get");
   end loop;
   for I in Commit'Range loop
      declare
         Next_Value : constant CCL.Objects.Image := Value (Revision + 1);
      begin
         Started := Clock.Read_Counter;
         Client.Set (Writer, Next_Value, Revision, Next_Token, Sent);
         Finish (Writer, Item);
         Finished := Clock.Read_Counter;
         Commit (I) := Elapsed (Started, Finished);
      end;
      Revision := Revision + 1;
      Check (Item.Revision = Revision, "commit revision");
      Client.Get (Reader, Next_Token, Sent);
      Finish (Reader, Item);
      Check (Item.Revision = Revision and Item.Value = Value (Revision), "committed value");
   end loop;
   for I in Overlap_Read'Range loop
      declare
         Next_Value : constant CCL.Objects.Image := Value (Revision + 1);
         Entry_Buffer : CompletionEntry;
         Done : Client.Completion_Result;
         Read_Done, Write_Done : Boolean := False;
      begin
         Write_Start := Clock.Read_Counter;
         Client.Set (Writer, Next_Value, Revision, Next_Token, Sent);
         Check (Sent = Client.Submitted, "overlap write submit");
         Read_Start := Clock.Read_Counter;
         Client.Get (Reader, Next_Token, Sent);
         Check (Sent = Client.Submitted, "overlap read submit");
         while not (Read_Done and Write_Done) loop
            Receive (Entry_Buffer);
            Client.Complete (Writer, Entry_Buffer, Done);
            if Done = Client.Completed then
               Check (not Write_Done, "duplicate write");
               Take (Writer, Write_Item);
               Overlap_Commit (I) := Elapsed (Write_Start, Clock.Read_Counter);
               Write_Done := True;
            else
               Client.Complete (Reader, Entry_Buffer, Done);
               Check (Done = Client.Completed and not Read_Done, "read identity");
               Take (Reader, Read_Item);
               Overlap_Read (I) := Elapsed (Read_Start, Clock.Read_Counter);
               if not Write_Done then Read_First := Read_First + 1; end if;
               Read_Done := True;
            end if;
         end loop;
         -- A read concurrent with publication can observe either committed
         -- revision, never the proposed value labeled with the old revision.
         Check (Read_Item.Revision in Revision .. Revision + 1 and then
                Read_Item.Value = Value (Read_Item.Revision), "concurrent snapshot");
         Revision := Revision + 1;
         Check (Write_Item.Revision = Revision, "overlap commit revision");
      end;
   end loop;
   Client.Get (Reader, Next_Token, Sent);
   Finish (Reader, Item);
   Check (Item.Revision = Revision and Item.Value = Value (Revision), "final value");
   Close (Reader);
   Close (Writer);
   debugPrint ("CONFIG-BENCH: start samples=" & Samples'Image &
     " ticks_per_ms=" & Rate'Image & " final_revision=" & Revision'Image & ASCII.LF);
   Report ("cached-get", Cached_Read);
   Report ("committed-set", Commit);
   Report ("overlap-get", Overlap_Read);
   Report ("overlap-set", Overlap_Commit);
   debugPrint ("CONFIG-BENCH: reads_before_write_reply=" & Read_First'Image & ASCII.LF);
   debugPrint ("TEST: PASS config-objects-benchmark" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Benchmark;
