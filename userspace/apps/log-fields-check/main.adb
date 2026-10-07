pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with CCL_Manifest_Bindings;

--  Native acceptance check for logstore structured fields (LogRecord v2) and
--  severity-filtered subscriptions: a Warning-level observer receives this
--  process's Warning record with its typed fields intact and never sees the
--  Information record published just before it.
procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   use type P.Status;
   use type L.Severity;
   use type L.Field_Kind;

   Writer : CuBit.Logging.Publisher
     (CapabilitySlot (CCL_Manifest_Bindings.Slot_Logstore));
   Reader : CuBit.Logging.Reader
     (CapabilitySlot (CCL_Manifest_Bindings.Slot_Log_Observer));

   Wait_Ms : constant Unsigned_64 := 2_000;
   Frame_Latency_Us : constant := 4_167;
   Frame_Number : constant := 42;
   Delta_Value : constant Integer_64 := -5;
   Filtered_Text : constant String := "log-fields: filtered information";
   Kept_Text : constant String := "log-fields: frame presented late";

   Ignore : Unsigned_64;
   Result : P.Status;
   Value : P.Event;
   Lost : Unsigned_64;
   Saw_Kept, Saw_Filtered : Boolean := False;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL log-fields: " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop
            Ignore := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      end if;
   end Check;

   procedure Publish (Item : L.Log_Record) is
      Submitted, Drained : Boolean;
      Before : constant Unsigned_64 := CuBit.Logging.Dropped (Writer);
   begin
      CuBit.Logging.Emit (Writer, Item, Submitted);
      Check (Submitted, "record written into the ring");
      CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Natural (Wait_Ms));
      Check (Drained, "logstore took the record");
      Check (CuBit.Logging.Dropped (Writer) = Before, "record accepted");
   end Publish;

   function Field_Record return L.Log_Record is
      Item : L.Decoded := L.Make (Kept_Text, L.Warning);
   begin
      Check (Item.Success, "make warning");
      Item := L.With_Field (Item.Value, "latency_us", L.Duration_Microseconds,
                            Frame_Latency_Us);
      Check (Item.Success, "duration field");
      Item := L.With_Field (Item.Value, "frame", L.Unsigned_Integer,
                            Frame_Number);
      Check (Item.Success, "unsigned field");
      Item := L.With_Field (Item.Value, "delta", L.Signed_Integer,
                            not Unsigned_64 (-(Delta_Value + 1)));
      Check (Item.Success, "signed field");
      Item := L.With_Field (Item.Value, "late", L.Truth, 1);
      Check (Item.Success, "truth field");
      return Item.Value;
   end Field_Record;

   Information : constant L.Decoded := L.Make (Filtered_Text, L.Information);
   Own_Pid : constant Unsigned_64 := syscall (SYSCALL_GETPID);
begin
   CuBit.Logging.Subscribe (Reader, Result, Minimum => L.Warning);
   Check (Result = P.OK, "startup observer subscribes with filter");
   loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      exit when Result = P.Empty;
      Check (Result in P.OK | P.Gap, "retained history readable");
      Check (Result = P.Gap or else L.Level (Value.Data) >= L.Warning,
             "retained replay is filtered");
   end loop;

   Check (Information.Success, "make information");
   Publish (Information.Value);
   Publish (Field_Record);

   loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      exit when Result = P.Empty;
      Check (Result = P.OK, "filtered read");
      Check (L.Level (Value.Data) >= L.Warning, "filter honoured");
      if L.Text (Value.Data) = Filtered_Text then
         Saw_Filtered := True;
      elsif L.Text (Value.Data) = Kept_Text and then Value.Source = Own_Pid
      then
         Saw_Kept := True;
         Check (P.Is_Publisher (Value.Publication_Tag), "authenticated tag");
         Check (L.Field_Total (Value.Data) = 4, "four fields");
         Check (L.Name (L.Field_At (Value.Data, 1)) = "latency_us" and then
                L.Kind (L.Field_At (Value.Data, 1)) =
                  L.Duration_Microseconds and then
                L.Value (L.Field_At (Value.Data, 1)) = Frame_Latency_Us,
                "duration field intact");
         Check (L.Value (L.Field_At (Value.Data, 2)) = Frame_Number,
                "unsigned field intact");
         Check (L.Signed_Value (L.Field_At (Value.Data, 3)) = Delta_Value,
                "signed field intact");
         Check (L.Kind (L.Field_At (Value.Data, 4)) = L.Truth and then
                L.Value (L.Field_At (Value.Data, 4)) = 1,
                "truth field intact");
      end if;
   end loop;
   Check (Saw_Kept, "structured warning delivered");
   Check (not Saw_Filtered, "information filtered in service");
   debugPrint ("log-fields: frame latency_us=4167 frame=42 delta=-5 " &
               "late=true delivered to Warning observer" & ASCII.LF);
   debugPrint ("TEST: PASS log-fields" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
