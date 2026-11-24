pragma Ada_2022;
with CuBit.Power_Sources;

package body Power_ACPI_Reader with SPARK_Mode is
   package PS renames CuBit.Power_Sources;
   use type AF.Availability, AF.Battery_Subpage, AF.Entry_Status;

   --  A request counted (the count saturates).
   procedure Call
     (Request : AF.Words; Label : out Unsigned_32; Reply : out AF.Words; Delivered : out Boolean;
      Requests : in out Natural) is
   begin
      Ask (Request, Label, Reply, Delivered);
      if Requests < Natural'Last then
         Requests := Requests + 1;
      end if;
   end Call;

   function OK (Label : Unsigned_32; Delivered : Boolean) return Boolean is
     (Delivered and then Label = AF.Reply_OK);
   function Stale (Label : Unsigned_32; Reply : AF.Words; Delivered : Boolean) return Boolean is
     (Delivered and then Label = AF.Reply_Error and then Reply (0) = AF.Stale_Outcome);

   procedure Read_AC
     (Maximum_Age : AF.Word; Facts : out AF.AC_Facts; Generation : out AF.Word; Requests : in out Natural)
   with Post => (if not Facts.Present then Generation = 0)
   is
      Label : Unsigned_32;
      Reply : AF.Words;
      Delivered, Valid : Boolean;
      Status : AF.Availability;
   begin
      Facts := AF.No_AC;
      Generation := 0;
      Call (AF.Source_Request (Maximum_Age), Label, Reply, Delivered, Requests);
      if not OK (Label, Delivered) then
         return;
      end if;
      AF.Decode_Source (Reply, Facts, Status, Valid);
      if Valid and then Status = AF.Too_Old then
         Call (AF.Source_Request (AF.Any_Age), Label, Reply, Delivered, Requests);
         if not OK (Label, Delivered) then
            Facts := AF.No_AC;
            return;
         end if;
         AF.Decode_Source (Reply, Facts, Status, Valid);
         if Valid and then Status = AF.Available then
            Facts.Present := True;
            Facts.Fresh := PS.Stale;
            Generation := Reply (0);
            return;
         end if;
      end if;
      if not Valid then
         Facts := AF.No_AC;
         return;
      end if;
      case Status is
         when AF.Available =>
            Facts.Present := True;
            Facts.Fresh := PS.Fresh;
            Generation := Reply (0);
         --  The device is known, its state is not.
         when AF.Failure | AF.Clock_Unknown | AF.Clock_Regressed | AF.Counter_Exhausted | AF.Too_Old =>
            Facts := (Present => True, State => AF.Unknown, Fresh => PS.Unknown);
         --  No ACPI0003 device, or acpi.svc does not know yet.
         when AF.Missing | AF.Not_Ready =>
            Facts := AF.No_AC;
      end case;
   end Read_AC;

   type Battery_Outcome is (Done, Again, Unread);

   procedure Read_Batteries_Once
     (Maximum_Age : AF.Word; Result : in out Reading; Outcome : out Battery_Outcome)
   with Pre => Maximum_Age <= AF.Any_Age
   is
      Label : Unsigned_32;
      Reply : AF.Words;
      Delivered, Valid : Boolean;
      Status : AF.Availability;
      Generation : AF.Word;
      Count : AF.Battery_Count;
      Age : AF.Word := Maximum_Age;
      Fresh : PS.Freshness := PS.Fresh;
   begin
      Result.Count := 0;
      Result.Batteries := [others => AF.Unread_Battery];
      Result.Battery_Generation := 0;
      Outcome := Unread;
      Call (AF.Inventory_Request (Age), Label, Reply, Delivered, Result.Requests);
      if not OK (Label, Delivered) then
         return;
      end if;
      AF.Decode_Inventory (Reply, Generation, Status, Count, Valid);
      if Valid and then Status = AF.Too_Old then
         Age := AF.Any_Age;
         Fresh := PS.Stale;
         Call (AF.Inventory_Request (Age), Label, Reply, Delivered, Result.Requests);
         if not OK (Label, Delivered) then
            return;
         end if;
         AF.Decode_Inventory (Reply, Generation, Status, Count, Valid);
      end if;
      if not Valid or else Status /= AF.Available or else Generation = 0 then
         --  No inventory (none yet, no battery path, or unreadable).
         return;
      end if;
      for Slot in 1 .. Count loop
         declare
            Facts : AF.Battery_Facts := AF.Unread_Battery;
         begin
            Facts.Fresh := Fresh;
            for Page in AF.Entry_Summary .. AF.Dynamic_Voltage loop
               Call (AF.Entry_Request (Age, Slot, Page, Generation), Label, Reply, Delivered, Result.Requests);
               if Stale (Label, Reply, Delivered) then
                  Outcome := Again;
                  return;
               elsif not OK (Label, Delivered) then
                  return;
               elsif Reply (0) /= Generation then
                  Outcome := Again;
                  return;
               end if;
               AF.Decode_Entry (Page, Reply, Facts, Valid);
               if not Valid then
                  --  Not a page acpi.svc writes: this battery is unread.
                  Facts := (Status => AF.Acquisition_Failed, Fresh => PS.Unknown, others => <>);
                  exit;
               end if;
               --  An absent or failed entry's other pages carry nothing.
               exit when Page = AF.Entry_Summary and then Facts.Status /= AF.Sampled;
            end loop;
            Result.Batteries (Slot) := Facts;
         end;
      end loop;
      Result.Count := Count;
      Result.Battery_Generation := Generation;
      Outcome := Done;
   end Read_Batteries_Once;

   procedure Read (Maximum_Age : AF.Word; Result : out Reading; AC_Maximum_Age : AF.Word := 0) is
      Outcome : Battery_Outcome := Again;
   begin
      Result := (others => <>);
      Read_AC ((if AC_Maximum_Age = 0 then Maximum_Age else AC_Maximum_Age), Result.AC, Result.AC_Generation,
               Result.Requests);
      for Attempt in 1 .. Battery_Attempts loop
         Read_Batteries_Once (Maximum_Age, Result, Outcome);
         exit when Outcome /= Again;
      end loop;
      if Outcome /= Done then
         Result.Count := 0;
         Result.Batteries := [others => AF.Unread_Battery];
         Result.Battery_Generation := 0;
      end if;
   end Read;
end Power_ACPI_Reader;
