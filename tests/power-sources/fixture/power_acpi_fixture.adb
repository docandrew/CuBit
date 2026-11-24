pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Battery_Status;
with ACPI_Observation_Values;
with ACPI_Cached_Power_Values;
with ACPI_Cached_Battery_Values;
with ACPI_Cached_Power_Codec;
with ACPI_Cached_Power_Dispatch;
with ACPI_Cached_Battery_Codec;
with ACPI_Cached_Battery_Dispatch;
with ACPI_Power_Observation_Protocol;
with ACPI_Battery_Observation_Protocol;
with ACPI_Power_Identity_Protocol;
with ACPI_Source_Paths;

package body Power_ACPI_Fixture is
   package V renames ACPI_Cached_Power_Values;
   package B renames ACPI_Cached_Battery_Values;
   package OV renames ACPI_Observation_Values;
   package BS renames ACPI_Battery_Status;
   use type V.Microseconds;

   --  ACPI_Endpoint.Reply_OK / Reply_Error and ACPI_Requests.Outcome'Pos.
   Reply_OK : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   Denied_Outcome : constant Word := 1;
   Malformed_Outcome : constant Word := 2;
   Stale_Outcome : constant Word := 3;
   Index_Outcome : constant Word := 10;

   --  A sample time: known, never zero.
   Sampled_At : constant V.Microseconds := 1_000_000;

   type Owner is null record;
   The_Owner : constant Owner := (null record);

   Script : Scenario;
   Count_Answered, Count_Inventory, Count_Identity : Natural := 0;
   Change_Countdown : Natural := 0;

   procedure Set (S : Scenario) is
   begin
      Script := S;
      Count_Answered := 0;
      Count_Inventory := 0;
      Count_Identity := 0;
      Change_Countdown := 0;
   end Set;

   function Path_Of (Text : String) return Source_Path is
      Result : Source_Path := (Length => Text'Length, Text => [others => ' ']);
   begin
      Result.Text (1 .. Text'Length) := Text;
      return Result;
   end Path_Of;

   function Identity_Reads return Natural is (Count_Identity);

   function Current return Scenario is (Script);

   procedure Change_After (Requests : Natural) is
   begin
      Change_Countdown := Requests;
   end Change_After;

   function Answered return Natural is (Count_Answered);
   function Inventory_Reads return Natural is (Count_Inventory);

   function Measure (W : Word) return BS.Measurement is
     (if W = Unknown then (Known => False, Value => 0) else (Known => True, Value => BS.Quantity_Value (W)));
   function Amount (W : Word) return B.Quantity is
     (if W = Unknown then (Known => False, Value => 0) else (Known => True, Value => Unsigned_32 (W)));

   function Item (Spec : Battery) return B.Entry_Value is
   begin
      case Spec.Kind is
         when Absent => return (Status => B.Known_Absent, Reason => B.Absent, Value => B.Empty_Sample);
         when Failed => return (Status => B.Acquisition_Failed, Reason => B.Status_Failed, Value => B.Empty_Sample);
         when Sampled =>
            return
              (Status => B.Sampled, Reason => B.No_Failure,
               Value =>
                 (Source => B.Extended_Reported,
                  Units => (case Spec.Unit is
                              when Unspecified => B.Unspecified_Unit,
                              when Energy => B.Energy_And_Power,
                              when Charge => B.Charge_And_Current),
                  Design_Capacity => Amount (Spec.Design),
                  Last_Full_Capacity => Amount (Spec.Last_Full),
                  Design_Voltage => Amount (Spec.Design_Voltage),
                  Dynamic =>
                    (Flow => (case Spec.Direction is
                                when Neither => BS.Neither,
                                when Discharging => BS.Discharging,
                                when Charging => BS.Charging),
                     Critical => Spec.Critical, Charge_Limiting => Spec.Charge_Limiting,
                     Reserved_State_Bits => 0,
                     Rate => Measure (Spec.Rate), Remaining_Capacity => Measure (Spec.Remaining),
                     Voltage => Measure (Spec.Voltage))));
      end case;
   end Item;

   procedure Read_Batteries (Server : Owner; Maximum_Age : V.Microseconds; Value : out B.View) is
      pragma Unreferenced (Server);
   begin
      Value := (Revision => V.Generation (Script.Generation), Status => V.Available,
                Time => (Known => True, Value => Sampled_At), Count => Script.Count, Items => B.Empty_Entries);
      for I in 1 .. Script.Count loop
         Value.Items (I) := Item (Script.Batteries (I));
      end loop;
      if not Script.Batteries_Ready then
         Value.Status := V.Not_Ready;
      elsif V.Microseconds (Script.Battery_Age) > Maximum_Age then
         Value.Status := V.Too_Old;
      end if;
   end Read_Batteries;

   procedure Read_Power (Server : Owner; Maximum_Age : V.Microseconds; Value : out V.Snapshot) is
      pragma Unreferenced (Server);
   begin
      Value := (Lid => (others => <>), Source => (Revision => V.Generation (Script.AC_Generation), Status => V.Available,
                Time => (Known => True, Value => Sampled_At), Value => OV.Empty_Power_Source_Sample));
      case Script.AC is
         when No_Device => Value.Source.Status := V.Missing;
         when Not_Ready => Value.Source.Status := V.Not_Ready;
         when Failed    => Value.Source.Status := V.Failure;
         when Offline   => Value.Source.Value := (State => OV.Offline, Raw => 0);
         when Online    => Value.Source.Value := (State => OV.Online, Raw => 1);
      end case;
      if Script.AC in Offline | Online and then V.Microseconds (Script.AC_Age) > Maximum_Age then
         Value.Source.Status := V.Too_Old;
      end if;
   end Read_Power;

   package Power_Codec is new ACPI_Cached_Power_Codec
     (ACPI_Power_Observation_Protocol.Read_Metrics_Label, ACPI_Power_Observation_Protocol.Lid_Page,
      ACPI_Power_Observation_Protocol.Source_Page);
   package Power_Reader is new ACPI_Cached_Power_Dispatch (Owner, Power_Codec, Read_Power);
   package Battery_Codec is new ACPI_Cached_Battery_Codec
     (ACPI_Battery_Observation_Protocol.Read_Metrics_Label, ACPI_Battery_Observation_Protocol.Battery_Page);
   package Battery_Reader is new ACPI_Cached_Battery_Dispatch (Owner, Battery_Codec, Read_Batteries);

   procedure Answer (Request : Words; Label : out Unsigned_32; Reply : out Words; Handled : out Boolean) is
      Stamp : constant ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
      procedure Error (Outcome : Word) is
      begin
         Label := Reply_Error;
         Reply := [Outcome, 0, 0, 0];
      end Error;
   begin
      Label := Reply_Error;
      Reply := [others => 0];
      Handled := False;
      if Request (0) in ACPI_Battery_Observation_Protocol.Battery_Page | ACPI_Power_Identity_Protocol.Identity_Page
        and then Change_Countdown > 0
      then
         Change_Countdown := Change_Countdown - 1;
         if Change_Countdown = 0 then
            Script.Generation := Script.Generation + 1;
         end if;
      end if;
      if Request (0) = ACPI_Battery_Observation_Protocol.Battery_Page then
         if Request (2) = 0 then
            Count_Inventory := Count_Inventory + 1;
         end if;
         declare
            Result : Battery_Reader.Reply;
         begin
            Battery_Reader.Dispatch
              (The_Owner, Stamp,
               (Label => ACPI_Battery_Observation_Protocol.Read_Metrics_Label, Length => 4, Flags => 0,
                Reserved => 0, Data => Battery_Codec.Words (Request)), Result, Read_Allowed => True);
            case Result.Status is
               when Battery_Reader.Not_Handled => return;
               when Battery_Reader.Denied => Error (Denied_Outcome);
               when Battery_Reader.Malformed => Error (Malformed_Outcome);
               when Battery_Reader.Stale => Error (Stale_Outcome);
               when Battery_Reader.Index_Out_Of_Range => Error (Index_Outcome);
               when Battery_Reader.OK =>
                  Label := Reply_OK;
                  Reply := [for I in Words'Range => Result.Data (I)];
            end case;
         end;
      elsif Request (0) = ACPI_Power_Identity_Protocol.Identity_Page then
         --  A MODEL of ACPI_Native_Power_Identities.Dispatch over
         --  ACPI_Native_Event_Domain.Read_Identity: the same framing checks
         --  and result codes, the path chunked by acpi.svc's own
         --  ACPI_Source_Paths.Chunk.
         Count_Identity := Count_Identity + 1;
         if Request (1) > ACPI_Power_Identity_Protocol.Maximum_Battery_Index
           or else Request (2) > ACPI_Power_Identity_Protocol.Maximum_Path_Characters
           or else Request (3) = 0 or else Request (3) > Word (Integer_64'Last)
         then
            Error (Malformed_Outcome);
         else
            declare
               package IP renames ACPI_Power_Identity_Protocol;
               Index : constant Natural := Natural (Request (1));
               Family : constant Word := (if Index = 0 then Script.AC_Generation else Script.Generation);
               Spec : Source_Path;
               Code : IP.Result_Code := IP.OK;
               Path : ACPI_Source_Paths.Path;
            begin
               Reply := [Family, 0, 0, 0];
               if Index = 0 and then Script.AC = No_Device then
                  Code := IP.Index_Out_Of_Range;
               elsif (Index = 0 and then Script.AC = Not_Ready) or else (Index > 0 and then not Script.Batteries_Ready) then
                  Code := IP.Not_Ready;
               elsif Index > Script.Count then
                  Code := IP.Index_Out_Of_Range;
               elsif Request (3) /= Family then
                  Code := IP.Stale;
               else
                  Spec := (if Index = 0 then Script.AC_Path else Script.Battery_Paths (Index));
                  if Spec.Length = 0 then
                     Code := IP.Not_Ready;
                  elsif Request (2) > Word (Spec.Length) then
                     Code := IP.Bad_Offset;
                  else
                     Path := (State => ACPI_Source_Paths.Known, Length => Spec.Length, Text => [others => ' ']);
                     Path.Text (1 .. Spec.Length) := Spec.Text (1 .. Spec.Length);
                     Reply (2) := Word (Spec.Length);
                     Reply (3) := ACPI_Source_Paths.Chunk (Path, Request (2));
                  end if;
               end if;
               Reply (1) := Word (IP.Result_Code'Enum_Rep (Code));
               Label := Reply_OK;
            end;
         end if;
      elsif Request (0) in ACPI_Power_Observation_Protocol.Lid_Page | ACPI_Power_Observation_Protocol.Source_Page then
         declare
            Result : Power_Reader.Reply;
         begin
            Power_Reader.Dispatch
              (The_Owner, Stamp,
               (Label => ACPI_Power_Observation_Protocol.Read_Metrics_Label, Length => 4, Flags => 0,
                Reserved => 0, Data => Power_Codec.Words (Request)), Result, Read_Allowed => True);
            case Result.Status is
               when Power_Reader.Not_Handled => return;
               when Power_Reader.Denied => Error (Denied_Outcome);
               when Power_Reader.Malformed => Error (Malformed_Outcome);
               when Power_Reader.Stale => Error (Stale_Outcome);
               when Power_Reader.OK =>
                  Label := Reply_OK;
                  Reply := [for I in Words'Range => Result.Data (I)];
            end case;
         end;
      else
         return;
      end if;
      Handled := True;
      Count_Answered := Count_Answered + 1;
   end Answer;
end Power_ACPI_Fixture;
