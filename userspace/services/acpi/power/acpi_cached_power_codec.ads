pragma Ada_2022;
with Interfaces;
with ACPI_Protocol;
with ACPI_Observation_Values;
with ACPI_Cached_Power_Values;
generic
   Read_Label : Interfaces.Unsigned_32;
   Lid_Page, Source_Page : Interfaces.Unsigned_64;
package ACPI_Cached_Power_Codec with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_16, Interfaces.Unsigned_8;
   use type ACPI_Cached_Power_Values.Availability;
   use type ACPI_Cached_Power_Values.Generation, ACPI_Cached_Power_Values.Microseconds;
   use type ACPI_Observation_Values.Lid_State, ACPI_Observation_Values.Power_Source_State;
   pragma Compile_Time_Error (Lid_Page = Source_Page, "cached power pages must differ");
   subtype Word is Interfaces.Unsigned_64;
   subtype Word_Index is Natural range 0 .. 3;
   type Words is array (Word_Index) of Word;
   Empty_Words : constant Words := [others => 0];
   Maximum_Reply_Word : constant Word := Word (Interfaces.Integer_64'Last);
   subtype Reply_Word is Word range 0 .. Maximum_Reply_Word;
   type Reply_Words is array (Word_Index) of Reply_Word;
   Empty_Reply : constant Reply_Words := [others => 0];
   Page_Word : constant Word_Index := 0;
   Maximum_Age_Word : constant Word_Index := 1;
   Subpage_Word : constant Word_Index := 2;
   Expected_Generation_Word : constant Word_Index := 3;
   Request_Length : constant Interfaces.Unsigned_8 := Words'Length;
   type Packet is record
      Label : Interfaces.Unsigned_32 := 0;
      Length, Flags : Interfaces.Unsigned_8 := 0;
      Reserved : Interfaces.Unsigned_16 := 0;
      Data : Words := Empty_Words;
   end record;
   type Subpage is (Summary, Sample_Time, Raw_Value);
   Summary_Page : constant Word := 0;
   Time_Page : constant Word := 1;
   Raw_Page : constant Word := 2;
   type Decision is (Not_Handled, Denied, Malformed, Read_Lid, Read_Source);
   type Selection (Kind : Decision := Not_Handled) is record
      case Kind is
         when Read_Lid | Read_Source =>
            Maximum_Age : ACPI_Cached_Power_Values.Microseconds := 0;
            Page : Subpage := Summary;
            Expected : ACPI_Cached_Power_Values.Generation := 0;
         when others => null;
      end case;
   end record;
   -- Stamp is the actual receive's kernel-stamped authorityTag, not packet
   -- words or a PID. Auxiliary pages require a nonzero expected generation.
   function Expected_Decision (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Decision is
     (if Request.Label /= Read_Label or else Request.Data (Page_Word) not in Lid_Page | Source_Page then Not_Handled
      elsif not ACPI_Protocol.Is_Observer (Stamp) then Denied
      elsif Request.Length /= Request_Length or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (Subpage_Word) not in Summary_Page | Time_Page | Raw_Page
        or else Request.Data (Expected_Generation_Word) > Word (ACPI_Cached_Power_Values.Generation'Last)
        or else (Request.Data (Subpage_Word) /= Summary_Page and then Request.Data (Expected_Generation_Word) = 0)
      then Malformed elsif Request.Data (Page_Word) = Lid_Page then Read_Lid else Read_Source)
     with Ghost, Global => null;
   function Inspect (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Selection
     with Global => null,
       Post => Inspect'Result.Kind = Expected_Decision (Request, Stamp)
         and then (if Inspect'Result.Kind in Read_Lid | Read_Source then
           Inspect'Result.Maximum_Age = ACPI_Cached_Power_Values.Microseconds (Request.Data (Maximum_Age_Word))
           and then Inspect'Result.Expected = ACPI_Cached_Power_Values.Generation (Request.Data (Expected_Generation_Word))
           and then Subpage'Pos (Inspect'Result.Page) = Request.Data (Subpage_Word));
   Not_Ready_Code : constant Reply_Word := 0;
   Available_Code : constant Reply_Word := 1;
   Missing_Code : constant Reply_Word := 2;
   Failure_Code : constant Reply_Word := 3;
   Too_Old_Code : constant Reply_Word := 4;
   Clock_Unknown_Code : constant Reply_Word := 5;
   Clock_Regressed_Code : constant Reply_Word := 6;
   Counter_Exhausted_Code : constant Reply_Word := 7;
   Unknown_State : constant Reply_Word := 0;
   Closed_State : constant Reply_Word := 1;
   Open_State : constant Reply_Word := 2;
   Offline_State : constant Reply_Word := 1;
   Online_State : constant Reply_Word := 2;
   -- Summary [generation,availability,state,time-known].
   -- Time [generation,time-known,time-low32,time-high32].
   -- Raw [generation,current-availability,raw-low32,raw-high32].
   -- All reply words fit CCL signed integers, with lossless64bit halves.
   -- The owner checks Selected.Expected before using an encoding. Generation
   -- is scoped to one owner/session; availability may vary with clock/age.
   Half_Word_Bits : constant := Interfaces.Unsigned_32'Size;
   Half_Word_Mask : constant Word := Word (Interfaces.Unsigned_32'Last);
   function Join (Low, High : Reply_Word) return Word is
     (Low or Interfaces.Shift_Left (High, Half_Word_Bits)) with Ghost, Global => null;
   function Status_Code (Status : ACPI_Cached_Power_Values.Availability) return Reply_Word is
     (case Status is
        when ACPI_Cached_Power_Values.Not_Ready => Not_Ready_Code,
        when ACPI_Cached_Power_Values.Available => Available_Code,
        when ACPI_Cached_Power_Values.Missing => Missing_Code,
        when ACPI_Cached_Power_Values.Failure => Failure_Code,
        when ACPI_Cached_Power_Values.Too_Old => Too_Old_Code,
        when ACPI_Cached_Power_Values.Clock_Unknown => Clock_Unknown_Code,
        when ACPI_Cached_Power_Values.Clock_Regressed => Clock_Regressed_Code,
        when ACPI_Cached_Power_Values.Counter_Exhausted => Counter_Exhausted_Code)
     with Ghost, Global => null;
   function Observation_Code (Value : ACPI_Cached_Power_Values.Lid_View) return Reply_Word is
     (if Value.Status /= ACPI_Cached_Power_Values.Available then Status_Code (Value.Status)
      elsif not Value.Time.Known then Clock_Unknown_Code
      elsif (Value.Value.State = ACPI_Observation_Values.Closed and then Value.Value.Raw = 0)
        or else (Value.Value.State = ACPI_Observation_Values.Open and then Value.Value.Raw /= 0)
      then Available_Code else Failure_Code) with Ghost, Global => null;
   function Observation_State (Value : ACPI_Cached_Power_Values.Lid_View) return Reply_Word is
     (if Observation_Code (Value) /= Available_Code then Unknown_State
      elsif Value.Value.State = ACPI_Observation_Values.Closed then Closed_State
      else Open_State) with Ghost, Global => null;
   function Encode (Value : ACPI_Cached_Power_Values.Lid_View; Page : Subpage) return Reply_Words
     with Global => null,
       Post => Encode'Result (0) = Word (Value.Revision)
         and then (case Page is
           when Summary => Encode'Result (1) = Observation_Code (Value)
             and then Encode'Result (2) = Observation_State (Value)
             and then Encode'Result (3) = Boolean'Pos (Value.Time.Known),
           when Sample_Time => Encode'Result (1) = Boolean'Pos (Value.Time.Known)
             and then Encode'Result (2) <= Half_Word_Mask and then Encode'Result (3) <= Half_Word_Mask
             and then Join (Encode'Result (2), Encode'Result (3)) = (if Value.Time.Known then Word (Value.Time.Value) else 0),
           when Raw_Value => Encode'Result (1) = Observation_Code (Value)
             and then Encode'Result (2) <= Half_Word_Mask and then Encode'Result (3) <= Half_Word_Mask
             and then Join (Encode'Result (2), Encode'Result (3)) =
               (if Observation_Code (Value) = Available_Code then Value.Value.Raw else 0));
   function Observation_Code (Value : ACPI_Cached_Power_Values.Source_View) return Reply_Word is
     (if Value.Status /= ACPI_Cached_Power_Values.Available then Status_Code (Value.Status)
      elsif not Value.Time.Known then Clock_Unknown_Code
      elsif (Value.Value.State = ACPI_Observation_Values.Offline and then Value.Value.Raw = 0)
        or else (Value.Value.State = ACPI_Observation_Values.Online and then Value.Value.Raw = 1)
      then Available_Code else Failure_Code) with Ghost, Global => null;
   function Observation_State (Value : ACPI_Cached_Power_Values.Source_View) return Reply_Word is
     (if Observation_Code (Value) /= Available_Code then Unknown_State
      elsif Value.Value.State = ACPI_Observation_Values.Offline then Offline_State
      else Online_State) with Ghost, Global => null;
   function Encode (Value : ACPI_Cached_Power_Values.Source_View; Page : Subpage) return Reply_Words
     with Global => null,
       Post => Encode'Result (0) = Word (Value.Revision)
         and then (case Page is
           when Summary => Encode'Result (1) = Observation_Code (Value)
             and then Encode'Result (2) = Observation_State (Value)
             and then Encode'Result (3) = Boolean'Pos (Value.Time.Known),
           when Sample_Time => Encode'Result (1) = Boolean'Pos (Value.Time.Known)
             and then Encode'Result (2) <= Half_Word_Mask and then Encode'Result (3) <= Half_Word_Mask
             and then Join (Encode'Result (2), Encode'Result (3)) = (if Value.Time.Known then Word (Value.Time.Value) else 0),
           when Raw_Value => Encode'Result (1) = Observation_Code (Value)
             and then Encode'Result (2) <= Half_Word_Mask and then Encode'Result (3) <= Half_Word_Mask
             and then Join (Encode'Result (2), Encode'Result (3)) =
               (if Observation_Code (Value) = Available_Code then Value.Value.Raw else 0));
end ACPI_Cached_Power_Codec;
