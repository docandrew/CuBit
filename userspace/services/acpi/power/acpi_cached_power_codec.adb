pragma Ada_2022;
package body ACPI_Cached_Power_Codec with SPARK_Mode is
   package V renames ACPI_Cached_Power_Values;
   package O renames ACPI_Observation_Values;
   function Inspect (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Selection is
      Page : Subpage;
   begin
      if Request.Label /= Read_Label or else Request.Data (Page_Word) not in Lid_Page | Source_Page then
         return (Kind => Not_Handled);
      elsif not ACPI_Protocol.Is_Observer (Stamp) then return (Kind => Denied);
      elsif Request.Length /= Request_Length or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (Subpage_Word) not in Summary_Page | Time_Page | Raw_Page
        or else Request.Data (Expected_Generation_Word) > Word (V.Generation'Last)
        or else (Request.Data (Subpage_Word) /= Summary_Page and then Request.Data (Expected_Generation_Word) = 0)
      then return (Kind => Malformed);
      end if;
      Page := (case Request.Data (Subpage_Word) is
         when Summary_Page => Summary, when Time_Page => Sample_Time, when others => Raw_Value);
      if Request.Data (Page_Word) = Lid_Page then
         return (Read_Lid, V.Microseconds (Request.Data (Maximum_Age_Word)), Page,
           V.Generation (Request.Data (Expected_Generation_Word)));
      else return (Read_Source, V.Microseconds (Request.Data (Maximum_Age_Word)), Page,
           V.Generation (Request.Data (Expected_Generation_Word)));
      end if;
   end Inspect;
   function Code (Status : V.Availability) return Reply_Word is
     (case Status is
        when V.Not_Ready => Not_Ready_Code, when V.Available => Available_Code,
        when V.Missing => Missing_Code, when V.Failure => Failure_Code,
        when V.Too_Old => Too_Old_Code, when V.Clock_Unknown => Clock_Unknown_Code,
        when V.Clock_Regressed => Clock_Regressed_Code, when V.Counter_Exhausted => Counter_Exhausted_Code);
   function Low (Value : Word) return Reply_Word is (Value and Half_Word_Mask);
   function High (Value : Word) return Reply_Word is (Interfaces.Shift_Right (Value, Half_Word_Bits));
   function Make (Revision : V.Generation; Time : V.Sample_Time; Status, State : Reply_Word;
      Raw : Word; Page : Subpage) return Reply_Words
     with Post => Make'Result (0) = Word (Revision)
       and then (case Page is
         when Summary => Make'Result (1) = Status and then Make'Result (2) = State
           and then Make'Result (3) = Boolean'Pos (Time.Known),
         when Sample_Time => Make'Result (1) = Boolean'Pos (Time.Known)
           and then Make'Result (2) <= Half_Word_Mask and then Make'Result (3) <= Half_Word_Mask
           and then Join (Make'Result (2), Make'Result (3)) = (if Time.Known then Word (Time.Value) else 0),
         when Raw_Value => Make'Result (1) = Status
           and then Make'Result (2) <= Half_Word_Mask and then Make'Result (3) <= Half_Word_Mask
           and then Join (Make'Result (2), Make'Result (3)) = (if Status = Available_Code then Raw else 0))
   is
      Known : constant Reply_Word := Boolean'Pos (Time.Known);
   begin
      case Page is
         when Summary => return [Word (Revision), Status, State, Known];
         when Sample_Time => return [Word (Revision), Known,
            (if Time.Known then Low (Word (Time.Value)) else 0),
            (if Time.Known then High (Word (Time.Value)) else 0)];
         when Raw_Value => return [Word (Revision), Status,
            (if Status = Available_Code then Low (Raw) else 0),
            (if Status = Available_Code then High (Raw) else 0)];
      end case;
   end Make;
   function Encode (Value : V.Lid_View; Page : Subpage) return Reply_Words is
      Result_Code : constant Reply_Word :=
        (if Value.Status /= V.Available then Code (Value.Status)
         elsif not Value.Time.Known then Clock_Unknown_Code
         elsif (Value.Value.State = O.Closed and then Value.Value.Raw = 0)
           or else (Value.Value.State = O.Open and then Value.Value.Raw /= 0)
         then Available_Code else Failure_Code);
      State : constant Reply_Word := (if Result_Code /= Available_Code then Unknown_State
        else (case Value.Value.State is when O.Unknown => Unknown_State,
           when O.Closed => Closed_State, when O.Open => Open_State));
   begin return Make (Value.Revision, Value.Time, Result_Code, State, Value.Value.Raw, Page); end Encode;
   function Encode (Value : V.Source_View; Page : Subpage) return Reply_Words is
      Result_Code : constant Reply_Word :=
        (if Value.Status /= V.Available then Code (Value.Status)
         elsif not Value.Time.Known then Clock_Unknown_Code
         elsif (Value.Value.State = O.Offline and then Value.Value.Raw = 0)
           or else (Value.Value.State = O.Online and then Value.Value.Raw = 1)
         then Available_Code else Failure_Code);
      State : constant Reply_Word := (if Result_Code /= Available_Code then Unknown_State
        else (case Value.Value.State is when O.Unknown => Unknown_State,
           when O.Offline => Offline_State, when O.Online => Online_State));
   begin return Make (Value.Revision, Value.Time, Result_Code, State, Value.Value.Raw, Page); end Encode;
end ACPI_Cached_Power_Codec;
