pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with I2C_Mapping_Policy;
with I2C_Register_Window;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
procedure Window_Tests is
   package P renames I2C_Mapping_Policy;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   use type P.Address;
   use type P.Outcome;
   use type R.Word;
   use type R.Access_Status;
   use type C.Access_Status;
   use type L.Access_Status;
   use type L.Register_ID;
   type Page_Array is array (Natural range 0 .. 1023) of Interfaces.Unsigned_32
     with Volatile_Components;
   Page : Page_Array with Alignment => 4096;
   type Authority_ID is range 0 .. 1;
   Physical : constant P.Address := 16#1234_5000#;
   Virtual : constant P.Address := P.Address (System.Storage_Elements.To_Integer (Page'Address));
   Metadata : P.Description;
   Inspect_Calls, Map_Calls, Checks : Natural := 0;
   Allow_Map : Boolean := True;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Inspect (Authority : Authority_ID; Result : out P.Description) is
   begin
      Check (Authority = 1, "same authority inspected");
      Inspect_Calls := Inspect_Calls + 1; Result := Metadata;
   end Inspect;
   procedure Map
     (Authority : Authority_ID; First, Destination : P.Address; Mapped : out Boolean) is
   begin
      Check (Authority = 1 and then First = Physical and then Destination = Virtual,
             "same authority and validated physical/virtual facts reach mapper");
      Map_Calls := Map_Calls + 1; Mapped := Allow_Map;
   end Map;
   procedure Case_Run (Which : Natural) is
      package W is new I2C_Register_Window (Authority_ID, Inspect, Map);
      use type W.Outcome;
      Report : W.Report;
      Value : R.Word;
      RS : R.Access_Status;
      CS : C.Access_Status;
      LS : L.Access_Status;
      Before : R.Word;
   begin
      Metadata := (True, True, True, True, Physical, P.Page_Bytes);
      Inspect_Calls := 0; Map_Calls := 0; Allow_Map := True;
      Page := [others => 16#A5A5_A5A5#];
      W.Data_Read (R.Component_Type, Value, RS);
      Check (not W.Ready and then RS = R.Unavailable and then Value = 0,
             "no overlay read before successful mapping");
      case Which is
         when 1 => Metadata.Current := False;
         when 2 => Metadata.Device_Memory := False;
         when 3 => Metadata.Readable := False;
         when 4 => Metadata.Writable := False;
         when 5 => Metadata.First := Physical + P.Page_Bytes;
         when 6 => Metadata.Bytes := P.Page_Bytes - 1;
         when 7 => Allow_Map := False;
         when others => null;
      end case;
      W.Bind (1, Physical, P.Page_Bytes, Virtual, Report);
      if Which /= 0 then
         Check (not W.Ready and then Report.Status /= W.Bound, "rejected map never publishes ready");
         Check (Map_Calls = (if Which = 7 then 1 else 0), "policy refusal cannot reach mapper");
         W.Data_Write (R.Write_Data_Command, 16#DEAD#, RS);
         Check (RS = R.Unavailable and then Page (4) = 16#A5A5_A5A5#,
                "no volatile store after refused map");
      else
         Check (W.Ready and then Report.Status = W.Bound and then Map_Calls = 1,
                "one successful map activates named callbacks");
         for ID in R.Read_Register loop
            Page (Natural (R.Offset (ID)) / 4) := 16#1234_5678#;
            W.Data_Read (ID, Value, RS);
            Check (RS = R.Access_OK and then Value = 16#1234_5678#, "data selector exact volatile load");
         end loop;
         for ID in L.Register_ID loop
            Page (Natural (L.Offset (ID)) / 4) := 16#7654_3210#;
            W.Reset_Read (ID, Value, LS);
            Check (LS = L.Access_OK and then Value = 16#7654_3210#, "LPSS selector exact volatile load");
            W.Reset_Write (ID, 16#ABCD#, LS);
            Check ((if ID = L.Capabilities then LS = L.Denied and then
                       Page (Natural (L.Offset (ID)) / 4) = 16#7654_3210#
                    else LS = L.Access_OK and then Page (Natural (L.Offset (ID)) / 4) = 16#ABCD#),
                   "read-only LPSS capabilities cannot be written");
         end loop;
         for ID in C.Register_ID loop
            Page (Natural (C.Offset (ID)) / 4) := 16#55#;
            W.Config_Read (ID, Value, CS);
            case ID is
               when C.Enable_Status | C.Controller_Status | C.TX_Level | C.RX_Level |
                    C.Data_Command | C.Abort_Source | C.Clear_Interrupts =>
                  Check (CS = C.Access_OK and then Value = 16#55#, "configuration read selector");
               when others => Check (CS = C.Denied and then Value = 0, "non-read selector denied and zeroed");
            end case;
            Before := Page (Natural (C.Offset (ID)) / 4);
            W.Config_Write (ID, 16#AA#, CS);
            case ID is
               when C.Interrupt_Mask | C.Enable | C.Standard_High | C.Standard_Low |
                    C.Fast_High | C.Fast_Low | C.SDA_Hold | C.Control | C.Target |
                    C.TX_Threshold | C.RX_Threshold =>
                  Check (CS = C.Access_OK and then Page (Natural (C.Offset (ID)) / 4) = 16#AA#,
                         "configuration write selector");
               when others => Check (CS = C.Denied and then Page (Natural (C.Offset (ID)) / 4) = Before,
                                     "non-write selector preserves memory");
            end case;
         end loop;
         W.Data_Write (R.Write_Data_Command, 16#700#, RS);
         Check (RS = R.Access_OK and then Page (4) = 16#700#, "DATA_CMD exact volatile store");
         Check (Page (1023) = 16#A5A5_A5A5#, "unrelated mapped page tail untouched");
      end if;
      W.Bind (1, Physical, P.Page_Bytes, Virtual, Report);
      Check (Report.Status = W.Already_Attempted and then Inspect_Calls = 1,
             "same owner never rebinds/retries a mapping attempt");
   end Case_Run;
begin
   for Which in 0 .. 7 loop Case_Run (Which); end loop;
   Metadata := (True, True, True, True, Physical, P.Page_Bytes);
   Check (P.Admit (Metadata, 0, P.Page_Bytes, Virtual) = P.Invalid_Physical, "zero physical refused");
   Check (P.Admit (Metadata, Physical + 1, P.Page_Bytes, Virtual) = P.Invalid_Physical, "unaligned physical refused");
   Check (P.Admit (Metadata, P.Address'Last - P.Page_Bytes + 1, P.Page_Bytes, Virtual) = P.Invalid_Physical,
          "end-exclusive physical wrap refused");
   Check (P.Admit (Metadata, Physical, P.Page_Bytes - 1, Virtual) = P.Short_Window, "claimed BAR must cover page");
   Check (P.Admit (Metadata, Physical, P.Page_Bytes, 0) = P.Invalid_Virtual, "zero virtual refused");
   Check (P.Admit (Metadata, Physical, P.Page_Bytes, Virtual + 1) = P.Invalid_Virtual, "unaligned virtual refused");
   Check (P.Admit (Metadata, Physical, P.Page_Bytes, P.User_Virtual_Limit) = P.Invalid_Virtual, "kernel-half virtual refused");
   Check (P.Admit (Metadata, Physical, P.Page_Bytes, P.User_Virtual_Limit - P.Page_Bytes) = P.Allowed,
          "last complete user page fits");
   Ada.Text_IO.Put_Line ("I2C register window checks" & Checks'Image);
end Window_Tests;
