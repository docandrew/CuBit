pragma Ada_2022;
with Ada.Text_IO; with Interfaces; with ACPI_Power_Retirement_Replies;
procedure Tests is
   package R renames ACPI_Power_Retirement_Replies;
   package E renames R.E;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_32, R.Word, R.Words;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   ID : constant R.Word := 16#F123_4567_89AB_CDEF#;
   Count : constant R.Word := 16#D234_5678_9ABC_DEF0#;
   function Valid (Label : Interfaces.Unsigned_32) return R.Request is
      Input : R.Request;
   begin
      Input.Label := Label; Input.Authority := E.Authority_Tag; Input.Data (0) := ID;
      if Label = E.Button_Watch then Input.Length := E.Watch_Request_Words;
      else Input.Length := E.Sleep_Request_Words; Input.Data (E.State_Word) := E.Sleep_State'Enum_Rep (E.S5);
         Input.Data (E.Confirm_Word) := E.Sleep_Confirmation;
      end if;
      return Input;
   end Valid;
   Labels : constant array (1 .. 3) of Interfaces.Unsigned_32 :=
     [E.Button_Watch, E.Prepare_To_Sleep, E.Enter_Sleep_State];
   procedure Expect (Input : R.Request; Kind : Natural) is
      Reply : constant R.Response := R.Refuse (Input, Count, E.Intel_HID);
      Status : R.Word;
      Base : constant R.Request := Valid (Input.Label);
   begin
      Check (Reply.Handled and Reply.Label = Input.Label and Reply.Data (0) = Input.Data (0));
      if Input.Label = E.Button_Watch then
         Status := (case Kind is when 0 => E.Watch_Status'Enum_Rep (E.Stopping),
           when 1 => E.Watch_Status'Enum_Rep (E.Not_Approved), when others => E.Watch_Status'Enum_Rep (E.Malformed));
         Check (Reply.Length = E.Watch_Reply_Words and Reply.Data (1) = Status);
         Check (Reply.Data (2) = Count and Reply.Data (3) = E.Press_Source'Enum_Rep (E.Intel_HID));
      elsif Input.Label = E.Prepare_To_Sleep then
         Status := (case Kind is when 0 => E.Prepare_Status'Enum_Rep (E.Not_Ready),
           when 1 => E.Prepare_Status'Enum_Rep (E.Not_Approved), when others => E.Prepare_Status'Enum_Rep (E.Malformed));
         Check (Reply.Length = E.Prepare_Reply_Words and Reply.Data (1) = Status);
         Check (Reply.Data (2) = 0 and Reply.Data (3) = 0);
      else
         Status := (case Kind is when 0 => E.Enter_Status'Enum_Rep (E.Not_Prepared),
           when 1 => E.Enter_Status'Enum_Rep (E.Enter_Not_Approved), when others => E.Enter_Status'Enum_Rep (E.Enter_Malformed));
         Check (Reply.Length = E.Enter_Reply_Words and Reply.Data (1) = Status);
         Check (Reply.Data (2) = 0 and Reply.Data (3) = 0);
      end if;
      Check (Base.Authority = E.Authority_Tag);
   end Expect;
   Input : R.Request;
begin
   for Label of Labels loop
      Input := Valid (Label); Expect (Input, 0);
      for Length in Interfaces.Unsigned_8 loop
         Input := Valid (Label); Input.Length := Length;
         Expect (Input, (if Length = Valid (Label).Length then 0 else 2));
      end loop;
      for Flags in Interfaces.Unsigned_8 loop
         Input := Valid (Label); Input.Flags := Flags; Expect (Input, (if Flags = 0 then 0 else 2));
      end loop;
      Input := Valid (Label); Input.Reserved := 1; Expect (Input, 2);
      Input := Valid (Label); Input.Data (0) := 0; Expect (Input, 2);
      Input := Valid (Label); Input.Authority := 0; Expect (Input, 1);
      Input.Flags := 255; Input.Length := 0; Expect (Input, 1);
      Input := Valid (Label); Input.Authority := R.Word'Last; Expect (Input, 1);
      if Label /= E.Button_Watch then
         Input := Valid (Label); Input.Data (E.Confirm_Word) := 0; Expect (Input, 2);
         Input := Valid (Label); Input.Data (E.State_Word) := 0; Expect (Input, 2);
         Input := Valid (Label); Input.Data (3) := 1; Expect (Input, 2);
         for State in E.Sleep_State loop
            Input := Valid (Label); Input.Data (E.State_Word) := E.Sleep_State'Enum_Rep (State); Expect (Input, 0);
         end loop;
      end if;
   end loop;
   for Source in E.Press_Source loop
      Input := Valid (E.Button_Watch);
      declare Reply : constant R.Response := R.Refuse (Input, R.Word'Last, Source); begin
         Check (Reply.Data (2) = R.Word'Last and Reply.Data (3) = E.Press_Source'Enum_Rep (Source));
      end;
   end loop;
   Input := (others => <>);
   declare Reply : constant R.Response := R.Refuse (Input, Count, E.Intel_HID); begin
      Check (not Reply.Handled and Reply.Label = 0 and Reply.Length = 0 and Reply.Data = R.Words'[others => 0]);
   end;
   Ada.Text_IO.Put_Line ("ACPI retirement reply checks " & Checks'Image);
end Tests;
