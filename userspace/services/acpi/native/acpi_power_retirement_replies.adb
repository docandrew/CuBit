pragma Ada_2022;
package body ACPI_Power_Retirement_Replies with SPARK_Mode is
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_8, Word;
   function Refuse (Input : Request; Count : Word; Source : E.Press_Source)
      return Response is
      Result : Response;
      Approved : constant Boolean := Input.Authority = E.Authority_Tag;
      Envelope : constant Boolean := Input.Flags = 0 and then Input.Reserved = 0;
      Sleep_Valid : constant Boolean := Envelope and then Input.Data (3) = 0
        and then E.Sleep_Request_Valid
          (Word (Input.Length), Input.Data (E.Request_Word),
           Input.Data (E.State_Word), Input.Data (E.Confirm_Word));
   begin
      if Input.Label not in E.Button_Watch | E.Prepare_To_Sleep | E.Enter_Sleep_State then
         return Result;
      end if;
      Result.Handled := True; Result.Label := Input.Label;
      Result.Data (E.Reply_Request_Word) := Input.Data (E.Request_Word);
      if Input.Label = E.Button_Watch then
         Result.Length := E.Watch_Reply_Words;
         Result.Data (E.Count_Word) := Count;
         Result.Data (E.Source_Word) := E.Press_Source'Enum_Rep (Source);
         declare
            Status : constant E.Watch_Status :=
              (if not Approved then E.Not_Approved
               elsif not Envelope or else Input.Length /= E.Watch_Request_Words
                 or else Input.Data (E.Request_Word) = 0 then E.Malformed
               else E.Stopping);
         begin Result.Data (E.Watch_Status_Word) := E.Watch_Status'Enum_Rep (Status); end;
      elsif Input.Label = E.Prepare_To_Sleep then
         Result.Length := E.Prepare_Reply_Words;
         declare
            Status : constant E.Prepare_Status :=
              (if not Approved then E.Not_Approved
               elsif not Sleep_Valid then E.Malformed else E.Not_Ready);
         begin Result.Data (E.Prepare_Status_Word) := E.Prepare_Status'Enum_Rep (Status); end;
      else
         Result.Length := E.Enter_Reply_Words;
         declare
            Status : constant E.Enter_Status :=
              (if not Approved then E.Enter_Not_Approved
               elsif not Sleep_Valid then E.Enter_Malformed else E.Not_Prepared);
         begin Result.Data (E.Enter_Status_Word) := E.Enter_Status'Enum_Rep (Status); end;
      end if;
      return Result;
   end Refuse;
end ACPI_Power_Retirement_Replies;
