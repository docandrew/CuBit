pragma Ada_2022;
with ACPI_Passive_Query.Replies;
package body ACPI_Observer_Loans with SPARK_Mode is
   procedure Retry_Return (Item : in out State; Owner : in out Context; Released : out Boolean) is
   begin
      Released := not Item.Held;
      if not Item.Held then return; end if;
      Return_Acquisition (Owner, Item.Loan, Item.Acquired, Released);
      if Released then Item := (Held => False, Loan => No_Reference, Acquired => No_Mapping); end if;
   end Retry_Return;
   procedure Handle
     (Item : in out State; Owner : in out Context; Stamp : ACPI_Protocol.Role_Tag;
      Sender : Sender_ID; Loan : Reference; Declared_Length : Wire.Word;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report) is
      Accepted : Boolean;
      procedure Finish is
      begin
         Result.Release_Attempted := Item.Held;
         Retry_Return (Item, Owner, Result.Released);
         Result.May_Reply := not Item.Held;
         if Result.Status /= Completed then Response_Data := [others => 0]; Result.Written := 0; end if;
      end Finish;
   begin
      Result := (others => <>); Request_Data := [others => 0]; Response_Data := [others => 0];
      Result.May_Reply := not Item.Held;
      if not ACPI_Protocol.Is_Observer (Stamp) then Result.Status := Denied; return; end if;
      if Item.Held then Result.Status := Busy; return; end if;
      if not Admits_Extent (Declared_Length, Wire.Word (Request_Data'Length), Wire.Word (Response_Data'Length))
      then Result.Required_Bytes := Pages.Frame_Byte_Count; return; end if;
      declare
         Length : constant Natural := Natural (Declared_Length);
      begin
         -- State the prefix bounds in the array index domain as well as the
         -- wire-size domain before constructing callback views.
         if Length = 0 or else Request_Data'Length = 0 or else Response_Data'Length = 0
           or else Length - 1 > Request_Data'Last - Request_Data'First
           or else Length - 1 > Response_Data'Last - Response_Data'First
         then Result.Status := Invalid_Extent; return; end if;
      declare
         Request_Part : Pages.Byte_Array renames
           Request_Data (Request_Data'First .. Request_Data'First + (Length - 1));
         Response_Part : Pages.Byte_Array renames
           Response_Data (Response_Data'First .. Response_Data'First + (Length - 1));
      begin
      Acquire_Writable (Owner, Loan, Sender, Declared_Length, Item.Acquired, Accepted);
      if not Accepted then Item.Acquired := No_Mapping; Result.Status := Acquire_Refused; return; end if;
      Item.Held := True; Item.Loan := Loan;
      Copy_In (Owner, Item.Acquired, Request_Part, Accepted);
      if not Accepted then Request_Data := [others => 0]; Result.Status := Copy_In_Failed; Finish; return; end if;
      Evaluate (Owner, Request_Part, Response_Part,
        Result.Written, Result.Required_Bytes, Accepted);
      if not Accepted then Result.Status := Evaluation_Refused; Finish; return; end if;
      if Result.Written < ACPI_Passive_Query.Replies.Header_Byte_Count or else Result.Written > Declared_Length then
         Result.Status := Invalid_Response; Finish; return;
      end if;
      -- Normalize callback padding before exposing the entire loan extent.
      for I in Response_Part'Range loop
         if Wire.Word (I - Response_Part'First) >= Result.Written then Response_Part (I) := 0; end if;
      end loop;
      declare
         Header : constant ACPI_Passive_Query.Replies.Header_Result := ACPI_Passive_Query.Replies.Read_Envelope
           (Response_Part, Wire.Node_Limit'Last);
      begin
         if not Header.Accepted or else ACPI_Passive_Query.Replies.Extent (Header.Value) /= Result.Written
           or else not ACPI_Passive_Query.Replies.Matches_Request
             (Response_Part, Request_Part, Wire.Node_Limit'Last)
         then Result.Status := Invalid_Response; Finish; return; end if;
      end;
      Copy_Out (Owner, Item.Acquired, Response_Part, Accepted);
      Result.Status := (if Accepted then Completed else Copy_Out_Failed);
      Finish;
      end;
      end;
   end Handle;
end ACPI_Observer_Loans;
