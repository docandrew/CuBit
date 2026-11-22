pragma Ada_2022;
package body ACPI_Query_Sessions with SPARK_Mode is
   procedure Deliver (Item : in out State; Owner : in out Loans.Backend_Context; Result : out Report) is
      Delivered : Boolean;
   begin
      Result := (others => <>); Result.Operation := Item.Pending;
      Answer (Owner, Item.Handle, Item.Pending, Delivered);
      Item.Saved := False; Item.Handle := No_Reply; Item.Pending := (others => <>);
      Result.Status := (if Delivered then Answered else Delivery_Lost);
   end Deliver;
   procedure Begin_Request
     (Item : in out State; Owner : in out Loans.Backend_Context; Stamp : ACPI_Protocol.Role_Tag;
      Sender : Loans.Peer_Identity; Loan : Loans.Loan_Reference; Declared_Length : Wire.Word;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report) is
      Accepted : Boolean;
   begin
      Result := (others => <>); Request_Data := [others => 0]; Response_Data := [others => 0];
      if not ACPI_Protocol.Is_Observer (Stamp) then Result.Status := Denied; Result.Immediate_Reply := True; return; end if;
      if not Ready (Item) then Result.Status := Busy; Result.Immediate_Reply := True; return; end if;
      if not Loans.Admits_Extent (Declared_Length, Wire.Word (Request_Data'Length), Wire.Word (Response_Data'Length)) then
         Result.Status := Invalid_Extent; Result.Immediate_Reply := True; return;
      end if;
      Save_Reply (Owner, Item.Handle, Accepted);
      if not Accepted then Item.Handle := No_Reply; Result.Status := Save_Refused; Result.Immediate_Reply := True; return; end if;
      Item.Saved := True;
      Loans.Handle (Item.Loan_State, Owner, Stamp, Sender, Loan, Declared_Length, Request_Data, Response_Data, Item.Pending);
      if Item.Pending.May_Reply then Deliver (Item, Owner, Result);
      else Result.Status := Deferred; Result.Operation := Item.Pending; end if;
   end Begin_Request;
   procedure Progress (Item : in out State; Owner : in out Loans.Backend_Context; Result : out Report) is
      Released : Boolean;
   begin
      Result := (others => <>);
      if not Item.Saved then return; end if;
      Loans.Retry_Return (Item.Loan_State, Owner, Released);
      if Released then Item.Pending.May_Reply := True; Item.Pending.Released := True; Deliver (Item, Owner, Result);
      else Result.Status := Deferred; Result.Operation := Item.Pending; end if;
   end Progress;
end ACPI_Query_Sessions;
