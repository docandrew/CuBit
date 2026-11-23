package body ACPI_CMOS_Transactions with SPARK_Mode is
   use type I.Decision_Kind, P.Route_Kind;
   procedure Initialize
     (Owner : in out State; Profile : P.Profile; Century : I.Byte;
      Handover_Known, NMI_Disabled, Standard_Granted, Extended_Granted : Boolean;
      Result : out Initialization)
   is
   begin
      Result := Already_Attempted;
      if Owner.Stage /= Fresh then return; end if;
      Owner.Stage := Failed;
      if not Handover_Known then Result := Unknown_Handover; return; end if;
      if not P.Supported (Profile) then Result := Unsupported_Profile; return; end if;
      if Century > P.Last_Cell (Profile) then Result := Invalid_Century; return; end if;
      if not Standard_Granted
        or else (Profile.Device = P.Intel_Compatible and then Profile.Extended_Enabled
                 and then not Extended_Granted)
      then
         Result := Missing_Grant; return;
      end if;
      Owner := (Stage => Ready, Device => Profile, Century => Century,
        Known_NMI => True, NMI_Disabled => NMI_Disabled,
        Standard => Standard_Granted, Extended => Extended_Granted, Sequence => 0);
      Result := Initialized;
   end Initialize;

   procedure Begin_Access
     (Owner : in out State; Base, Length, Offset : I.Word;
      For_Write : Boolean; Value : I.Byte;
      Plan : out Transfer; Result : out Start_Result)
   is
   begin
      Plan := (others => <>); Result := Not_Ready;
      if Owner.Stage /= Ready then return; end if;
      if Owner.Sequence = I.Word'Last then Result := Ticket_Limit; return; end if;
      declare
         Decision : constant I.Decision := I.Resolve
           (Base, Length, Offset, P.Last_Cell (Owner.Device), Owner.Century, For_Write);
      begin
         if Decision.Kind /= I.Accepted then
            Result := (if Decision.Kind = I.Read_Only_Cell then Read_Only else Invalid_Extent);
            return;
         end if;
         declare
            Route : constant P.Route := P.Locate (Owner.Device, Decision.Cell);
         begin
            if Route.Kind /= P.Ready then Result := Invalid_Extent; return; end if;
            Owner.Sequence := Owner.Sequence + 1;
            Plan := (Owner.Sequence, Route.Pair,
              I.Selector (Route.Index, Owner.NMI_Disabled), For_Write, Value);
            Owner.Stage := In_Flight;
            Result := Started;
         end;
      end;
   end Begin_Access;

   procedure Finish_Access
     (Owner : in out State; Ticket : I.Word; Outcome : Completion;
      Result : out Finish_Result)
   is
   begin
      Result := Wrong_Operation;
      if Owner.Stage /= In_Flight or else Ticket /= Owner.Sequence then return; end if;
      Owner.Stage := (if Outcome = Unknown then Uncertain else Ready);
      Result := Finished;
   end Finish_Access;

   procedure Stop (Owner : in out State; Accepted : out Boolean) is
   begin
      Accepted := Owner.Stage not in In_Flight | Uncertain;
      if Accepted then Owner.Stage := Stopped; end if;
   end Stop;
end ACPI_CMOS_Transactions;
