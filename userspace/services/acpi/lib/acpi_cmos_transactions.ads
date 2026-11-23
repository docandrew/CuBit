pragma Ada_2022;
with ACPI_CMOS_Profiles;

-- Pure owner protocol. Transfer plans are not capabilities: the native port
-- interface must hold the exact pair, run select/data without AML reentry,
-- and report its actual completion. Unknown completion pins this owner.
package ACPI_CMOS_Transactions with SPARK_Mode, Pure is
   package P renames ACPI_CMOS_Profiles;
   package I renames P.I;
   use type I.Word, I.Byte, P.Port_Pair, P.Device_Kind;
   type Phase is (Fresh, Ready, In_Flight, Uncertain, Failed, Stopped);
   type State is private with Default_Initial_Condition => Valid (State);
   function Current (Owner : State) return Phase;
   function Valid (Owner : State) return Boolean;
   function Serial (Owner : State) return I.Word;
   function NMI_Bit (Owner : State) return Boolean;
   function Century_Index (Owner : State) return I.Byte;
   type Initialization is
     (Initialized, Already_Attempted, Unknown_Handover, Unsupported_Profile,
      Invalid_Century, Missing_Grant);
   procedure Initialize
     (Owner : in out State; Profile : P.Profile; Century : I.Byte;
      Handover_Known, NMI_Disabled, Standard_Granted, Extended_Granted : Boolean;
      Result : out Initialization)
     with Pre => Valid (Owner),
       Post => Valid (Owner)
         and then (if Current (Owner'Old) /= Fresh then
           Owner = Owner'Old and then Result = Already_Attempted)
         and then (if Result = Initialized then
           Current (Owner) = Ready and then Serial (Owner) = 0
           and then NMI_Bit (Owner) = NMI_Disabled
           elsif Current (Owner'Old) = Fresh then Current (Owner) = Failed);

   type Transfer is record
      Ticket : I.Word := 0;
      Pair : P.Port_Pair := P.Standard_Pair;
      Selector : I.Byte := 0;
      For_Write : Boolean := False;
      Value : I.Byte := 0;
   end record;
   function Logical_Cell (Plan : Transfer) return I.Byte is
     ((Plan.Selector and 16#7F#) + (if Plan.Pair = P.Extended_Pair then 128 else 0));
   type Start_Result is (Started, Not_Ready, Invalid_Extent, Read_Only, Ticket_Limit);
   procedure Begin_Access
     (Owner : in out State; Base, Length, Offset : I.Word;
      For_Write : Boolean; Value : I.Byte;
      Plan : out Transfer; Result : out Start_Result)
     with Pre => Valid (Owner),
       Post => Valid (Owner)
         and then (if Result = Started then
           Current (Owner'Old) = Ready and then Current (Owner) = In_Flight
           and then Serial (Owner'Old) < I.Word'Last
           and then Serial (Owner) = Serial (Owner'Old) + 1
           and then Plan.Ticket = Serial (Owner) and then Plan.Ticket /= 0
           and then Plan.For_Write = For_Write and then Plan.Value = Value
           and then I.Word (Logical_Cell (Plan)) = Base + Offset
           and then (not For_Write or else not I.Read_Only (Logical_Cell (Plan), Century_Index (Owner)))
           and then ((Plan.Selector and 16#80#) /= 0) = NMI_Bit (Owner)
           else Owner = Owner'Old and then Plan.Ticket = 0)
         and then (if Current (Owner'Old) /= Ready then Result = Not_Ready);

   type Completion is (Completed, Refused, Unknown);
   type Finish_Result is (Finished, Wrong_Operation);
   procedure Finish_Access
     (Owner : in out State; Ticket : I.Word; Outcome : Completion;
      Result : out Finish_Result)
     with Pre => Valid (Owner),
       Post => Valid (Owner) and then Serial (Owner) = Serial (Owner'Old)
         and then (if Current (Owner'Old) = In_Flight and then Ticket = Serial (Owner'Old)
           then Result = Finished
             and then Current (Owner) = (if Outcome = Unknown then Uncertain else Ready)
           else Result = Wrong_Operation and then Owner = Owner'Old);
   -- No reset/reinitialize/retry API can discard an uncertain transfer.
   procedure Stop (Owner : in out State; Accepted : out Boolean)
     with Pre => Valid (Owner),
       Post => Valid (Owner)
         and then (if Current (Owner'Old) in In_Flight | Uncertain then
           not Accepted and then Owner = Owner'Old
           else Accepted and then Current (Owner) = Stopped);
private
   type State is record
      Stage : Phase := Fresh;
      Device : P.Profile;
      Century : I.Byte := 0;
      Known_NMI, NMI_Disabled, Standard, Extended : Boolean := False;
      Sequence : I.Word := 0;
   end record;
   function Current (Owner : State) return Phase is (Owner.Stage);
   function Serial (Owner : State) return I.Word is (Owner.Sequence);
   function NMI_Bit (Owner : State) return Boolean is (Owner.NMI_Disabled);
   function Century_Index (Owner : State) return I.Byte is (Owner.Century);
   function Valid (Owner : State) return Boolean is
     (if Owner.Stage in Ready | In_Flight | Uncertain then
        Owner.Known_NMI and then P.Supported (Owner.Device)
        and then Owner.Century <= P.Last_Cell (Owner.Device)
        and then Owner.Standard
        and then (if Owner.Device.Device = P.Intel_Compatible
                    and then Owner.Device.Extended_Enabled then Owner.Extended)
        and then (if Owner.Stage in In_Flight | Uncertain then Owner.Sequence /= 0)
      else True);
end ACPI_CMOS_Transactions;
