pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_Region_Policy;
with ACPI_Region_Protocol;
with ACPI_FADT.Transactions;
-- A request executor and draft wire adapter, not a native hardware endpoint.
-- The backend must serialize policy transitions and retain actual backing.
-- The hardware callback owns real access semantics. Completed=False means
-- side effects may have occurred: never replay or release automatically.
generic
   type Hardware_State is private;
   with procedure Transact
     (Hardware : in out Hardware_State;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : ACPI_Region_Policy.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean);
package ACPI_Region_IO with SPARK_Mode is
   use ACPI_Region_Policy;
   -- Internal transaction only: never deserialize this type from a service.
   type Request is record
      Token, Offset : Unsigned_64 := 0;
      Width : Access_Width := ACPI_FADT.Transactions.Byte_Access;
      For_Write : Boolean := False;
      Value : Unsigned_64 := 0;
   end record;
   function Fits_Value (Value : Unsigned_64; Width : Access_Width) return Boolean is
     (case Width is
       when ACPI_FADT.Transactions.Byte_Access => Value <= 16#FF#,
       when ACPI_FADT.Transactions.Word_Access => Value <= 16#FFFF#,
       when ACPI_FADT.Transactions.Dword_Access => Value <= 16#FFFF_FFFF#,
       when ACPI_FADT.Transactions.Qword_Access => True);
   type Outcome is (Denied, Malformed, Done, Indeterminate, Backend_Fault);
   type Response is record
      Status : Outcome := Denied;
      Value : Unsigned_64 := 0;
      -- Internal cleanup correlation only, not an untrusted Finish request.
      Pending_Ticket : Unsigned_64 := 0;
   end record;
   procedure Execute
     (Region : in out State; Hardware : in out Hardware_State;
      Stamp : Unsigned_64; Item : Request; Reply : out Response) with
     Post => Policy (Region) = Policy (Region'Old)
       and then Epoch (Region) = Epoch (Region'Old)
       and then (if Reply.Status in Denied | Malformed then
         Region = Region'Old and then Hardware = Hardware'Old)
       and then (if Reply.Status /= Done or else Item.For_Write then Reply.Value = 0)
       and then (if Reply.Status = Done then
         not Busy (Region) and then Fits_Value (Reply.Value, Item.Width))
       and then (if Reply.Status = Indeterminate then
         not Active (Region) and then Busy (Region)
         and then Reply.Pending_Ticket = Receipt (Region)
         and then Reply.Pending_Ticket /= 0
       else Reply.Pending_Ticket = 0);
   Read_Operation : constant Unsigned_32 := 0;
   Write_Operation : constant Unsigned_32 := 1;
   Operation_Reply : constant Unsigned_32 := 16#F002#;
   -- Separate scoped endpoint protocol. Exactly four words:
   -- [record epoch, named register ID, value, zero]. Address, offset and width
   -- come exclusively from the trusted epoch-bound record. Reads require
   -- value=0; writes may set only Write_Mask bits. This is an exact write, not
   -- read-modify-write; catalog admission must establish safe write semantics.
   -- Flags/reserved must be zero. Replies contain
   -- [outcome, low32 value, high32 value, 0], representable by CCL integers.
   -- Pending_Ticket is returned ONLY to the backend, never encoded on wire.
   -- Stamp must be taken from kernel receive metadata. No wire word supplies it.
   procedure Dispatch
     (Region : in out State; Hardware : in out Hardware_State;
      Stamp : Unsigned_64; Item : ACPI_Region_Protocol.Packet;
      Reply : out ACPI_Region_Protocol.Packet; Pending_Ticket : out Unsigned_64) with
     Post => Reply.Label = Operation_Reply and then Reply.Length = 4
       and then Reply.Flags = 0 and then Reply.Reserved = 0
       and then Reply.Data (0) <= Unsigned_64 (Outcome'Pos (Outcome'Last))
       and then Reply.Data (1) <= 16#FFFF_FFFF# and then Reply.Data (2) <= 16#FFFF_FFFF#
       and then Reply.Data (3) = 0
       and then (if Reply.Data (0) in Outcome'Pos (Done) | Outcome'Pos (Indeterminate) |
                   Outcome'Pos (Backend_Fault) then
         Policy (Region).Register_Authority.Resource_ID /= 0
         and then Item.Data (1) = Policy (Region).Register_Authority.Resource_ID
         and then Item.Data (3) = 0
         and then (if Item.Label = Write_Operation then
           (Item.Data (2) and not Policy (Region).Register_Authority.Write_Mask) = 0))
       and then (if Reply.Data (0) in Outcome'Pos (Denied) | Outcome'Pos (Malformed) then
         Region = Region'Old and then Hardware = Hardware'Old)
       and then (if Reply.Data (0) /= Outcome'Pos (Done) then
         Reply.Data (1) = 0 and then Reply.Data (2) = 0)
       and then (if Reply.Data (0) = Outcome'Pos (Indeterminate) then
         Busy (Region) and then not Active (Region)
         and then Pending_Ticket = Receipt (Region) and then Pending_Ticket /= 0
       else Pending_Ticket = 0);
end ACPI_Region_IO;
