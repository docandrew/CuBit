with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_Table_Provenance is
   function Generation (Object : Ledger) return Unsigned_64 is (Object.Epoch);
   function Capacity (Object : Ledger) return Positive is (Records.Capacity (Object.Items));
   function Count (Object : Ledger) return Natural is (Object.Used);
   package body Authority is
   procedure Resolve (Session : Unsigned_64; Item : Mapping; Value : out Mapping) is
      CPU, DMA : Unsigned_64;
      OK : Boolean;
   begin
      Value := (others => 0);
      if Session = 0 or else Item.Ticket = 0 or else Item.Offset mod 4096 /= 0 then return; end if;
      Resolve_Owned_Page (Session, Item.Ticket, Item.Offset, CPU, DMA, OK);
      if not OK or else CPU = 0 or else CPU mod 4096 /= 0 or else CPU > 2 ** 47 - 4096
        or else not Intel_GPU_ADLN_PPGTT.Valid_DMA_Page (DMA)
        or else (Item.CPU /= 0 and then Item.CPU /= CPU)
        or else (Item.DMA /= 0 and then Item.DMA /= DMA)
      then return; end if;
      Value := (Item.Ticket, Item.Offset, CPU, DMA);
   end Resolve;
   procedure Install
     (Object : in out Ledger; Session, Expected_Generation : Unsigned_64; Index : Positive;
      Ticket, Offset : Unsigned_64; Accepted : out Boolean)
   is
      Value : Mapping;
   begin
      Accepted := False;
      if Expected_Generation /= Object.Epoch or else Object.Phase /= Open or else Session = 0 or else (Object.Owner /= 0 and then Object.Owner /= Session)
        or else Object.Used = Natural'Last or else Index /= Object.Used + 1
        or else Index > Capacity (Object) then return; end if;
      Resolve (Session, (Ticket, Offset, 0, 0), Value);
      if Value.Ticket = 0 or else Expected_Generation /= Object.Epoch or else
        Object.Phase /= Open or else Object.Used /= Index - 1 or else
        (Object.Owner /= 0 and then Object.Owner /= Session)
      then return; end if;
      Records.Put (Object.Items, Index, Value);
      Object.Owner := Session; Object.Used := Index; Accepted := True;
   end Install;
   function Lookup
     (Object : Ledger; Session, Expected_Generation : Unsigned_64; Index : Positive) return Mapping
   is
      Value : Mapping;
   begin
      if Expected_Generation /= Object.Epoch or else Object.Phase /= Open or else Session = 0 or else Session /= Object.Owner or else Index > Object.Used
      then return (others => 0); end if;
      Resolve (Session, Records.Get (Object.Items, Index), Value);
      return Value;
   end Lookup;
   function Status (Operation : Append_State) return Append_Phase is (Operation.Phase);
   function First_ID (Operation : Append_State) return Natural is
     (if Operation.Phase = Appended then Operation.Before + 1 else 0);
   function Installed (Operation : Append_State) return Natural is (Operation.Done);
   procedure Rearm
     (Operation : in out Append_State; Object : Ledger; Accepted : out Boolean) is
      use type System.Address;
   begin
      Accepted := False;
      if Operation.Phase /= Appended or else
        Object'Address /= Operation.Ledger_Address or else Object.Phase /= Open or else
        Object.Owner /= Operation.Session or else Object.Epoch /= Operation.Epoch or else
        Object.Used < Operation.Before or else
        Object.Used - Operation.Before /= Operation.Pages or else
        Operation.Done /= Operation.Pages
      then return; end if;
      Operation.Phase := Unused;
      Operation.Ledger_Address := System.Null_Address;
      Operation.Session := 0; Operation.Epoch := 0; Operation.Ticket := 0;
      Operation.Offset := 0; Operation.Before := 0; Operation.Pages := 0; Operation.Done := 0;
      Accepted := True;
   end Rearm;
   procedure Begin_Append
     (Operation : in out Append_State; Object : Ledger;
      Session, Expected_Generation, Ticket, Offset : Unsigned_64;
      Pages : Positive; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Operation.Phase /= Unused then return; end if;
      Operation.Phase := Rejected;
      if Object.Phase /= Open or else Object.Epoch /= Expected_Generation or else
        Session = 0 or else Ticket = 0 or else Offset mod 4096 /= 0 or else
        (Object.Owner /= 0 and then Object.Owner /= Session) or else
        Pages > Capacity (Object) - Object.Used or else
        Unsigned_64 (Pages - 1) > (Unsigned_64'Last - Offset) / 4096
      then return; end if;
      Operation.Ledger_Address := Object'Address;
      Operation.Session := Session; Operation.Epoch := Expected_Generation;
      Operation.Ticket := Ticket; Operation.Offset := Offset;
      Operation.Before := Object.Used; Operation.Pages := Pages;
      Operation.Phase := Appending; Accepted := True;
   end Begin_Append;
   procedure Step (Operation : in out Append_State; Object : in out Ledger) is
      use type System.Address;
      OK : Boolean;
   begin
      if Operation.Phase /= Appending then return; end if;
      Operation.Phase := Rejected;
      if Object'Address /= Operation.Ledger_Address or else Object.Phase /= Open or else
        Object.Epoch /= Operation.Epoch or else
        Object.Used /= Operation.Before + Operation.Done or else
        (Object.Owner /= 0 and then Object.Owner /= Operation.Session)
      then return; end if;
      Install (Object, Operation.Session, Operation.Epoch, Object.Used + 1,
        Operation.Ticket, Operation.Offset + Unsigned_64 (Operation.Done) * 4096, OK);
      if not OK then return; end if;
      Operation.Done := Operation.Done + 1;
      Operation.Phase := (if Operation.Done = Operation.Pages then Appended else Appending);
   end Step;
   end Authority;
   procedure Extend
     (Object : in out Ledger; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Phase = Open then Records.Extend (Object.Items, Base, Bytes, Accepted); end if;
   end Extend;
   procedure Scan_Ticket
     (Object : Ledger; Session, Ticket : Unsigned_64; First : Positive;
      Found : out Boolean; Next : out Natural; Accepted : out Boolean)
   is
      Last : Natural;
   begin
      Found := False; Next := 0; Accepted := False;
      if Session = 0 or else Session /= Object.Owner or else Ticket = 0
        or else First > Object.Used then return; end if;
      Last := First + Natural'Min (63, Object.Used - First);
      for I in First .. Last loop
         Found := Found or else Records.Get (Object.Items, I).Ticket = Ticket;
      end loop;
      if Last < Object.Used then Next := Last + 1; end if;
      Accepted := True;
   end Scan_Ticket;
end Intel_GPU_Table_Provenance;
