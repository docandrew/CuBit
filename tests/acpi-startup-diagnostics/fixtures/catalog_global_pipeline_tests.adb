pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with ACPI_FADT;
with ACPI_FADT.Transactions;
with ACPI_Backend_Catalog;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_FACS_Binding;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Lock_Atomic;
with ACPI_Backend_Global_Lock;
with ACPI_Backend_Global_Signal;
with ACPI_PM1_Global_Release;
procedure Catalog_Global_Pipeline_Tests is
   use Interfaces;
   package B renames ACPI_Backend_Binding_Types;
   package A renames ACPI_Backend_Lock_Atomic;
   type Page is array (Positive range 1 .. 4_096) of Unsigned_8 with Alignment => 4_096;
   Backing : aliased Page := [others => 0];
   Virtual : constant Unsigned_64 := Unsigned_64 (System.Storage_Elements.To_Integer (Backing'Address));
   Physical : constant Unsigned_64 := 16#0000_0001_4000_0000#;
   type Handle is new Unsigned_64;
   type Principal is new Unsigned_64;
   Caller : constant Principal := 16#0000_0001_0000_0111#;
   Checks, Byte_Reads, Map_Calls : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Binding_Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Binding_Clock;
   procedure Memory_Info (Authority : Handle; Item : out B.Description) is
   begin
      Item := (Current => Authority = 1, Kind => B.Memory_Resource,
        Base => Physical, Length => 4_096, Readable => True, Writable => True, Delegable => False);
   end Memory_Info;
   procedure Port_Info (Authority : Handle; Item : out B.Description) is
   begin Item := (Authority = 2, B.Port_Resource, 16#1000#, 4, True, True, False); end Port_Info;
   procedure Map
     (Authority : Handle; Address, At_Virtual, Pages : Unsigned_64; Read_Only : Boolean; Mapped : out Boolean) is
   begin
      -- Hosted RAM is already allocated. No physical mapping syscall occurs.
      Check (Authority = 1 and Address = Physical and At_Virtual = Virtual and Pages = 1 and not Read_Only);
      Map_Calls := Map_Calls + 1; Mapped := True;
   end Map;
   procedure No_Memory_IO
     (Address : Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width; Writing : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Address, Width, Writing, Input);
   begin Output := 0; Completed := False; Check (False); end No_Memory_IO;
   procedure No_Port_IO
     (Authority : Handle; Port : Unsigned_16; Width : ACPI_FADT.Transactions.Access_Width;
      Writing : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Authority, Port, Width, Writing, Input);
   begin Output := 0; Completed := False; Check (False); end No_Port_IO;
   package Catalog is new ACPI_Backend_Catalog
     (Handle, 0, Handle, 0, Memory_Info, Port_Info, Map, Binding_Clock,
      No_Memory_IO, No_Port_IO, 2, 1, 1, Virtual, Virtual + 4_096, 1);
   package H renames Catalog.Bindings;
   package W renames H.W;
   use type W.Window_ID, B.Outcome;
   Hardware : Catalog.Context;
   function Coherent (ID : W.Window_ID) return Boolean is (ID = 1);
   procedure Read_Byte (Address : Unsigned_64; Value : out Unsigned_8; Completed : out Boolean) is
      Wide : Unsigned_64;
   begin
      Byte_Reads := Byte_Reads + 1;
      ACPI_Backend_Memory_Access.Transfer (Address, H.T.Byte_Access, False, 0, Wide, Completed);
      if Completed and Wide <= Unsigned_64 (Unsigned_8'Last) then Value := Unsigned_8 (Wide);
      else Value := 0; Completed := False; end if;
   end Read_Byte;
   package F is new ACPI_Backend_FACS_Binding (H, Catalog.Context, Catalog.Describe_Window, Catalog.Select_Access, Coherent, Read_Byte);
   FACS : F.Context;
   procedure Clock (Now : out Unsigned_64; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   use type H.P.Address_Space;
   package C renames ACPI_PM1_Global_Release;
   Control_A : Unsigned_16 := 16#1A01#;
   Control_B : Unsigned_16 := 16#0402#;
   Control_Reads, Control_Writes : Natural := 0;
   Refuse_Read : Boolean := False;
   procedure Control_Transfer (View : H.Access_View; For_Write : Boolean; Input : Unsigned_16;
      Output : out Unsigned_16; Status : out C.Completion) is
      Before : Unsigned_16;
   begin
      Check (View.Allowed);
      Check (View.Space = H.P.IO_Space and View.Ports = 2 and View.Port in 16#1000# | 16#1002#);
      if Refuse_Read and then not For_Write then
         Output := 0; Status := C.Not_Attempted; return;
      end if;
      Before := (if View.Port = 16#1000# then Control_A else Control_B);
      Output := 0; Status := C.Completed;
      if For_Write then
         Control_Writes := Control_Writes + 1;
         Check ((Input and C.Global_Release) /= 0 and (Input and C.Sleep_Enable) = 0);
         Check ((Input and not C.Command_Bits) = (Before and not C.Command_Bits));
         if View.Port = 16#1000# then Control_A := Input and not C.Command_Bits;
         else Control_B := Input and not C.Command_Bits; end if;
      else
         Check (Input = 0); Control_Reads := Control_Reads + 1; Output := Before and not C.Command_Bits;
      end if;
   end Control_Transfer;
   package Signal_Owner is new ACPI_Backend_Global_Signal (H, Catalog.Context, Catalog.Select_Access, Control_Transfer);
   use type Signal_Owner.Outcome;
   Signal_State : Signal_Owner.Context;
   Signal_Report : Signal_Owner.Report;
   Observed_Reads, Observed_CAS, Observed_Signals : Natural := 0;
   procedure Note_Word (Word : Unsigned_32; Available : Boolean) is
   begin
      Observed_Reads := Observed_Reads+1;
      if not Available then Check (Word = 0); end if;
   end Note_Word;
   procedure Note_CAS (Expected, Replacement, Observed : Unsigned_32; Known, Attempted, Exchanged : Boolean) is
   begin
      Observed_CAS := Observed_CAS+1;
      Check (Known and Attempted and Exchanged and Expected = Observed);
      Check ((Replacement and 2) /= (Expected and 2));
   end Note_CAS;
   procedure Note_Signal (Report : Signal_Owner.Report) is
   begin
      Observed_Signals := Observed_Signals+1;
      Check (Report.Status in Signal_Owner.Signaled | Signal_Owner.Not_Sent);
   end Note_Signal;
   package Bridge is new ACPI_Backend_Global_Lock
     (H, Catalog.Context, F, Signal_Owner, Hardware, FACS, Signal_State, Principal, 0, Clock,
      Observe_Word => Note_Word, Observe_CAS => Note_CAS, Observe_Signal => Note_Signal);
   package O renames Bridge.Owner;
   package S renames Bridge.Sessions;
   use type O.Outcome, S.Token, F.Outcome;
   State : S.Context;
   D : ACPI_FADT.Description;
   Bound : B.Report;
   Prepared : F.Report;
   Token : S.Token;
   Result : O.Outcome;
   Cell : aliased A.Word with Import, Address => Backing (17)'Address;
   Seen : Unsigned_32;
   Disabled : Boolean;
begin
   Backing (1) := Character'Pos ('F'); Backing (2) := Character'Pos ('A');
   Backing (3) := Character'Pos ('C'); Backing (4) := Character'Pos ('S');
   Backing (5) := 64; Backing (33) := 2;
   D.X_FACS := (True, Physical); D.FACS := 16#4000_0000#;
   D.Legacy (ACPI_FADT.PM1A_Control) := 16#1000#; D.Legacy (ACPI_FADT.PM1B_Control) := 16#1002#;
   D.Lengths (ACPI_FADT.PM1A_Control) := 2; D.Lengths (ACPI_FADT.PM1B_Control) := 2;
   Catalog.Bind_Memory (Hardware, (1, H.P.Memory_Space, Physical, 4_096, True, True, False), 1, 100, Bound);
   Check (Bound.Status = B.Bound);
   Catalog.Bind_Ports (Hardware, (2, H.P.IO_Space, 16#1000#, 4, True, True, False), 2, 100, Bound);
   Check (Bound.Status = B.Bound);
   F.Prepare (FACS, Hardware, D, 1, Prepared);
   Check (Prepared.Status = F.Ready and Byte_Reads = 64);
   Check (Map_Calls = 1 and then Catalog.Observe (Hardware).Retained_Windows = 2);
   Catalog.Seal (Hardware, Disabled); Check (Disabled);
   S.Configure (State, Caller, 16#1000_0000_0001#, Result); Check (Result = O.Configured);
   S.Acquire (State, Caller, 100, 3, Token, Result);
   Check (Result = O.Read_Unavailable and then Token = S.No_Token and then A.Read_Word (Cell'Access) = 0);
   Signal_Owner.Prepare (Signal_State, Hardware, D, [others => 2], Signal_Report);
   Check (Signal_Report.Status = Signal_Owner.Prepared and Signal_Report.Banks = 2);
   for I in Unsigned_32 range 0 .. 1_023 loop
      Cell := A.Word (16#CAFE_0000# + I * 4);
      S.Acquire (State, Caller, 100, 3, Token, Result); Check (Result = O.Acquired);
      S.Check_Access (State, Caller, Token, Result); Check (Result = O.Configured);
      -- Simulated firmware marks pending using the actual hosted atomic.
      Seen := A.Compare_Exchange (Cell'Access, 16#CAFE_0002# + I * 4, 16#CAFE_0003# + I * 4);
      Check (Seen = 16#CAFE_0002# + I * 4);
      S.Release (State, Caller, Token, 3, Result);
      Check (Result = O.Released and A.Read_Word (Cell'Access) = 16#CAFE_0000# + I * 4);
   end loop;
   Check (Byte_Reads = 64);
   Check (Control_Reads = 2_048 and Control_Writes = 2_048 and Control_A = 16#1A01# and Control_B = 16#0402#);
   Cell := 16#A000_0000#;
   S.Acquire (State, Caller, 100, 3, Token, Result); Check (Result = O.Acquired);
   Seen := A.Compare_Exchange (Cell'Access, 16#A000_0002#, 16#A000_0003#);
   Check (Seen = 16#A000_0002#);
   Refuse_Read := True;
   S.Release (State, Caller, Token, 3, Result);
   Check (Result = O.Signal_Deferred and then A.Read_Word (Cell'Access) = 16#A000_0000#);
   Check (Control_Writes = 2_048);
   S.Check_Access (State, Caller, Token, Result); Check (Result /= O.Configured);
   Refuse_Read := False;
   S.Release (State, Caller, Token, 3, Result);
   Check (Result = O.Released and then Control_Writes = 2_050);
   Catalog.Disable_Window (Hardware, 1, Disabled); Check (Disabled);
   S.Acquire (State, Caller, 100, 1, Token, Result);
   Check (Result = O.Read_Unavailable and Byte_Reads = 64);
   Check (Map_Calls = 1 and then Catalog.Observe (Hardware).Sealed);
   Check (Observed_Reads = 2_052 and Observed_CAS = 2_050 and Observed_Signals = 1_026);
   Ada.Text_IO.Put_Line ("Hosted catalog FACS and PM1 pipeline checks" & Checks'Image);
end Catalog_Global_Pipeline_Tests;
