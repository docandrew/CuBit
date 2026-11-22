pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Region_Policy;
with Hardware_Authority;
-- Backend-only admission from independently verified launcher window metadata.
-- These values are descriptive, not capabilities. The owning backend must bind
-- each ID to the actual retained kernel handle and serialize admission/access.
-- Rights, including delegation, come from that handle; metadata grants nothing.
-- Firmware and the public scalar endpoint cannot install or enlarge windows.
package ACPI_Backend_Windows with SPARK_Mode is
   use Interfaces;
   package P renames ACPI_Region_Policy;
   package T renames ACPI_FADT.Transactions;
   use type P.Address_Space, P.Configuration, P.State;
   type Window_ID is new Unsigned_64;
   No_Window : constant Window_ID := 0;
   Map_Page_Bytes : constant Unsigned_64 := 4_096;
   Last_IO_Port : constant Unsigned_64 := Unsigned_64 (Unsigned_16'Last);
   type Granted_Window is record
      ID : Window_ID := No_Window;
      Space : P.Address_Space := P.Memory_Space;
      Base, Length : Unsigned_64 := 0;
      Readable, Writable, Delegable : Boolean := False;
   end record;
   -- Memory windows are complete admitted pages, never outward-rounded byte
   -- spans. Excluding RAM/other owners and establishing the handle's identity
   -- are independent kernel/launcher obligations, not consequences of Valid.
   function Valid (Window : Granted_Window) return Boolean is
     (Window.ID /= No_Window and then Window.Length /= 0
      and then Window.Length - 1 <= Unsigned_64'Last - Window.Base
      and then (Window.Readable or Window.Writable)
      and then (if Window.Space = P.IO_Space then
        Window.Base <= Last_IO_Port
        and then Window.Length - 1 <= Last_IO_Port - Window.Base
      else Window.Base mod Map_Page_Bytes = 0
        and then Window.Length mod Map_Page_Bytes = 0));
   type Window_Result (Allowed : Boolean := False) is record
      case Allowed is
         when True => Value : Granted_Window;
         when False => null;
      end case;
   end record;
   function Memory_Description_Valid
     (ID : Window_ID; First_Page, Page_Count : Unsigned_64;
      Readable, Writable : Boolean) return Boolean is
     (ID /= No_Window and then (Readable or Writable)
      and then First_Page <= Unsigned_64'Last / Map_Page_Bytes
      and then Page_Count /= 0
      and then Page_Count <= Unsigned_64'Last / Map_Page_Bytes
      and then Page_Count - 1 <= Unsigned_64'Last / Map_Page_Bytes - First_Page);
   function IO_Description_Valid
     (ID : Window_ID; First_Port, Count : Unsigned_64;
      Readable, Writable : Boolean) return Boolean is
     (ID /= No_Window and then (Readable or Writable)
      and then First_Port <= Last_IO_Port and then Count /= 0
      and then Count - 1 <= Last_IO_Port - First_Port);
   -- Constructors for typed launch descriptions. They perform no handle
   -- lookup, grant derivation, mapping or outward rounding.
   function From_Memory_Pages
     (ID : Window_ID; First_Page, Page_Count : Unsigned_64;
      Readable, Writable : Boolean; Delegable : Boolean := False) return Window_Result
     with Post => From_Memory_Pages'Result.Allowed =
       Memory_Description_Valid (ID, First_Page, Page_Count, Readable, Writable)
       and then (if From_Memory_Pages'Result.Allowed then
       Valid (From_Memory_Pages'Result.Value)
       and then From_Memory_Pages'Result.Value.Space = P.Memory_Space
       and then From_Memory_Pages'Result.Value.ID = ID
       and then From_Memory_Pages'Result.Value.Base = First_Page * Map_Page_Bytes
       and then From_Memory_Pages'Result.Value.Length = Page_Count * Map_Page_Bytes
       and then From_Memory_Pages'Result.Value.Readable = Readable
       and then From_Memory_Pages'Result.Value.Writable = Writable
       and then From_Memory_Pages'Result.Value.Delegable = Delegable);
   function From_IO_Ports
     (ID : Window_ID; First_Port, Count : Unsigned_64;
      Readable, Writable : Boolean; Delegable : Boolean := False) return Window_Result
     with Post => From_IO_Ports'Result.Allowed =
       IO_Description_Valid (ID, First_Port, Count, Readable, Writable)
       and then (if From_IO_Ports'Result.Allowed then
       Valid (From_IO_Ports'Result.Value)
       and then From_IO_Ports'Result.Value.Space = P.IO_Space
       and then From_IO_Ports'Result.Value.ID = ID
       and then From_IO_Ports'Result.Value.Base = First_Port
       and then From_IO_Ports'Result.Value.Length = Count
       and then From_IO_Ports'Result.Value.Readable = Readable
       and then From_IO_Ports'Result.Value.Writable = Writable
       and then From_IO_Ports'Result.Value.Delegable = Delegable);
   function Width_Mask (Width : T.Access_Width) return Unsigned_64 is
     (case Width is
       when T.Byte_Access => 16#FF#,
       when T.Word_Access => 16#FFFF#,
       when T.Dword_Access => 16#FFFF_FFFF#,
       when T.Qword_Access => Unsigned_64'Last);
   function Named_Record_Valid (Item : P.Configuration) return Boolean is
     (P.Valid (Item)
      and then Hardware_Authority.Valid (Item.Register_Authority)
      and then (not Item.Register_Authority.Readable or Item.Readable)
      and then (not Item.Register_Authority.Writable or Item.Writable)
      and then Item.Widths (Item.Register_Width)
      and then Item.Register_Offset < Item.Length
      and then Unsigned_64 (T.Octets (Item.Register_Width)) <=
        Item.Length - Item.Register_Offset
      and then T.Aligned (Item.Base + Item.Register_Offset, Item.Register_Width)
      and then (Item.Register_Authority.Write_Mask and
        not Width_Mask (Item.Register_Width)) = 0);
   function Contains
     (Window : Granted_Window; Item : P.Configuration) return Boolean is
     (Valid (Window) and then P.Valid (Item)
      and then Window.Space = Item.Space
      and then Item.Base >= Window.Base
      and then Item.Base - Window.Base < Window.Length
      and then Item.Length <= Window.Length - (Item.Base - Window.Base));
   -- Check the actual fixed scalar independently of the enclosing region.
   -- This remains a runtime gate if region geometry rules change later.
   function Register_Contained
     (Window : Granted_Window; Item : P.Configuration) return Boolean is
     (Valid (Window) and then P.Valid (Item)
      and then Window.Space = Item.Space
      and then Item.Register_Offset <= Unsigned_64'Last - Item.Base
      and then Item.Base + Item.Register_Offset >= Window.Base
      and then Item.Base + Item.Register_Offset - Window.Base < Window.Length
      and then Unsigned_64 (T.Octets (Item.Register_Width)) <=
        Window.Length - (Item.Base + Item.Register_Offset - Window.Base));
   function Rights_Fit
     (Window : Granted_Window; Item : P.Configuration) return Boolean is
     ((not Item.Readable or Window.Readable)
      and then (not Item.Writable or Window.Writable)
      and then (not Item.Register_Authority.Delegable or Window.Delegable));
   type Write_Safety is (Unestablished, Exact_Writes_Admitted);
   type Outcome is (Admitted, Invalid_Window, Invalid_Record, Outside_Window,
                    Rights_Unavailable, Writes_Not_Admitted, Slot_Unavailable);
   -- Exact-write approval is a trusted, register-specific platform fact. A
   -- valid firmware address or a writable window cannot establish it.
   function Check
     (Window : Granted_Window; Item : P.Configuration;
      Writes : Write_Safety) return Outcome
     with Post => (Check'Result = Admitted) =
       (Valid (Window) and then Named_Record_Valid (Item)
        and then Contains (Window, Item) and then Register_Contained (Window, Item)
        and then Rights_Fit (Window, Item)
        and then (not Item.Writable or Writes = Exact_Writes_Admitted))
       and then (if Check'Result = Admitted then
         Item.Register_Offset <= Unsigned_64'Last - Item.Base
         and then Item.Base + Item.Register_Offset >= Window.Base
         and then Item.Base + Item.Register_Offset - Window.Base < Window.Length
         and then Unsigned_64 (T.Octets (Item.Register_Width)) <=
           Window.Length - (Item.Base + Item.Register_Offset - Window.Base));
   -- Existing region epochs and uncertainty reservations survive refusal and
   -- reuse. The owner retains the matching actual window backing separately.
   procedure Install
     (Region : in out P.State; Window : Granted_Window;
      Item : P.Configuration; Writes : Write_Safety; Result : out Outcome)
     with Post =>
       (if Result = Admitted then
          P.Active (Region) and then P.Policy (Region) = Item
          and then Check (Window, Item, Writes) = Admitted
          and then P.Epoch (Region) = P.Epoch (Region'Old) + 1
          and then P.Receipt (Region) = P.Receipt (Region'Old)
        else Region = Region'Old);
end ACPI_Backend_Windows;
