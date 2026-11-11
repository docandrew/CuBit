pragma Ada_2022;
with ACPI_PCI_Window_Exchange;
with CuBit.Process_IDs;
-- Plain policy records for the existing growable claim-ledger family.
-- No array is indexed by a process identity; no record grants authority.
package Devmgr_PCI_Claims with Pure, SPARK_Mode is
   package Q renames ACPI_PCI_Window_Exchange;
   package P renames CuBit.Process_IDs;
   use type Q.Word, Q.Record_ID, Q.Request, P.Process_ID;
   type Root_ID is new Q.Word;
   No_Root : constant Root_ID := 0;
   type Window is record
      Root : Root_ID := No_Root;
      Key : Q.Bus_Key;
      First : Q.Word := 0;
   end record;
   function Valid (Item : Window) return Boolean is
     (Item.Root /= No_Root and then Item.First mod Q.Page_Bytes = 0
      and then Q.Bus_Bytes-1 <= Q.Word'Last-Item.First);
   type Reader_State is (Preparing, Granted_Lifetime_Unconfirmed,
      Reply_Refused, Reply_Uncertain);
   type Claim is record
      Reader : P.Process_ID := P.No_Process;
      Request : Q.Request;
      ID : Q.Record_ID := Q.No_Record;
      Backing : Window;
      State : Reader_State := Preparing;
   end record;
   type Delivery is (Transferred, Refused, Uncertain);
   function Complete (Item : Claim; Fact : Delivery) return Claim
     with Pre => Item.State = Preparing,
       Post => Complete'Result.Reader = Item.Reader
         and then Complete'Result.Request = Item.Request
         and then Complete'Result.ID = Item.ID
         and then Complete'Result.Backing = Item.Backing;
   function May_Hold (Item : Claim) return Boolean is
     (Item.State in Granted_Lifetime_Unconfirmed | Reply_Uncertain);
   function Successor (ID : Q.Record_ID) return Q.Record_ID is
     (if ID = Q.Record_ID'Last then Q.No_Record else ID+1)
     with Pre => ID /= Q.No_Record;
   type Device_Number is range 0 .. Q.Devices_Per_Bus-1;
   type Function_Number is range 0 .. Q.Functions_Per_Device-1;
   type Function_Key is record
      Bus : Q.Bus_Key;
      Device : Device_Number := 0;
      Func : Function_Number := 0;
   end record;
   type Function_Owner is record
      Key : Function_Key;
      Owner : P.Process_ID := P.No_Process;
   end record;
end Devmgr_PCI_Claims;
