pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Passive_Query.Pages;
generic
   type Context is limited private;
   type Sender_ID is private;
   type Reference is private;
   No_Reference : Reference;
   type Mapping is private;
   No_Mapping : Mapping;
   with procedure Acquire_Writable
     (Owner : in out Context; Loan : Reference; Expected_Owner : Sender_ID;
      Byte_Length : ACPI_Passive_Query.Word; Acquired : out Mapping; Accepted : out Boolean);
   with procedure Copy_In
     (Owner : in out Context; Acquired : Mapping;
      Data : out ACPI_Passive_Query.Pages.Byte_Array; Accepted : out Boolean);
   with procedure Evaluate
     (Owner : in out Context; Request_Data : ACPI_Passive_Query.Pages.Byte_Array;
      Response_Data : out ACPI_Passive_Query.Pages.Byte_Array;
      Written, Required_Bytes : out ACPI_Passive_Query.Word; Accepted : out Boolean);
   with procedure Copy_Out
     (Owner : in out Context; Acquired : Mapping;
      Data : ACPI_Passive_Query.Pages.Byte_Array; Accepted : out Boolean);
   with procedure Return_Acquisition
     (Owner : in out Context; Loan : Reference; Acquired : Mapping; Accepted : out Boolean);
package ACPI_Observer_Loans with SPARK_Mode is
   package Wire renames ACPI_Passive_Query;
   package Pages renames ACPI_Passive_Query.Pages;
   subtype Backend_Context is Context;
   subtype Peer_Identity is Sender_ID;
   subtype Loan_Reference is Reference;
   use type Wire.Word;
   function Admits_Extent (Declared_Length, Request_Capacity, Response_Capacity : Wire.Word) return Boolean is
     (Declared_Length >= Pages.Frame_Byte_Count and then Declared_Length <= Wire.Word (Natural'Last)
      and then Declared_Length <= Request_Capacity and then Declared_Length <= Response_Capacity);
   type State is private;
   function Holding (Item : State) return Boolean;
   type Outcome is (Completed, Denied, Invalid_Extent, Busy, Acquire_Refused,
                    Copy_In_Failed, Evaluation_Refused, Invalid_Response, Copy_Out_Failed);
   type Report is record
      Status : Outcome := Invalid_Extent;
      Written, Required_Bytes : Wire.Word := 0;
      May_Reply : Boolean := True;
      Release_Attempted, Released : Boolean := False;
   end record;
   use type Pages.Byte;
   -- Kernel-stamped role and sender, generation-checked opaque reference.
   -- No sender/reference arithmetic or mapping-address construction occurs.
   -- Acquire_Writable must acquire offset zero with Expected_Owner and exact
   -- Byte_Length. Copy callbacks operate only within that acquired extent.
   -- Evaluate receives a stable private request, never a mapped loan view.
   -- Callbacks must return normally and report copy/acquisition failures.
   -- The owner must keep the received/saved reply while Holding and may send
   -- it only after May_Reply or successful Retry_Return. Client retirement is
   -- separate; a completed return does not authorize the client's page reuse.
   procedure Handle
     (Item : in out State; Owner : in out Context; Stamp : ACPI_Protocol.Role_Tag;
      Sender : Sender_ID; Loan : Reference; Declared_Length : Wire.Word;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report)
     with Post => Result.May_Reply = not Holding (Item)
       and then (if Result.Status /= Completed then Result.Written = 0
         and then (for all B of Response_Data => B = 0));
   procedure Retry_Return (Item : in out State; Owner : in out Context; Released : out Boolean)
     with Post => Released = not Holding (Item);
private
   type State is record
      Held : Boolean := False;
      Loan : Reference := No_Reference;
      Acquired : Mapping := No_Mapping;
   end record;
   function Holding (Item : State) return Boolean is (Item.Held);
end ACPI_Observer_Loans;
