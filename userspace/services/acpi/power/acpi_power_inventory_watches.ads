pragma Ada_2022;
with ACPI_Power_Watch_Protocol;
generic
   type Reply_Token is private;
   No_Reply : Reply_Token;
   Maximum_Generation : ACPI_Power_Watch_Protocol.Generation_Limit := ACPI_Power_Watch_Protocol.Generation_Limit'Last;
   -- Trusted serialized transport. Save returns a unique live token or none.
   -- Answer consumes it even when the holder has gone; no callback reentry.
   with procedure Save_Current (Token : out Reply_Token; Saved : out Boolean);
   with procedure Answer_Saved (Token : in out Reply_Token;
      ID : ACPI_Power_Watch_Protocol.Word; Status : ACPI_Power_Watch_Protocol.Status;
      Generation : ACPI_Power_Watch_Protocol.Word; Delivered : out Boolean);
package ACPI_Power_Inventory_Watches with SPARK_Mode is
   package P renames ACPI_Power_Watch_Protocol;
   subtype Word is P.Word;
   type Request is record
      Authority, Sender, Length, ID, Seen, Zero_1, Zero_2, Flags, Reserved : Word := 0;
   end record;
   type Response is record ID : Word := 0; Status : P.Status := P.Unavailable; Generation : Word := 1; end record;
   type Delivery is (No_Attempt, Answered, Caller_Gone);
   type State is limited private;
   function Generation (Item : State) return Word;
   function Quiet (Item : State) return Boolean;
   function Runnable (Item : State) return Boolean;
   function Valid (Item : State) return Boolean;
   -- Facts only; no transport. Initial generation1 makes an empty inventory
   -- synchronizable; completion/publication and external events advance it.
   procedure Changed (Item : in out State) with Pre=>Valid(Item),Post=>Valid(Item);
   procedure Begin_Stop (Item : in out State) with Pre=>Valid(Item),Post=>Valid(Item);
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Delivery)
     with Pre=>Valid(Item),Post=>Valid(Item);
   procedure Watch (Item : in out State; Input : Request; Owner_Quiet : Boolean;
      Reply : out Response; Deferred : out Boolean; Previous : out Delivery)
     with Pre=>Valid(Item),Post=>Valid(Item);
private
   subtype Slot is Positive range 1 .. P.Maximum_Watches;
   type Holder is record Token : Reply_Token := No_Reply; Sender, ID, Seen : Word := 0; end record;
   type Entries is array (Slot) of Holder;
   type State is limited record
      Current : Word := 1;
      Stopped, Overflow : Boolean := False;
      Holders : Entries;
      Next_Slot : Slot := Slot'First;
   end record;
end ACPI_Power_Inventory_Watches;
