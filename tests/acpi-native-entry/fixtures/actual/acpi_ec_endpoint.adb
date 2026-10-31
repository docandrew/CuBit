with Interfaces;
package body ACPI_EC_Endpoint with SPARK_Mode is
   use Interfaces;
   use ACPI_Region_Protocol;
   use type Registers.Submission;
   use type Registers.Owner_ID;
   use type ACPI_EC_Types.Outcome;
   function Response (Status : Outcome; Value : Unsigned_64 := 0) return Packet is
     ((Label => Operation_Reply,
       Data => [Unsigned_64 (Outcome'Pos (Status)), Value, 0, 0], others => <>));
   procedure Begin_Call
     (Sender : Registers.Owner_ID; Item : Packet;
      Deadline : ACPI_EC_Types.Microseconds;
      Budget : ACPI_EC_Types.Work_Budget;
      Deferred : out Boolean; Token : out Registers.Operation_ID;
      Reply : out Packet)
   is
      Result : Registers.Submission;
   begin
      Deferred := False;
      Token := 0;
      Reply := Response (Denied);
      if Sender = 0 then
         return;
      end if;
      Reply := Response (Malformed);
      if Item.Length /= 4 or Item.Flags /= 0 or Item.Reserved /= 0
        or Item.Label not in Read_Operation | Write_Operation
        or Item.Data (3) /= 0
        or (Item.Label = Read_Operation and Item.Data (2) /= 0)
        or Item.Data (2) > Unsigned_64 (ACPI_EC_Types.Byte'Last)
      then
         return;
      end if;
      Reply := Response (Denied);
      Registers.Submit
        (Sender, Registers.Register_ID (Item.Data (1)),
         Registers.Epoch_ID (Item.Data (0)), Item.Label = Write_Operation,
         ACPI_EC_Types.Byte (Item.Data (2)), Deadline, Budget, Result, Token);
      Deferred := Result = Registers.Started;
      --  Reply is ignored while Deferred; only Collect produces the eventual
      --  response for that retained call. Immediate denial has performed no I/O.
   end Begin_Call;
   procedure Collect (Fact : out Registers.Completion; Reply : out Packet) is
   begin
      Registers.Take_Completion (Fact);
      Reply := Response (Denied);
      if not Fact.Present then
         return;
      elsif Fact.Status = ACPI_EC_Types.Succeeded then
         Reply := Response (Done, Unsigned_64 (Fact.Value));
      elsif Fact.Effects_Possible or Fact.Needs_Recovery then
         Reply := Response (Indeterminate);
      end if;
   end Collect;
end ACPI_EC_Endpoint;
