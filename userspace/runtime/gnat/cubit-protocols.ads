------------------------------------------------------------------------------
--  CuBit typed IPC and stream protocol metadata
--
--  This package contains data-only contracts.  A contract identifies wire
--  schemas and transfer semantics; it never names or grants an authority.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Protocols with
   Pure,
   SPARK_Mode => On
is
   type Interface_Id is new Unsigned_64;
   type Schema_Id is new Unsigned_64;
   type Operation_Id is new Unsigned_32;
   type Protocol_Version is new Unsigned_16;

   NO_INTERFACE : constant Interface_Id := 0;
   NO_SCHEMA    : constant Schema_Id := 0;

   type Transport_Kind is (Inline_Message, Shared_Grant, Typed_Stream);
   type Wire_Size_Kind is (Fixed_Size, Bounded_Size);

   --  Programmer-facing names follow the CCL ownership vocabulary.  Copy is
   --  valid only for unrestricted values.  Move transfers responsibility.
   type Transfer_Mode is
     (Copy_Value, Move_Value, Borrowed_RO, Borrowed_RW);

   --  Required outcome for an ownership-bearing argument.  The operation's
   --  declared verb gives the domain-specific spelling (send, commit, cancel,
   --  rollback, return, and so on).
   type Completion_Effect is
     (No_Completion_Effect, Consume_Value, Return_Value, Transition_Value);

   type Schema_Contract is record
      Identity  : Schema_Id := NO_SCHEMA;
      Version   : Protocol_Version := 0;
      Sizing    : Wire_Size_Kind := Fixed_Size;
      Wire_Size : Unsigned_32 := 0;
   end record;

   NO_SCHEMA_CONTRACT : constant Schema_Contract := (others => <>);

   type Outcome_Contract is record
      Verb        : Unsigned_8 := 0;
      Effect      : Completion_Effect := No_Completion_Effect;
      Result_Type : Schema_Id := NO_SCHEMA;
   end record;

   type Operation_Contract is record
      Interface_Identity : Interface_Id := NO_INTERFACE;
      Operation       : Operation_Id := 0;
      Version         : Protocol_Version := 0;
      Transport       : Transport_Kind := Inline_Message;
      Request         : Schema_Contract := NO_SCHEMA_CONTRACT;
      Response        : Schema_Contract := NO_SCHEMA_CONTRACT;
      Argument_Mode   : Transfer_Mode := Copy_Value;
      On_Success      : Outcome_Contract := (others => <>);
      On_Failure      : Outcome_Contract := (others => <>);
      Max_In_Flight   : Unsigned_16 := 1;
   end record;

   function Valid (Item : Schema_Contract) return Boolean is
     (Item.Identity /= NO_SCHEMA and then Item.Version > 0 and then
      Item.Wire_Size > 0);

   function Wire_Descriptor (Item : Schema_Contract) return Unsigned_64 is
     (Unsigned_64 (Item.Wire_Size) or
      (if Item.Sizing = Bounded_Size then 16#0000_0001_0000_0000# else 0));

   function Valid (Item : Outcome_Contract) return Boolean is
     (if Item.Effect = No_Completion_Effect then
         Item.Verb = 0 and then Item.Result_Type = NO_SCHEMA
      elsif Item.Effect = Transition_Value then
         Item.Verb /= 0 and then Item.Result_Type /= NO_SCHEMA
      else Item.Verb /= 0 and then Item.Result_Type = NO_SCHEMA);

   function Valid (Item : Operation_Contract) return Boolean is
     (Item.Interface_Identity /= NO_INTERFACE and then
      Item.Version > 0 and then
      Valid (Item.Request) and then Valid (Item.Response) and then
      Valid (Item.On_Success) and then Valid (Item.On_Failure) and then
      Item.Max_In_Flight > 0 and then
      (if Item.Argument_Mode = Copy_Value then
          Item.On_Success.Effect = No_Completion_Effect and then
          Item.On_Failure.Effect = No_Completion_Effect
       elsif Item.Argument_Mode in Borrowed_RO | Borrowed_RW then
          Item.On_Success.Effect = Return_Value and then
          Item.On_Failure.Effect = Return_Value
       else Item.On_Success.Effect /= No_Completion_Effect and then
            Item.On_Failure.Effect /= No_Completion_Effect));

   function Compatible
     (Producer, Consumer : Schema_Contract) return Boolean is
     (Valid (Producer) and then Valid (Consumer) and then
      Producer.Identity = Consumer.Identity and then
      Producer.Version = Consumer.Version and then
      Producer.Sizing = Consumer.Sizing and then
      Producer.Wire_Size = Consumer.Wire_Size);

   --  First migrated protocol.  These declarations will move into generated
   --  interface packages once the interface compiler exists.
   CCL_TEST_INTERFACE : constant Interface_Id := 16#4343_4C54_4553_5401#;
   INTEGER_64_SCHEMA  : constant Schema_Id := 16#4343_4C49_3634_0001#;
   INTEGER_64_CONTRACT : constant Schema_Contract :=
     (Identity => INTEGER_64_SCHEMA, Version => 1,
      Sizing => Fixed_Size, Wire_Size => 8);
   CCL_TEST_INCREMENT : constant Operation_Contract :=
     (Interface_Identity => CCL_TEST_INTERFACE,
      Operation => 16#0A00#,
      Version => 1,
      Transport => Inline_Message,
      Request =>
        (Identity => 16#4343_4C49_3634_0001#, Version => 1,
         Sizing => Fixed_Size, Wire_Size => 8),
      Response =>
        (Identity => 16#4343_4C49_3634_0001#, Version => 1,
         Sizing => Fixed_Size, Wire_Size => 8),
      Argument_Mode => Copy_Value,
      On_Success => (others => <>),
      On_Failure => (others => <>),
      Max_In_Flight => 1);
   CCL_TEST_OP_INCREMENT : constant Unsigned_32 := 16#0A00#;

   --  Monotonic clock v1. The reserved request word is zero until CCL gains a
   --  first-class Unit value. The response is milliseconds since kernel boot,
   --  capped at Integer_64'Last so it has an exact CCL Integer representation.
   CLOCK_INTERFACE : constant Interface_Id := 16#434C_4F43_4B00_0001#;
   CLOCK_MONOTONIC_MS : constant Operation_Contract :=
     (Interface_Identity => CLOCK_INTERFACE,
      Operation => 16#0B00#,
      Version => 1,
      Transport => Inline_Message,
      Request =>
        (Identity => 16#4343_4C49_3634_0001#, Version => 1,
         Sizing => Fixed_Size, Wire_Size => 8),
      Response =>
        (Identity => 16#4343_4C49_3634_0001#, Version => 1,
         Sizing => Fixed_Size, Wire_Size => 8),
      Argument_Mode => Copy_Value,
      On_Success => (others => <>),
      On_Failure => (others => <>),
      Max_In_Flight => 1);
   CLOCK_OP_MONOTONIC_MS : constant Unsigned_32 := 16#0B00#;

   TEXT_LINE_SCHEMA : constant Schema_Id := 16#4355_4249_5454_5801#;
   TEXT_LINE_CONTRACT : constant Schema_Contract :=
     (Identity => 16#4355_4249_5454_5801#,
      Version => 1, Sizing => Bounded_Size, Wire_Size => 16#FFFE#);

end CuBit.Protocols;
