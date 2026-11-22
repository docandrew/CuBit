pragma Ada_2022;
--  Immutable client publication, independent from the legacy mutable attach
--  protocol. A successful publish does NOT return write ownership. Only an
--  authenticated successful retirement reply permits reuse of that ticket.
package CuBit.Desktop_Protocol.Publication with SPARK_Mode, Pure is
   subtype Failure_Status is Status_Code range Denied .. Resources_Exhausted;
   Configuration_Label : constant Unsigned_32 := 16#0860#;
   Stage_Label         : constant Unsigned_32 := 16#0861#;
   Publish_Label       : constant Unsigned_32 := 16#0862#;
   Retirement_Label    : constant Unsigned_32 := 16#0863#;
   --  Match the nonwrapping compositor policy; never narrow arbitrary words.
   subtype Identity is Unsigned_64 range 1 .. 2 ** 31 - 1;
   subtype Scale_Component is Positive range 1 .. 16;
   type Configuration is record
      Epoch : Identity := 1;
      Width, Height : Positive_Extent := 1;
      Numerator, Denominator : Scale_Component := 1;
      Layout : Buffer_Layout;
   end record;
   function Valid (Item : Configuration) return Boolean is
     (Valid_Layout (Item.Layout) and then Item.Layout.Pitch mod 4 = 0 and then
      Natural (Item.Layout.Width) =
        (Natural (Item.Width) * Item.Numerator + Item.Denominator - 1) /
          Item.Denominator and then
      Natural (Item.Layout.Height) =
        (Natural (Item.Height) * Item.Numerator + Item.Denominator - 1) /
          Item.Denominator);
   --  Keep the validated predicate abstract at callers; no assumed fact.
   pragma Annotate
     (GNATprove, Hide_Info, "Expression_Function_Body", Valid);
   type Configuration_Result
     (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success => Value : Configuration;
         when others => null;
      end case;
   end record;
   --  Transparent field models expose exact semantics to encoder proofs.
   function Configuration_Fields (Wire : Wire_Message)
      return Configuration_Result
     with Annotate => (GNATprove, Inline_For_Proof);
   function Decode_Configuration (Wire : Wire_Message)
      return Configuration_Result
     with Post => Decode_Configuration'Result = Configuration_Fields (Wire)
       and then (if Decode_Configuration'Result.Status = Success then
         Valid (Decode_Configuration'Result.Value));
   function Encode_Configuration (Item : Configuration_Result)
      return Wire_Message
     with Pre => (if Item.Status = Success then Valid (Item.Value)),
          Post => Decode_Configuration (Encode_Configuration'Result) = Item;

   type Query is record
      Surface : Live_Surface_Name := 1;
      --  Zero only for configuration query; positive for retirement query.
      Ticket : Unsigned_64 := 0;
   end record;
   type Query_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Query;
         when False => null;
      end case;
   end record;
   function Valid_Query (Item : Query; Retirement : Boolean) return Boolean is
     (if Retirement then Item.Ticket in Identity else Item.Ticket = 0);
   function Decode_Query (Wire : Wire_Message; Retirement : Boolean)
      return Query_Decoding
     with Post => (if Decode_Query'Result.Valid then
       Valid_Query (Decode_Query'Result.Value, Retirement));
   function Encode_Query (Item : Query; Retirement : Boolean)
      return Wire_Message
     with Pre => Valid_Query (Item, Retirement),
          Post =>
            Decode_Query (Encode_Query'Result, Retirement) = (True, Item);

   type Stage_Request is record
      Surface : Live_Surface_Name := 1;
      Epoch : Identity := 1;
      Grant : CuBit.Grant_References.Reference;
   end record;
   type Stage_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Stage_Request;
         when False => null;
      end case;
   end record;
   function Decode_Stage (Wire : Wire_Message) return Stage_Decoding
     with Annotate => (GNATprove, Inline_For_Proof);
   function Encode_Stage (Item : Stage_Request) return Wire_Message
     with Post => Decode_Stage (Encode_Stage'Result) = (True, Item);

   type Receipt (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success => Epoch, Ticket : Identity;
         when others => null;
      end case;
   end record;
   --  Stage returns the new ticket. Publish echoes the accepted ticket.
   --  Retirement succeeds only after actual readers are gone and echoes that
   --  exact ticket/epoch. Bad_State means pending, never permission to reuse.
   subtype Receipt_Label is Unsigned_32 range Stage_Label .. Retirement_Label;
   function Decode_Receipt (Wire : Wire_Message; Expected : Receipt_Label)
      return Receipt
     with Annotate => (GNATprove, Inline_For_Proof);
   function Encode_Receipt (Item : Receipt; Label : Receipt_Label)
      return Wire_Message
     with Post => Decode_Receipt (Encode_Receipt'Result, Label) = Item;

   type Publish_Request is record
      Surface : Live_Surface_Name := 1;
      Epoch, Ticket : Identity := 1;
      --  Physical source pixels. All-zero means complete source changed.
      Area : Rectangle := (0, 0, 0, 0);
      --  Handled-input watermark frozen before drawing; zero means unknown.
      --  Correlation metadata, never permission to read/write/retire a buffer.
      Input_After : Unsigned_64 := 0;
   end record;
   type Publish_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Publish_Request;
         when False => null;
      end case;
   end record;
   function Publication_Fields (Wire : Wire_Message) return Publish_Decoding
     with Annotate => (GNATprove, Inline_For_Proof);
   function Decode_Publish (Wire : Wire_Message) return Publish_Decoding
     with Post => Decode_Publish'Result = Publication_Fields (Wire) and then
       (if Decode_Publish'Result.Valid then
          Valid_Damage (Decode_Publish'Result.Value.Area));
   function Encode_Publish (Item : Publish_Request) return Wire_Message
     with Pre => Valid_Damage (Item.Area),
          Post => Decode_Publish (Encode_Publish'Result) = (True, Item);
end CuBit.Desktop_Protocol.Publication;
