------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Host tests for CuBit.Channel_Contracts: valid contracts round-trip
--  through their words; invalid ones, and words with any bit changed, are
--  refused or decode to the contract those words encode.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Channel_Contracts; use CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   Text : constant Contract :=
     (Element => TEXT_LINE_CONTRACT, Kind => Queue, Policy => Drop_Oldest,
      Pages => 4, Buffers => 1, Rule => Copy_Then_Validate);
   Numbers : constant Contract :=
     (Element => INTEGER_64_CONTRACT, Kind => Queue, Policy => Lossless,
      Pages => 1, Buffers => 1, Rule => In_Place);
   Blocks : constant Contract :=
     (Element => (Identity => 16#424C_4F43_4B00_0001#, Version => 1,
                  Sizing => Fixed_Size, Wire_Size => 4_096),
      Kind => Arena, Policy => Lossless, Pages => 1, Buffers => 64, Rule => In_Place);

   procedure Round_Trip (Item : Contract; Name : String) is
      Result : Contract;
      Accepted : Boolean;
      W : Words;
   begin
      Check (Valid (Item), Name & " is valid");
      W := Encode (Item);
      Decode (W, Result, Accepted);
      Check (Accepted and then Result = Item, Name & " round-trips");
      --  Flip each bit: refused, or a different valid contract that
      --  encodes back to exactly those words.
      for Word in W'Range loop
         for Bit in 0 .. 63 loop
            declare
               Changed : Words := W;
            begin
               Changed (Word) := Changed (Word) xor Shift_Left (1, Bit);
               Decode (Changed, Result, Accepted);
               Check (not Accepted
                      or else (Valid (Result) and then Encode (Result) = Changed
                               and then Result /= Item),
                      Name & " with a changed bit");
            end;
         end loop;
      end loop;
   end Round_Trip;

   Pair : constant Contract :=
     (Element => (Identity => 16#4653_5155_4555_0001#, Version => 1,
                  Sizing => Fixed_Size, Wire_Size => 64),
      Kind => Duplex, Policy => Lossless, Pages => 3, Buffers => 1, Rule => Copy_Then_Validate);

   Result : Contract;
   Accepted : Boolean;
begin
   Round_Trip (Pair, "a duplex queue pair");
   Check (Valid ((Pair with delta Buffers => 18)), "a duplex acceptor's region has its own size");
   Check (Valid ((Blocks with delta Buffers => 4_095)), "an arena filling one grant");
   Check (not Valid ((Blocks with delta Buffers => 4_096)),
          "an arena beyond one grant (with its control page)");
   Check (not Valid ((Blocks with delta Pages => 128, Buffers => 32)),
          "large buffers beyond one grant");
   Check (not Valid ((Pair with delta Buffers => 129)), "a duplex region is at most 128 pages");
   Check (not Valid ((Pair with delta Policy => Drop_Oldest)), "a duplex channel is lossless");
   Round_Trip (Text, "a text outlet");
   Round_Trip (Numbers, "a lossless number queue");
   Round_Trip (Blocks, "a block arena");
   Check (not Valid ((Text with delta Policy => Drop_Oldest, Kind => Arena)),
          "an arena must be lossless");
   Check (not Valid ((Numbers with delta Pages => 3)), "a queue's ring is a power of two");
   Check (not Valid ((Text with delta Element => NO_SCHEMA_CONTRACT)), "a schema is required");
   Check (not Valid ((Blocks with delta Kind => Queue)),
          "a queue's element is at most half its ring");
   --  Region sizes (CuBit.Channel_Protocol): what each side grants and maps.
   Check (CuBit.Channel_Protocol.Region_Pages (Numbers) = 2, "a one-page queue's region");
   Check (CuBit.Channel_Protocol.Region_Pages ((Blocks with delta Buffers => 4_095)) = 4_096,
          "an arena filling one grant");
   Check (CuBit.Channel_Protocol.Region_Pages (Pair) = 3
          and then CuBit.Channel_Protocol.Acceptor_Region_Pages ((Pair with delta Buffers => 18)) = 18,
          "a duplex channel's two regions");
   Decode ([others => 0], Result, Accepted);
   Check (not Accepted, "zero words are refused");
   Decode ([0 => 1, 1 => 1, 2 => Unsigned_64'Last], Result, Accepted);
   Check (not Accepted, "out-of-range fields are refused");
   if Failures = 0 then
      Put_Line ("channel-contracts: PASS");
   else
      Put_Line ("channel-contracts: FAIL (" & Failures'Image & " )");
   end if;
end Main;
