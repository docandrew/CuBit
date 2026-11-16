------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A private copy of memory a peer can still write (SEC-025).
--
--  @description
--  Granted, lent, ring, published and DMA pages can change while a program
--  reads them. Code that checks a word and then reads it again to use it
--  (an index, a length, a count) can be steered past its own check. The
--  rule: copy what will be decoded once, in order, into private memory, and
--  decode only the copy. Copy_At reads each element of the shared memory
--  exactly once through a volatile view. Copy_With takes the read as a
--  function, so a test can change an element between reads and check that
--  each is read once (tests/sec025).
--
--  Not SPARK: the reads are of memory outside the program's model.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
generic
   type Index is range <>;
   type Element is private;
   type Block is array (Index range <>) of Element;
package CuBit.Shared_Copies with SPARK_Mode => Off is
   generic
      with function Read (I : Index) return Element;
   procedure Copy_With (Into : out Block);

   --  Into'Length elements at Location, Into'First's first.
   procedure Copy_At (Location : System.Address; Into : out Block);
end CuBit.Shared_Copies;
