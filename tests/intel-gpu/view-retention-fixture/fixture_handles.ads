with Interfaces; use Interfaces;
--  Hosted stand-in for CuBit.Handles: only the Reply handle the buffer views
--  grant to. A test builds a reply naming its client with Test_Reply;
--  Reply_Ready False models a saved reply the kernel no longer resolves.
package CuBit.Handles is
   type Reply is private;
   No_Reply : constant Reply;
   Reply_Ready : Boolean := True;
   function Valid (Item : Reply) return Boolean;
   function Test_Reply (Word : Unsigned_64) return Reply;
   function Word_Of (Item : Reply) return Unsigned_64;
private
   type Reply is record
      Word : Unsigned_64 := 0;
   end record;
   No_Reply : constant Reply := (Word => 0);
   function Valid (Item : Reply) return Boolean is (Reply_Ready and then Item.Word /= 0);
   function Test_Reply (Word : Unsigned_64) return Reply is ((Word => Word));
   function Word_Of (Item : Reply) return Unsigned_64 is (Item.Word);
end CuBit.Handles;
