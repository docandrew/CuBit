--  Test fixture: distinct saved replies, identified by a word.
with Interfaces; use Interfaces;
package CuBit.Handles.Block_Fixture is
   function Reply_Of (Word : Unsigned_64) return Reply;
   function Word_Of (Item : Reply) return Unsigned_64;
end CuBit.Handles.Block_Fixture;
