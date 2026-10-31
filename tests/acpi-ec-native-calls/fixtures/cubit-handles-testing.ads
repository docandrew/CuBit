pragma Ada_2022;
package CuBit.Handles.Testing with Pure is
   function Reply_For_Test (Word : Interfaces.Unsigned_64) return Reply;
private
   function Reply_For_Test (Word : Interfaces.Unsigned_64) return Reply is
     ((Word => Word));
end CuBit.Handles.Testing;
