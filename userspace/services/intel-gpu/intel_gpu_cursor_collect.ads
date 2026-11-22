with Interfaces;
with Intel_GPU_Cursor_Decode;
generic
   -- Bounded, nonraising and nonreentrant callbacks. Begin retains display
   -- power and excludes reconfiguration until End. Failed Begin owns nothing.
   with procedure Begin_Access (Success : out Boolean);
   with procedure End_Access (Success : out Boolean);
   -- 0..3: CURCNTR, CURBASE, CURSURFLIVE, CUR_FBC_CTL for an admitted pipe.
   with procedure Read_Field
     (Index : Natural; Value : out Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_Cursor_Collect is
   type Outcome is (Access_Unavailable, Read_Failed, Access_End_Failed, Collected);
   type Observation is record
      State : Outcome := Access_Unavailable;
      Reads : Natural range 0 .. 8 := 0;
      Before, After : Intel_GPU_Cursor_Decode.Sample;
      Decoded : Intel_GPU_Cursor_Decode.Decoded;
   end record;
   -- Collected means both samples and End succeeded, not that the decoded
   -- extent is valid. Neither result establishes GPU address ownership.
   procedure Inspect (Table_Bytes : Interfaces.Unsigned_64; Result : out Observation);
end Intel_GPU_Cursor_Collect;
