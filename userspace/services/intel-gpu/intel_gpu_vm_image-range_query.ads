generic
package Intel_GPU_VM_Image.Range_Query is
   type Result is (Idle, Scanning, Reusable, Not_Reusable, Stale);
   type Query is limited private;
   procedure Start (State : in out Query; Object : Image;
                    GPU, Bytes : Unsigned_64; Accepted : out Boolean);
   procedure Step (State : in out Query; Object : Image);
   procedure Cancel (State : in out Query);
   function Status (State : Query) return Result;
   -- Metadata-only dispatch hint: no backing, publication or owner authority.
   -- Step does at most32 descriptor/leaf checks and rechecks root/revision at
   -- every entry. Serialized owner must keep the image alive across turns.
   -- A terminal result may be restarted; an active scan may not be replaced.
private
   type Query is limited record
      Value : Result := Idle;
      Root, Epoch, First, Region : Unsigned_64 := 0;
      Pages, Cursor : Natural range 0 .. Capacity * 512 := 0;
      Depth : Natural range 0 .. 3 := 0;
      Table : Page_Number := 1;
      Candidate : Natural range 2 .. Capacity + 1 := 2;
   end record;
end Intel_GPU_VM_Image.Range_Query;
