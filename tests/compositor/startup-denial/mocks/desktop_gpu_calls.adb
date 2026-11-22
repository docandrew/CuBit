package body Desktop_GPU_Calls is
 procedure Bound(Milliseconds : Interfaces.Unsigned_64) is null;
 procedure Read(Timeouts, Label : out Interfaces.Unsigned_32) is
 begin Timeouts := 0; Label := 0; end Read;
end Desktop_GPU_Calls;
