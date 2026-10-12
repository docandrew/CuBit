with Interfaces;
package Desktop_GPU_Calls is
 procedure Bound(Milliseconds : Interfaces.Unsigned_64);
 procedure Read(Timeouts, Label : out Interfaces.Unsigned_32);
end Desktop_GPU_Calls;
