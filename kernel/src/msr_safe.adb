package body Msr_Safe is

    Recovered : constant Unsigned_64 := 0;

    -- Unassigned on Intel and AMD, and outside KVM's hypervisor range.
    Absent_Register : constant Unsigned_32 := 16#4000_FFF0#;

    function Write (Register : Unsigned_32; Value : Unsigned_64) return Boolean is
      (cubitWrmsrSafe (Unsigned_64 (Register), Value) = Recovered);

    procedure Read (Register : Unsigned_32; Value : out Unsigned_64; Ok : out Boolean) is
        Result : aliased Unsigned_64 := 0;
    begin
        Ok := cubitRdmsrSafe (Unsigned_64 (Register), Result'Address) = Recovered;
        Value := (if Ok then Result else 0);
    end Read;

    function Absent_Register_Recovered return Boolean is
        Value : Unsigned_64;
        Ok : Boolean;
    begin
        Read (Absent_Register, Value, Ok);
        return not Ok;
    end Absent_Register_Recovered;

end Msr_Safe;
