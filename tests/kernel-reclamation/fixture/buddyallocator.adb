with Interfaces;
package body BuddyAllocator is
    type Chunk is array (0 .. 65_535) of Interfaces.Unsigned_8
      with Alignment => 4096;
    type Arena_Type is array (1 .. 32) of Chunk;
    Arena : Arena_Type := (others => (others => 0));
    Next : Positive := 1;
    function blockSize (O : Order) return Storage_Count is
      (4096 * 2 ** Natural (O));
    function getOrder (Bytes : Storage_Count) return Order is
    begin
        for O in Order loop
            if blockSize (O) >= Bytes then
                return O;
            end if;
        end loop;
        raise Program_Error;
    end getOrder;
    procedure alloc (O : Order; Address : out System.Address) is
    begin
        pragma Assert (blockSize (O) <= Chunk'Size / 8 and Next <= Arena'Last);
        Address := Arena (Next)'Address;
        Next := Next + 1;
    end alloc;
    procedure free (O : Order; Address : System.Address) is
    begin
        null; -- arena remains valid for the entire fixture run
    end free;
end BuddyAllocator;
