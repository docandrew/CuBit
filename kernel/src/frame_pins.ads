with Interfaces; use Interfaces;

-- Lifetime bookkeeping only. Allocation/ownership and synchronization are
-- established by the allocator adapter, not by possession of this state.
package Frame_Pins with SPARK_Mode => On is
    subtype Pin_Count is Unsigned_8 range 0 .. 127;
    type State is private;
    type Release_Action is (Keep_Frame, Reclaim_Frame);

    function Count (S : State) return Pin_Count;
    function Retiring (S : State) return Boolean;

    -- Preserve the existing one-byte per-frame metadata format.
    function Decode (Raw : Unsigned_8) return State
      with Global => null,
           Post => Count (Decode'Result) = (Raw and 127) and
                   Retiring (Decode'Result) = (Raw >= 128);
    function Encode (S : State) return Unsigned_8
      with Global => null, Post => Decode (Encode'Result) = S;

    procedure Pin (S : in out State; Success : out Boolean)
      with Global => null,
           Post =>
             (if Retiring (S'Old) or Count (S'Old) = Pin_Count'Last then
                 not Success and S = S'Old
              else Success and Count (S) = Count (S'Old) + 1 and
                   not Retiring (S));

    procedure Request_Free (S : in out State; Action : out Release_Action)
      with Global => null,
           Post => Count (S) = Count (S'Old) and
             (if Count (S'Old) > 0 then
                 Action = Keep_Frame and Retiring (S)
              else Action = Reclaim_Frame and not Retiring (S));

    procedure Unpin (S : in out State; Success : out Boolean;
                     Action : out Release_Action)
      with Global => null,
           Post =>
             (if Count (S'Old) = 0 then
                 not Success and S = S'Old and Action = Keep_Frame
              else Success and Count (S) = Count (S'Old) - 1 and
                (if Count (S'Old) = 1 and Retiring (S'Old) then
                    Action = Reclaim_Frame and not Retiring (S)
                 else Action = Keep_Frame and
                      Retiring (S) = Retiring (S'Old)));

    -- Releasing ownership cannot reclaim a pinned frame or admit new pins.
    procedure Prove_Pinned_Free (Original : State)
      with Ghost, Global => null, Pre => Count (Original) > 0;

private
    type State is record
        Pins : Pin_Count := 0;
        Pending_Free : Boolean := False;
    end record;
    function Count (S : State) return Pin_Count is (S.Pins);
    function Retiring (S : State) return Boolean is (S.Pending_Free);
end Frame_Pins;
