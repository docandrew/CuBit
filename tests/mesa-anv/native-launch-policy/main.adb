with CuBit.Messages; use CuBit.Messages;

procedure Main is
begin
   -- Neither launch in this fixture has an admitted rendering session.
   -- Reaching even the first application instruction is a policy failure.
   debugPrint ("TEST: FAIL render-launch-policy child resumed" & ASCII.LF);
end Main;
