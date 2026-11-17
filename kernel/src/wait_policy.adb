pragma Ada_2022;
package body Wait_Policy with SPARK_Mode => On is

    function Decide (Sources, Deadline, B, C : Unsigned_64) return Form is
    begin
        if Sources = ABI.Messages_Sources then
            if Deadline = ABI.Forever then
                return Messages_Forever;
            elsif Deadline = ABI.Now then
                return Messages_Now;
            else
                return Messages_Until;
            end if;
        elsif Sources = ABI.Requests_Sources then
            return (if Deadline = ABI.Now then Requests_Now else Unsupported);
        elsif Sources = ABI.Events_Sources then
            return (if Deadline = ABI.Now then Events_Now else Unsupported);
        elsif Sources = ABI.Completions_Sources then
            if B = 0 then
                return Unsupported;
            elsif Deadline = ABI.Forever and then C <= B then
                return Completions_Wait;
            elsif Deadline = ABI.Now and then C = 0 then
                return Completions_Now;
            else
                return Unsupported;
            end if;
        elsif Sources = ABI.Ready_Sources then
            return Ready;
        elsif Sources = ABI.Futex_Sources then
            return Futex;
        elsif Sources = ABI.Offered_Sources then
            return (if Deadline = ABI.Now then Offered else Unsupported);
        elsif Sources = ABI.Timer_Sources then
            return (if Deadline = ABI.Forever then Timer else Unsupported);
        else
            return Unsupported;
        end if;
    end Decide;

end Wait_Policy;
