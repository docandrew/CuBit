"""Native source-retirement delay injection and evidence oracle.

The real software renderer retires first; this withholds its evidence for three
observations while real CuBit acquisitions remain held. No GPU timing claim.
"""
import re


def instrument(source):
    s = source
    def rep(old, new):
        nonlocal s
        if s.count(old) != 1:
            raise ValueError(f"source-retirement fixture anchor changed: {old!r}")
        s = s.replace(old, new)
    rep('   sourceLoans : Source_Loan_Table;', '''   sourceLoans : Source_Loan_Table;
       sourceProbePolls : array (Source_Loans.Slot) of Natural := (others => 0);
       sourceProbeBusy, sourceProbeReturned, sourceProbeClosed : Natural := 0;''')
    rep('      sourceLoans (Source_Loans.Index (Loan)) := (Grant, Address);', '''      sourceLoans (Source_Loans.Index (Loan)) := (Grant, Address);
          sourceProbePolls (Source_Loans.Index (Loan)) := 0;''')
    rep('         Source_Loans.Observe_Renderer\n', '''         -- The real synchronous renderer has retired. Artificially withhold
             -- that evidence to exercise the actual retained native grant path.
             if Renderer = Desktop_Compositor.Source_Retired and then
               sourceProbePolls (Source_Loans.Index (Loan)) < 3
             then
                sourceProbePolls (Source_Loans.Index (Loan)) :=
                  sourceProbePolls (Source_Loans.Index (Loan)) + 1;
                sourceProbeBusy := sourceProbeBusy + 1;
                Renderer := Desktop_Compositor.Source_Busy;
             end if;
             Source_Loans.Observe_Renderer
    ''')
    rep('   use type Source_Loans.Phase;','   use type Source_Loans.Phase;\n   use type Desktop_Compositor.Source_Release;')
    rep('         MG.Return_Acquisition (sourceLoans (Source_Loans.Index (Loan)).Grant, Confirmed);', '''         if sourceProbePolls (Source_Loans.Index (Loan)) /= 3 then
                debugPrint ("SOURCE-NATIVE: FAIL early grant return" & LF);
                exitCompositor (1);
             end if;
             MG.Return_Acquisition (sourceLoans (Source_Loans.Index (Loan)).Grant, Confirmed);''')
    rep('            sourceLoans (Source_Loans.Index (Loan)) := (others => <>);', '''            sourceLoans (Source_Loans.Index (Loan)) := (others => <>);
                sourceProbeReturned := sourceProbeReturned + 1;
                debugPrint ("SOURCE-NATIVE: returned=" & sourceProbeReturned'Image &
                  " busy=" & sourceProbeBusy'Image & " closed=" & sourceProbeClosed'Image & LF);''')
    rep('   end releaseSurfaceBuffer;', '''      if sourceRetirementPending then
             sourceProbeClosed := sourceProbeClosed + 1;
             debugPrint ("SOURCE-NATIVE: detached while pending=" & sourceProbeClosed'Image & LF);
          end if;
       end releaseSurfaceBuffer;''')
    rep('               activity := Wait_For_Activity_Until (Unsigned_64\'Last);', '''               if sourceRetirementPending then
                      debugPrint ("SOURCE-NATIVE: FAIL indefinite wait while pending" & LF);
                      exitCompositor (1);
                   end if;
                   activity := Wait_For_Activity_Until (Unsigned_64'Last);''')
    return s


def check(serial):
    if 'SOURCE-NATIVE: FAIL' in serial or 'source retirement uncertain' in serial:
        raise ValueError('source-retirement fixture failed')
    samples = [tuple(map(int, match)) for match in re.findall(
        r'SOURCE-NATIVE: returned=\s*(\d+) busy=\s*(\d+) closed=\s*(\d+)', serial)]
    detached = re.findall(r'SOURCE-NATIVE: detached while pending=\s*(\d+)', serial)
    if not samples or not detached or not any(c > 0 for _, _, c in samples):
        raise ValueError('missing detach while pending followed by confirmed return')
    for i, (returned, busy, closed) in enumerate(samples, 1):
        if returned != i or busy < 3 * returned:
            raise ValueError('duplicate/skipped return or insufficient busy observations')
        if i > 1 and (busy < samples[i-2][1] or closed < samples[i-2][2]):
            raise ValueError('retirement counters went backwards')
    if list(map(int, detached)) != list(range(1, len(detached)+1)):
        raise ValueError('duplicate/skipped detach evidence')


def self_test():
    good = ('SOURCE-NATIVE: returned= 1 busy= 3 closed= 0\n'
            'SOURCE-NATIVE: detached while pending= 1\n'
            'SOURCE-NATIVE: returned= 2 busy= 6 closed= 1\n')
    check(good)
    for bad in ['', good.replace('closed= 1', 'closed= 0'),
                good.replace('busy= 6', 'busy= 5'),
                good.replace('returned= 2', 'returned= 1'),
                good.replace('SOURCE-NATIVE: detached', 'missing: detached'),
                good+'SOURCE-NATIVE: FAIL early grant return\n',
                good+'desktop: source retirement uncertain\n']:
        try:
            check(bad)
        except ValueError:
            continue
        raise AssertionError('bad source-retirement trace accepted')
    print('PASS source-retirement oracle: one positive, seven negative controls')
