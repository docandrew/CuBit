"""Make the existing corpus driver's clock unavailable for a negative control."""


def without_clock(source):
    assert source.count('procedure ASLTS_Corpus_Runner is') == 1
    assert source.count('end ASLTS_Corpus_Runner;') == 1
    source = source.replace('procedure ASLTS_Corpus_Runner is', 'procedure Module_No_Clock_Runner is')
    source = source.replace('end ASLTS_Corpus_Runner;', 'end Module_No_Clock_Runner;')
    first = source.index('   procedure Clock (')
    last = source.index('   end Clock;', first) + len('   end Clock;')
    source = source[:first] + source[last:]
    actual = 'Perform_Delay => Delay_AML, Read_Microseconds => Clock,'
    assert source.count(actual) == 1
    return source.replace(actual, 'Perform_Delay => Delay_AML,')
