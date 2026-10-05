"""Native UI checks for separating empty tab handles from navigated pages."""

def check_shared_blanks(text, wait, expect, key, navigate, record):
    # The controlled preceding cycles leave two dedicated (one parked) views.
    # Nineteen empty logical tabs must not add nineteen engine views.
    wait('CUBITSHELL-TABS: logical=20 frontend_views=3')
    expect('CUBITSHELL-BROWSER: title CuBitBrowserA', lambda: navigate('/browser-a'))
    wait('CUBITSHELL-TABS: logical=20 frontend_views=4')
    blank = 'CUBITSHELL-BROWSER: tab state 19 url=about:blank'
    previous = text().count(blank)
    expect('CUBITSHELL-BROWSER: tab select 19', lambda: key('ctrl-shift-tab'))
    wait(blank, previous)
    # Reloading an untouched tab must not navigate the shared document or
    # replace the real page in tab 20. Then verify its dedicated URL again.
    expect('CUBITSHELL: reload', lambda: key('ctrl-r'))
    loaded = 'CUBITSHELL-BROWSER: tab state 20 url=http://10.0.2.2:18470/browser-a'
    previous = text().count(loaded)
    expect('CUBITSHELL-BROWSER: tab select 20', lambda: key('ctrl-tab'))
    wait(loaded, previous)
    record('shared-blank-isolation-pass', logical_tabs=20, frontend_views_before=3,
           frontend_views_after_navigation=4)
